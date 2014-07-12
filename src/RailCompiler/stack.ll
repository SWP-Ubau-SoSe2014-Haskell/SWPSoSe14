; Module      : LLVM backend - stack helper functions (and some misc. functions)
; Description : Contains helper functions for working with the stack and also
;               some functions which have not yet been split out into their own modules.
; Maintainers : Tilman Blumenbach, Sascha Zinke, Maximilian Claus, Tudor Soroceanu,
;               Philipp Borgers, Lyudmila Vaseva, Marcus Hoffmann, Michal Ajchman
; License     : MIT
;
; Beware: Many of these functions directly crash (in Rail terms: properly exit) the program.


; Types

; See linked_stack.ll for the definition.
%stack_element = type opaque

; Misnamed struct returned by get_stack_elem() (which, unfortunately,
; is also misnamed). This is not the data type used for real stack elements,
; but more like a container used to store conversion results (string to numerical
; value).
;
; C definition was as follows:
;
;  typedef enum {INT = 1, FLOAT = 2, STRING = 3} elem_type;
;  struct stack_elem {
;      elem_type type;
;      union {
;          int ival;
;          float fval;
;          char *sval;
;      };
;  };
%struct.stack_elem = type { i32, %union.anon }
%union.anon = type { i8* }

; struct for the symbol table
%struct.table = type {i8*, %stack_element*, %struct.table*}

; Global variables
@lookahead = global i32 -1            ; Current lookahead for input from stdin,
                                      ; -1 means no lookahead done yet.

; Constants
@err_numeric = unnamed_addr constant [56 x i8] c"Failed to check whether stack elem is of type numeric!\0A\00"
@crash_cust_str_fmt = private unnamed_addr constant [24 x i8] c"Crash: Custom error: %s\00"
@err_stack_underflow = private unnamed_addr constant [18 x i8] c"Stack underflow!\0A\00"
@err_zero = unnamed_addr constant [18 x i8] c"Division by zero!\00"
@printf_str_fmt = private unnamed_addr constant [3 x i8] c"%s\00"
@err_oom = unnamed_addr constant [15 x i8] c"Out of memory!\00"
@err_type = unnamed_addr constant [14 x i8] c"Invalid type!\00"
@to_str  = private unnamed_addr constant [3 x i8] c"%i\00"
@err_eof = unnamed_addr constant [9 x i8] c"At EOF!\0A\00"
@false = unnamed_addr constant [2 x i8] c"0\00"
@true = unnamed_addr constant [2 x i8] c"1\00"
@write_mode = global [2 x i8] c"w\00"
@type_string = external global [7 x i8]
@type_lambda = external global  [7 x i8]
@type_list = external global [5 x i8]
@type_nil = external global [4 x i8]

; External declarations
%FILE = type opaque

@stderr = global %FILE* undef

declare i64 @strtol(i8*, i8**, i32 )
declare signext i32 @printf(i8*, ...)
declare %FILE* @fdopen(i32, i8*)
declare signext i32 @fprintf(%FILE*, i8*, ...)
declare float @strtof(i8*, i8**)
declare signext i32 @getchar()
declare i8* @calloc(i16 zeroext, i16 zeroext)
declare i8* @strdup(i8*)
declare void @exit(i32 signext)

declare %stack_element* @pop_struct()
declare void @push_struct(%stack_element*)
declare i8* @stack_element_get_data(%stack_element* %element)
declare i64 @pop_int()
declare i8* @pop_string()
declare void @push_int(i64)
declare %stack_element* @push_string_cpy(i8*)
declare %stack_element* @push_string_ptr(i8*)
declare i32 @stack_element_get_refcount(%stack_element*)
declare i8 @stack_element_get_type(%stack_element*)
declare %stack_element* @stack_element_new(i8, i8*, %stack_element*)
declare i64 @stack_get_size()
declare void @stack_element_ref(%stack_element* %element)
declare void @stack_element_unref(%stack_element* %element)
declare i8* @malloc(i64)

; Debugging stuff
@pushing = unnamed_addr constant [14 x i8] c"Pushing [%s]\0A\00"
@popped  = unnamed_addr constant [13 x i8] c"Popped [%s]\0a\00"
@msg = unnamed_addr constant [5 x i8] c"msg\0a\00"
@no_element = private unnamed_addr constant [18 x i8] c"No such Element!\0A\00"

@int_to_str = unnamed_addr constant [3 x i8] c"%i\00"
@float_to_str = unnamed_addr constant [3 x i8] c"%f\00"

@.str = private unnamed_addr constant [33 x i8] c"call int add with a=%i and b=%i\0A\00", align 1
@.str1 = private unnamed_addr constant [35 x i8] c"call float add with a=%f and b=%f\0A\00", align 1
@.str2 = private unnamed_addr constant [15 x i8] c"failed to add\0A\00", align 1



; Function definitions

; Push the stack size onto the stack
define void @underflow_check() {
  %stack_size = call i64 @stack_get_size()
  call void @push_int(i64 %stack_size)
  ret void
}

; Exit the program if stack is empty (prints error to stderr).
define void @underflow_assert() {
  %stack_size = call i64 @stack_get_size()
  %stack_empty = icmp eq i64 %stack_size, 0
  br i1 %stack_empty, label %uas_crash, label %uas_okay

uas_crash:
  %err = getelementptr [18 x i8]* @err_stack_underflow, i8 0, i8 0
  %stderr = load %FILE** @stderr
  call i32(%FILE*, i8*, ...)* @fprintf(%FILE* %stderr, i8* %err)
  call void @exit(i32 1)

  ret void

uas_okay:
  ret void
}

; pushes 'string' or 'nil' or 'list' or 'lambda' on stack, depending on the type
; of the top stack element.
define void @type(){
  %element = call %stack_element* @pop_struct()
  %actual_type = call i8 @stack_element_get_type(%stack_element* %element)
  br label %check_string

check_string:
  %is_string = icmp eq i8 %actual_type, 0
  br i1 %is_string, label %return_string, label %check_list

return_string:
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                         [7 x i8]* @type_string, i64 0, i64 0))
  br label %exit

check_list:
  %is_list = icmp eq i8 %actual_type, 1
  br i1 %is_list, label %check_nil, label %check_lambda

check_nil:
  %dataPtr = call i8* @stack_element_get_data(%stack_element* %element)
  %is_nil = icmp eq i8* %dataPtr, null 
  br i1 %is_nil, label %return_nil, label %return_list

return_nil:
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                         [4 x i8]* @type_nil, i64 0, i64 0))
  br label %exit

return_list:
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                         [5 x i8]* @type_list, i64 0, i64 0))
  br label %exit

check_lambda:
  br label %exit

exit:
  call void @stack_element_unref(%stack_element* %element)

  ret void
}

; Pop stack and print result string
define void @print() {
  call void @underflow_assert()

  %fmt = getelementptr [3 x i8]* @printf_str_fmt, i8 0, i8 0
  %elem = call %stack_element*()* @pop_struct()
  %val = call i8*(%stack_element*)* @stack_element_get_data(%stack_element* %elem)
  call i32(i8*, ...)* @printf(i8* %fmt, i8* %val)
  call void(%stack_element*)* @stack_element_unref(%stack_element* %elem)

  ret void
}

; Pop stack, print result string to stderr and exit the program.
define void @crash(i1 %is_custom_error) {
  call void @underflow_assert()
  br i1 %is_custom_error, label %custom_error, label %raw_error

custom_error:
  %cust_fmt = getelementptr [24 x i8]* @crash_cust_str_fmt, i8 0, i8 0
  br label %end

raw_error:
  %raw_fmt = getelementptr [3 x i8]* @printf_str_fmt, i8 0, i8 0
  br label %end

end:
  %fmt = phi i8* [%raw_fmt, %raw_error], [%cust_fmt, %custom_error]
  %val = call i8* @pop_string()
  %stderr = load %FILE** @stderr
  call i32(%FILE*, i8*, ...)* @fprintf(%FILE* %stderr, i8* %fmt, i8* %val)
  ; Now, crash!
  call void @exit(i32 1)

  ret void
}

; Get a byte of input from stdin and push it.
; Crashes the program on errors.
define void @input() {
  %read = call i32 @input_get()
  %err = icmp slt i32 %read, 0
  br i1 %err, label %error, label %push

error:
  %at_eof = getelementptr [9 x i8]* @err_eof, i64 0, i64 0
  call %stack_element* @push_string_cpy(i8* %at_eof)
  call void @crash(i1 0)
  ret void

push:
  %byte = trunc i32 %read to i8
  %buffer_addr = call i8* @xcalloc(i16 1, i16 2)
  store i8 %byte, i8* %buffer_addr
  call %stack_element* @push_string_ptr(i8* %buffer_addr)

  ret void
}

; Get a byte of input from stdin. Returns < 0 on error.
; This can be used together with input_peek().
define i32 @input_get() {
  %lookahead = load i32* @lookahead
  %need_read = icmp slt i32 %lookahead, 0
  br i1 %need_read, label %ig_read, label %ig_lookahead

ig_lookahead:
  store i32 -1, i32* @lookahead
  ret i32 %lookahead

ig_read:
  %read = call i32 @getchar()
  ret i32 %read
}

; Peek a byte of input from stdin. Returns < 0 on error.
; Successive calls to this function without interspersed calls
; to input_read() return the same value.
define i32 @input_peek() {
  %read = call i32 @input_get()
  store i32 %read, i32* @lookahead
  ret i32 %read
}

; If stdin is at EOF, push 1, else 0.
define void @eof_check() {
  %peek = call i32 @input_peek()
  %is_eof = icmp slt i32 %peek, 0
  br i1 %is_eof, label %at_eof, label %not_at_eof

at_eof:
  %true = getelementptr [2 x i8]* @true, i8 0, i8 0
  call %stack_element* @push_string_cpy(i8* %true)
  ret void

not_at_eof:
  %false = getelementptr [2 x i8]* @false, i8 0, i8 0
  call %stack_element* @push_string_cpy(i8* %false)

  ret void
}


define i32 @finish(){
  ret i32 0
}

; Popping a pointer from the stack into a variable
define void @pop_into(%struct.table* %t, i8* %name){
  call void @underflow_assert()
  
  %n_ptr = getelementptr inbounds %struct.table* %t, i32 0, i32 0
  %name_t = load i8** %n_ptr, align 8
  %is_null = icmp eq i8* %name_t, null
  br i1 %is_null, label %insert, label %search
insert:
  ; store name
  store i8* %name, i8** %n_ptr, align 8
  
  ; pop value from stack and store value
  %value = call %stack_element*()* @pop_struct()
  call void @stack_element_ref(%stack_element* %value)
  %v_ptr = getelementptr inbounds %struct.table* %t, i32 0, i32 1
  store %stack_element* %value, %stack_element** %v_ptr, align 8
  
  ; create new element and append to table
  %new_elem_alloc = call i8* @malloc(i64 24)
  %new_elem = bitcast i8* %new_elem_alloc to %struct.table*
  ; initialise new element with null
  call void @initialise(%struct.table* %new_elem)

  %next_ptr = getelementptr inbounds %struct.table* %t, i32 0, i32 2
  store %struct.table* %new_elem, %struct.table** %next_ptr, align 8
  
  br label %end

search:
  %is_equal = icmp eq i8* %name_t, %name
  br i1 %is_equal, label %insert2, label %search_further

insert2:
  %value2 = call %stack_element*()* @pop_struct()
  call void @stack_element_ref(%stack_element* %value2)
  %v_ptr_found = getelementptr inbounds %struct.table* %t, i32 0, i32 1
  %old_value = load %stack_element** %v_ptr_found, align 8
  call void @stack_element_unref(%stack_element* %old_value)
  store %stack_element* %value2, %stack_element** %v_ptr_found, align 8
  
  br label %end

search_further:
  %next_ptr_recursive = getelementptr inbounds %struct.table* %t, i32 0, i32 2
  %next_ptr_recursive2 = load %struct.table** %next_ptr_recursive, align 8
  call void @pop_into(%struct.table* %next_ptr_recursive2, i8* %name) 
  br label %end

end:
  ret void
}

; Pushing a pointer from a variable onto the stack
define void @push_from(%struct.table* %t, i8* %name){
  %n_ptr = getelementptr inbounds %struct.table* %t, i64 0, i32 0
  %name_t = load i8** %n_ptr

  %is_null = icmp eq i8* %name_t, null
  br i1 %is_null, label %no_such_elem, label %search
no_such_elem:
  %no_elem = getelementptr [18 x i8]* @no_element, i64 0, i64 0
  call i32(i8*, ...)* @printf(i8* %no_elem)
  br label %end

search:
  %is_equal = icmp eq i8* %name_t, %name
  br i1 %is_equal, label %push_onto_stack, label %search_further

push_onto_stack:
  %v_ptr_found = getelementptr inbounds %struct.table* %t, i64 0, i32 1
  %value_to_push = load %stack_element** %v_ptr_found
  call void @stack_element_ref(%stack_element* %value_to_push)
  call void @push_struct(%stack_element* %value_to_push)

  br label %end

search_further:
  %next_ptr_recursive = getelementptr inbounds %struct.table* %t, i64 0, i32 2
  %next_ptr_recursive2 = load %struct.table** %next_ptr_recursive
  call void @push_from(%struct.table* %next_ptr_recursive2, i8* %name) 
  br label %end

end:
  ret void
}

; Copy Function
; Takes two symbol tables and copies the whole content from the first to the second
; This is especially usefull for the lambda funtion
define void @copy_symbol_table(%struct.table* %old, %struct.table* %new){
  ; get pointers to the name field and store in the new table
  %n_ptr_old = getelementptr inbounds %struct.table* %old, i64 0, i32 0
  %n_ptr_new = getelementptr inbounds %struct.table* %new, i64 0, i32 0
  %name = load i8** %n_ptr_old
  store i8* %name, i8** %n_ptr_new

  %is_null = icmp eq i8* %name, null
  br i1 %is_null, label %end, label %next

next:
  ; get pointers to the value field and copy it 
  %v_ptr_old = getelementptr inbounds %struct.table* %old, i64 0, i32 1
  %v_ptr_new = getelementptr inbounds %struct.table* %new, i64 0, i32 1
  %value = load %stack_element** %v_ptr_old
  store %stack_element* %value, %stack_element** %v_ptr_new
  
  ; initialise a new element and append it to the new table
  %new_elem_alloc = call i8* @malloc(i64 24)
  %new_elem = bitcast i8* %new_elem_alloc to %struct.table*
  call void @initialise(%struct.table* %new_elem)
  
  %next_ptr_old = getelementptr inbounds %struct.table* %old, i64 0, i32 2  
  %next_ptr_new = getelementptr inbounds %struct.table* %new, i64 0, i32 2  
  store %struct.table* %new_elem, %struct.table** %next_ptr_new
  
  ;recursive call of copy function
  %next_ptr_old2 = load %struct.table** %next_ptr_old, align 8
  %next_ptr_new2 = load %struct.table** %next_ptr_new, align 8
  call void @copy_symbol_table(%struct.table* %next_ptr_old2, %struct.table* %next_ptr_new2)
  br label %end
end:
  ret void
}

; initialise the symbol table with the first element = null
define void @initialise(%struct.table* %t){
  %1 = getelementptr inbounds %struct.table* %t, i32 0, i32 0
  store i8* null, i8** %1, align 8

  ret void
}

; Function Attrs: nounwind uwtable
; Takes a string, determines the type it is representing and returns the
; corresponding stack element structure. Not that this is NOT an actual
; stack element structure, but more like a container used to store the conversion
; results (string to numerical value).
define i32 @get_stack_elem(i8* %string, %struct.stack_elem* %elem) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8*, align 8
  %3 = alloca %struct.stack_elem*, align 8
  %pEnd = alloca i8*, align 8
  %new_long = alloca i64, align 8
  %new_float = alloca float, align 4
  store i8* %string, i8** %2, align 8
  store %struct.stack_elem* %elem, %struct.stack_elem** %3, align 8
  %4 = load i8** %2, align 8
  %5 = call i64 @strtol(i8* %4, i8** %pEnd, i32 10) #2
  store i64 %5, i64* %new_long, align 8
  %6 = load i8** %pEnd, align 8
  %7 = load i8* %6, align 1
  %8 = sext i8 %7 to i32
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %18

; <label>:10                                      ; preds = %0
  %11 = load %struct.stack_elem** %3, align 8
  %12 = getelementptr inbounds %struct.stack_elem* %11, i32 0, i32 0
  store i32 1, i32* %12, align 4
  %13 = load i64* %new_long, align 8
  %14 = trunc i64 %13 to i32
  %15 = load %struct.stack_elem** %3, align 8
  %16 = getelementptr inbounds %struct.stack_elem* %15, i32 0, i32 1
  %17 = bitcast %union.anon* %16 to i32*
  store i32 %14, i32* %17, align 4
  store i32 0, i32* %1
  br label %39

; <label>:18                                      ; preds = %0
  %19 = load i8** %2, align 8
  %20 = call float @strtof(i8* %19, i8** %pEnd) #2
  store float %20, float* %new_float, align 4
  %21 = load i8** %pEnd, align 8
  %22 = load i8* %21, align 1
  %23 = sext i8 %22 to i32
  %24 = icmp eq i32 %23, 0
  br i1 %24, label %25, label %32

; <label>:25                                      ; preds = %18
  %26 = load %struct.stack_elem** %3, align 8
  %27 = getelementptr inbounds %struct.stack_elem* %26, i32 0, i32 0
  store i32 2, i32* %27, align 4
  %28 = load float* %new_float, align 4
  %29 = load %struct.stack_elem** %3, align 8
  %30 = getelementptr inbounds %struct.stack_elem* %29, i32 0, i32 1
  %31 = bitcast %union.anon* %30 to float*
  store float %28, float* %31, align 4
  store i32 0, i32* %1
  br label %39

; <label>:32                                      ; preds = %18
  %33 = load %struct.stack_elem** %3, align 8
  %34 = getelementptr inbounds %struct.stack_elem* %33, i32 0, i32 0
  store i32 3, i32* %34, align 4
  %35 = load i8** %2, align 8
  %36 = load %struct.stack_elem** %3, align 8
  %37 = getelementptr inbounds %struct.stack_elem* %36, i32 0, i32 1
  %38 = bitcast %union.anon* %37 to i8**
  store i8* %35, i8** %38, align 8
  store i32 0, i32* %1
  br label %39

; <label>:39                                      ; preds = %32, %25, %10
  %40 = load i32* %1
  ret i32 %40
}

; "Fatal" version of calloc(3): crash()es the program on errors.
define i8* @xcalloc(i16 zeroext %nmemb, i16 zeroext %size) {
  %mem = call i8* @calloc(i16 %nmemb, i16 %size)
  %is_null = icmp eq i8* %mem, null
  br i1 %is_null, label %bail_out, label %okay

bail_out:
  ; Oopsie, out of memory. Try to bail out politely.
  %oom_str = getelementptr [15 x i8]* @err_oom, i32 0, i32 0
  call %stack_element* @push_string_cpy(i8* %oom_str)
  call void @crash(i1 0)

  ret i8* null

okay:
  ret i8* %mem
}

; "Fatal" version of strdup(3): crash()es the program on errors.
define i8* @xstrdup(i8* %str) {
  %mem = call i8* @strdup(i8* %str)
  %is_null = icmp eq i8* %mem, null
  br i1 %is_null, label %bail_out, label %okay

bail_out:
  ; Oopsie, out of memory. Try to bail out politely.
  %oom_str = getelementptr [15 x i8]* @err_oom, i32 0, i32 0
  call %stack_element* @push_string_cpy(i8* %oom_str)
  call void @crash(i1 0)

  ret i8* null

okay:
  ret i8* %mem
}

;##############################################################################
;                                  init
;##############################################################################
define void @start() {

  %write_mode = getelementptr [2 x i8]* @write_mode, i64 0, i64 0
  %stderr = call %FILE* @fdopen(i32 2, i8* %write_mode)
  store %FILE* %stderr, %FILE** @stderr

  ret void
}

; vim:sw=2 ts=2 et
