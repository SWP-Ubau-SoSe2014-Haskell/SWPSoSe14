@stack = global [1000 x i8*] undef ; stack containing pointers to i8
@sp = global i64 0 ; global stack pointer (or rather: current number of elements)
@lookahead = global i32 -1  ; current lookahead for input from stdin.
                            ; -1 means no lookahead done yet.


; Constants
@to_str  = private unnamed_addr constant [3 x i8] c"%i\00"
@true = global [2 x i8] c"1\00"
@false = global [2 x i8] c"0\00"
@write_mode = global [2 x i8] c"w\00"
@printf_str_fmt = private unnamed_addr constant [3 x i8] c"%s\00"
@crash_cust_str_fmt = private unnamed_addr constant [24 x i8] c"Crash: Custom error: %s\00"
@err_stack_underflow = private unnamed_addr constant [18 x i8] c"Stack underflow!\0A\00"
@err_eof = private unnamed_addr constant [9 x i8] c"At EOF!\0A\00"
@err_type = private unnamed_addr constant [14 x i8] c"Invalid type!\00"
@err_zero = private unnamed_addr constant [18 x i8] c"Division by zero!\00"


; External declarations
%FILE = type opaque

@stderr = global %FILE* undef

declare signext i32 @atol(i8*)
declare i64 @strtol(i8*, i8**, i32 )
declare signext i32 @snprintf(i8*, ...)
declare signext i32 @printf(i8*, ...)
declare %FILE* @fdopen(i32, i8*)
declare signext i32 @fprintf(%FILE*, i8*, ...)
declare float @strtof(i8*, i8**)
declare signext i32 @getchar()
declare i8* @malloc(i16 zeroext) ; void *malloc(size_t) and size_t is 16 bits long (SIZE_MAX)
declare i8* @calloc(i16 zeroext, i16 zeroext)
declare void @exit(i32 signext)


; Debugging stuff
@pushing = private unnamed_addr constant [14 x i8] c"Pushing [%s]\0A\00"
@popped  = private unnamed_addr constant [13 x i8] c"Popped [%s]\0a\00"
@msg = private unnamed_addr constant [5 x i8] c"msg\0a\00"


@int_to_str  = private unnamed_addr constant [3 x i8] c"%i\00"
@float_to_str  = private unnamed_addr constant [3 x i8] c"%f\00"

;typedef enum {INT = 1, FLOAT = 2, STRING = 3} elem_type;
;struct stack_elem {
;    elem_type type;
;    union {
;        int ival;
;        float fval;
;        char *sval;
;    };
;};
%struct.stack_elem = type { i32, %union.anon }
%union.anon = type { i8* }


@.str = private unnamed_addr constant [33 x i8] c"call int add with a=%i and b=%i\0A\00", align 1
@.str1 = private unnamed_addr constant [35 x i8] c"call float add with a=%f and b=%f\0A\00", align 1
@.str2 = private unnamed_addr constant [15 x i8] c"failed to add\0A\00", align 1



; Function definitions

; Get number of element on the stack
define i64 @stack_get_size() {
  %sp = load i64* @sp
  ret i64 %sp
}

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

; Pop stack and print result string
define void @print() {
  ; TODO: Check if the top stack element is a string and crash if it is not.
  call void @underflow_assert()

  %fmt = getelementptr [3 x i8]* @printf_str_fmt, i8 0, i8 0
  %val = call i8* @pop()
  call i32(i8*, ...)* @printf(i8* %fmt, i8* %val)

  ret void
}

; Pop stack, print result string to stderr and exit the program.
define void @crash(i1 %is_custom_error) {
  ; TODO: Check if the top stack element is a string and crash if it is not.
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
  %val = call i8* @pop()
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
  call void @push(i8* %at_eof)
  call void @crash(i1 0)
  ret void

push:
  %byte = trunc i32 %read to i8
  %buffer_addr = call i8* @calloc(i16 1, i16 2)
  store i8 %byte, i8* %buffer_addr
  call void @push(i8* %buffer_addr)

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
  call void @push(i8* %true)
  ret void

not_at_eof:
  %false = getelementptr [2 x i8]* @false, i8 0, i8 0
  call void @push(i8* %false)

  ret void
}

define void @push(i8* %str_ptr) {
  ; dereferencing @sp by loading value into memory
  %sp   = load i64* @sp

  ; get position on the stack, the stack pointer points to. this is the top of
  ; the stack.
  ; nice getelementptr FAQ: http://llvm.org/docs/GetElementPtr.html
  ;                     value of pointer type,  index,    field
  %top = getelementptr [1000 x i8*]* @stack,   i8 0,     i64 %sp

  ; the contents of memory are updated to contain %str_ptr at the location
  ; specified by the %addr operand
  store i8* %str_ptr, i8** %top

  ; increase stack pointer to point to new free, top of stack
  %newsp = add i64 %sp, 1
  store i64 %newsp, i64* @sp

  ret void
}

; pops element from stack and converts in integer
; returns the element, in case of error undefined
define i64 @pop_int(){
  ; pop
  %top = call i8* @pop()

  ; convert to int, check for error
  %top_int0 = call i32 @atol(i8* %top)
  %top_int1 = sext i32 %top_int0 to i64

  ; return
  ret i64 %top_int1
}

define void @push_float(double %top_float)
{
  ; allocate memory to store string in
  ; TODO: Make sure this is free()'d at _some_ point during
  ;       program execution.
  %buffer_addr = call i8* @malloc(i16 128)
  %to_str_ptr = getelementptr [3 x i8]* @float_to_str, i64 0, i64 0

  ; convert to string
  call i32(i8*, ...)* @snprintf(
          i8* %buffer_addr, i16 128, i8* %to_str_ptr, double %top_float)

  ; push on stack
  call void(i8*)* @push(i8* %buffer_addr)

  ret void
}

define void @push_int(i64 %top_int)
{
  ; allocate memory to store string in
  ; TODO: Make sure this is free()'d at _some_ point during
  ;       program execution.
  %buffer_addr = call i8* @malloc(i16 128)
  %to_str_ptr = getelementptr [3 x i8]* @int_to_str, i64 0, i64 0

  ; convert to string
  call i32(i8*, ...)* @snprintf(
          i8* %buffer_addr, i16 128, i8* %to_str_ptr, i64 %top_int)

  ; push on stack
  call void(i8*)* @push(i8* %buffer_addr)

  ret void
}

define i32 @mult() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer multiplication
;##############################################################################

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %add_int, label %type_check_a_float

add_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i64*
  %ival_a = load i64* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i64*
  %ival_b = load i64* %ival_b_cast, align 4

  ; add the two integers and store result on the stack
  %ires = mul i64 %ival_a, %ival_b
  call void(i64)* @push_int(i64 %ires)
  br label %exit_with_success

;##############################################################################
;                        floating point multiplication
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %mult_float, label %exit_with_invalid_type

mult_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4
  %fval_b_d = fpext float %fval_b to double

  ; sub the two floats and store result on the stack
  %fres= fmul double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}

define i32 @rem() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer remainder
;##############################################################################

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %rem_int, label %type_check_a_float

rem_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; add the two integers and store result on the stack
  %ires = srem i32 %ival_a, %ival_b
  %lres = sext i32 %ires to i64
  call void(i64)* @push_int(i64 %lres)
  br label %exit_with_success

;##############################################################################
;                        floating point remainder
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %rem_float, label %exit_with_invalid_type

rem_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4
  %fval_b_d = fpext float %fval_b to double

  ; sub the two floats and store result on the stack
  %fres= frem double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}

define i32 @sub() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer subtraction
;##############################################################################

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %sub_int, label %type_check_a_float

sub_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i64*
  %ival_a = load i64* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i64*
  %ival_b = load i64* %ival_b_cast, align 4

  ; add the two integers and store result on the stack
  %ires = sub i64 %ival_a, %ival_b
  call void(i64)* @push_int(i64 %ires)
  br label %exit_with_success

;##############################################################################
;                        floating point subtraction
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %sub_float, label %exit_with_invalid_type

sub_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4
  %fval_b_d = fpext float %fval_b to double

  ; sub the two floats and store result on the stack
  %fres= fsub double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}

define i32 @add() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer addition
;##############################################################################

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4 
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %add_int, label %type_check_a_float

add_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i64*
  %ival_a = load i64* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i64*
  %ival_b = load i64* %ival_b_cast, align 4

  ; add the two integers and store result on the stack
  %ires = add i64 %ival_a, %ival_b
  call void(i64)* @push_int(i64 %ires)
  br label %exit_with_success

;##############################################################################
;                        floating point addition
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %add_float, label %exit_with_invalid_type

add_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4
  %fval_b_d = fpext float %fval_b to double

  ; add the two floats and store result on the stack
  %fres= fadd double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}

define void @sub_int() {
  ; get top of stack
  %top_1   = call i64()* @pop_int()

  ; get second top of stack
  %top_2   = call i64()* @pop_int()

  ; sub the two values
  %res = sub i64 %top_1, %top_2

  ; store result on stack
  call void(i64)* @push_int(i64 %res)

  ret void
}

define i32 @div() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert() 
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert() 
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer division
;##############################################################################

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %div_int, label %type_check_a_float

div_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; prevent division by zero
  %div_by_zero = icmp eq i32 %ival_b, 0
  br i1 %div_by_zero, label %exit_with_zero, label %div_int_ok

div_int_ok:
  ; divide the two integers and store result on the stack
  %ires = sdiv i32 %ival_a, %ival_b
  %lres = sext i32 %ires to i64

  call void(i64)* @push_int(i64 %lres)
  br label %exit_with_success

;##############################################################################
;                        floating point division
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %div_float, label %exit_with_invalid_type

div_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4

  ; prevent division by zero
  %div_by_zero_f = fcmp oeq float %fval_b, 0.0
  br i1 %div_by_zero_f, label %exit_with_zero, label %div_float_ok

div_float_ok:
  ; divide the two floats and store result on the stack
  %fval_b_d = fpext float %fval_b to double
  %fres= fdiv double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_zero: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [18 x i8]* @err_zero, i64 0, i64 0))
  br label %exit_with_failure

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}



@main.number_a = private unnamed_addr constant [4 x i8] c"-57\00"
@main.number_b  = private unnamed_addr constant [4 x i8] c"-58\00"

define i32 @main_div() {
  ; push two numbers on the stack
  %number0 = getelementptr [4 x i8]* @main.number_a, i64 0, i64 0   
  %number1 = getelementptr [4 x i8]* @main.number_b, i64 0, i64 0   

  call void(i8*)* @push(i8* %number0)
  call void(i8*)* @push(i8* %number1)

  call i32 @div()
  %result = call i8* @pop()
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]*
              @popped, i32 0, i32 0), i8* %result)

  ret i32 0
}

define i32 @main_equal() {
  ; push two numbers on the stack
  %number0 = getelementptr [4 x i8]* @main.number_a, i64 0, i64 0   
  %number1 = getelementptr [4 x i8]* @main.number_b, i64 0, i64 0   

  call void(i8*)* @push(i8* %number0)
  call void(i8*)* @push(i8* %number1)

  call i32 @equal()
  %result = call i8* @pop()
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]*
              @popped, i32 0, i32 0), i8* %result)

  ret i32 0
}


define i8* @peek() {
  %sp   = load i64* @sp
  %top_of_stack = sub i64 %sp, 1
  %addr = getelementptr [1000 x i8*]* @stack, i8 0, i64 %top_of_stack
  %val = load i8** %addr
  ret i8* %val
}

define i8* @pop() {
  %val = call i8*()* @peek()
  %sp = load i64* @sp
  %top_of_stack = sub i64 %sp, 1
  store i64 %top_of_stack, i64* @sp
  ret i8* %val
}

define i32 @finish(){
  ret i32 0
}

;##############################################################################
;                                 equal
;##############################################################################

define i32 @equal(){
  ; return value of this function
  %func_result = alloca i32, align 4

  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8
 
  ; get top
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get top-1
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %cmp_int, label %type_check_a_float

cmp_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; the actual comparison
  %equal_int = icmp eq i32 %ival_a, %ival_b 
  br i1 %equal_int, label %exit_with_true, label %exit_with_false

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %cmp_float, label %exit_with_invalid_type

cmp_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4

  ; prevent division by zero
  %equal_float = fcmp oeq float %fval_a, %fval_b
  br i1 %equal_float, label %exit_with_true, label %exit_with_false


exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_true: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [2 x i8]* @true, i64 0, i64 0))
  br label %exit_with_success

exit_with_false: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [2 x i8]* @false, i64 0, i64 0))
  br label %exit_with_success

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result

}

;##############################################################################
;                                 greater
;##############################################################################

define i32 @greater(){
  ; return value of this function
  %func_result = alloca i32, align 4

  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8
 
  ; get top
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get top-1
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %cmp_int, label %type_check_a_float

cmp_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; the actual comparison
  %greater_int = icmp sgt i32 %ival_a, %ival_b 
  br i1 %greater_int, label %exit_with_true, label %exit_with_false

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %cmp_float, label %exit_with_invalid_type

cmp_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4

  ; prevent division by zero
  %greater_float = fcmp ogt float %fval_a, %fval_b
  br i1 %greater_float, label %exit_with_true, label %exit_with_false


exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_true: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [2 x i8]* @true, i64 0, i64 0))
  br label %exit_with_success

exit_with_false: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [2 x i8]* @false, i64 0, i64 0))
  br label %exit_with_success

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result

}


; Popping a pointer from the stack into a variable
define void @pop_into(i8** %var_ptr) {
  call void @underflow_assert()
  %val_ptr = call i8* @pop()
  store i8* %val_ptr, i8** %var_ptr
  ret void
}

; Pushing a pointer from a variable onto the stack
define void @push_from(i8** %var_ptr) {
  %val = load i8** %var_ptr
  call void @push (i8* %val)
  ret void
}

; Function Attrs: nounwind uwtable
; Takes a string, determines the type it is representing and returns the
; corresponding stack element structure.
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

@number2 = private unnamed_addr constant [2 x i8] c"5\00"
@number3 = private unnamed_addr constant [2 x i8] c"2\00"

define i32 @main_() {
 %pushingptr = getelementptr [14 x i8]* @pushing, i64 0, i64 0
 %poppedptr = getelementptr [13 x i8]* @popped, i64 0, i64 0

 call void @eof_check()
 %i1 = call i8*()* @pop()
 call i32(i8*, ...)* @printf(i8* %poppedptr, i8* %i1)

 call void @input()
 %i0 = call i8*()* @pop()
 call i32(i8*, ...)* @printf(i8* %poppedptr, i8* %i0)

 call void @input()
 %i2 = call i8*()* @pop()
 call i32(i8*, ...)* @printf(i8* %poppedptr, i8* %i2)

 ; push two numbers on the stack
 %number2 = getelementptr [2 x i8]* @number2, i64 0, i64 0
 %number3 = getelementptr [2 x i8]* @number3, i64 0, i64 0

 call i32(i8*, ...)* @printf(i8* %pushingptr, i8* %number2)
 call void(i8*)* @push(i8* %number2)

 call i32(i8*, ...)* @printf(i8* %pushingptr, i8* %number3)
 call void(i8*)* @push(i8* %number3)

 call void @underflow_check()
 %size0 = call i8*()* @pop()
 call i32(i8*, ...)* @printf(i8* %poppedptr, i8* %size0)

 call void @sub_int()
 %sum  = call i8*()* @pop()
 call i32(i8*, ...)* @printf(i8* %poppedptr, i8* %sum)

 call void @underflow_check()
 %size1 = call i8*()* @pop()
 call i32(i8*, ...)* @printf(i8* %poppedptr, i8* %size1)

 ret i32 0
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
