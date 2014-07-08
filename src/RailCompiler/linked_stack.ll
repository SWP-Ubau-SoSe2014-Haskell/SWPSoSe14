; Module      : LLVM backend - linked stack implementation/reference counting
; Description : Contains our linked stack implementation and its reference counting
;               routines.
; Maintainers : Tilman Blumenbach et al.
; License     : MIT
;
; These functions are used by our LLVM backend and most of them operate directly on
; the stack. Many also directly crash (in Rail terms: properly exit) the program.


; Types

; A "real" stack element that is stored on the stack.
; This also double as a list element since lists need to be
; able to store all the types that can be pushed onto the stack.
;
; The fields are:
;  * i8 dataType: Type of the data stored in dataPtr.
;    * 0 means string. dataPtr points to a null-terminated string.
;    * 1 means list. Note that there is no seperate type for empty lists,
;      those are represented with type == 1 and dataPtr == null.
;      For non-empty lists, dataPtr points to another stack_element which
;      is the head of the (linked) list.
;  * void *dataPtr: Points to type-specific data. May be null.
;  * i32 refCount: The element's reference count. When this reaches 0, the element
;    is free'd.
;  * stack_element *nextElementPtr: Points to the next stack_element. May be null if there
;    is no next element.
%stack_element = type { i8, i8*, i32, %stack_element* }


; Global variables
@stack = global %stack_element* null  ; Linked list of stack_element structs.
@stack_size = global i64 0            ; Current number of elements on the stack.


; Constants
@err_type_mismatch = private unnamed_addr constant [16 x i8] c"Type mismatch!\0A\00"
@err_not_bool = private unnamed_addr constant [29 x i8] c"Stack value was not 0 or 1!\0A\00"

; External declarations

; C standard library variables/functions
declare signext i32 @atol(i8*)
declare void @free(i8*)
declare i8* @malloc(i16 zeroext) ; void *malloc(size_t) and size_t is 16 bits long (SIZE_MAX)
declare signext i32 @snprintf(i8*, ...)
declare i8* @xcalloc(i16 zeroext, i16 zeroext)
declare i8* @xstrdup(i8*)

; Own external LLVM variables/functions
@float_to_str = external global [3 x i8]
@int_to_str = external global [3 x i8]

declare void @crash(i1)
declare void @underflow_assert()


; Function definitions

; Get number of element on the stack
define i64 @stack_get_size() {
  %sz = load i64* @stack_size
  ret i64 %sz
}

; Creates a new stack_element with a reference count of 1.
define %stack_element* @stack_element_new(i8 %dataType, i8* %dataPtr, %stack_element* %nextElementPtr) {
  ; How many bytes do we need to allocate for a single stack element struct?
  ; getelementptr abuse taken from:
  ; http://nondot.org/sabre/LLVMNotes/SizeOf-OffsetOf-VariableSizedStructs.txt
  %elem_size0 = getelementptr %stack_element* null, i32 1
  %elem_size1 = ptrtoint %stack_element* %elem_size0 to i16

  ; Now we can allocate the memory.
  %element0 = call i8* @xcalloc(i16 1, i16 %elem_size1)
  %element1 = bitcast i8* %element0 to %stack_element*

  ; %element1 now can be treated like an element struct. Yay!
  call void @stack_element_set_type(%stack_element* %element1, i8 %dataType)
  call void @stack_element_set_data(%stack_element* %element1, i8* %dataPtr)
  call void @stack_element_set_next(%stack_element* %element1, %stack_element* %nextElementPtr)

  ; Finally, increment the reference count so that it is exactly 1.
  call void @stack_element_ref(%stack_element* %element1)

  ; That's it!
  ret %stack_element* %element1
}

; Decrement refcount of stack element
; If new refcount is zero, free the stack element and it's data
define void @stack_element_unref(%stack_element* %element) {
  %refcount = call i32(%stack_element*)* @stack_element_get_refcount(%stack_element* %element)
  %refcount_1 = sub i32 %refcount, 1
  %cond = icmp eq i32 %refcount_1, 0
  br i1 %cond, label %free_data, label %update_refcount

free_data:
  %data = call i8* @stack_element_get_data(%stack_element* %element)
  call void @free(i8* %data)
  %mem = bitcast %stack_element* %element to i8*
  call void @free(i8* %mem)
  br label %finished
update_refcount:
  call void(%stack_element*, i32)* @stack_element_set_refcount(%stack_element* %element, i32 %refcount_1)
  br label %finished

finished:
  ret void
}

; free() a stack element and optionally, free the data it contains as well
; (i. e. the memory pointed to by the dataPtr member).
;
; Returns the dataPtr if %free_data == 1 and null otherwise.
;
; TODO: This should probably decrement the reference count and only do something
;       if it is 0 after decrementing.
define i8* @stack_element_free(%stack_element* %element, i1 %free_data) {
top:
  %data = call i8* @stack_element_get_data(%stack_element* %element)
  br i1 %free_data, label %do_free_data, label %free_stack_struct

do_free_data:
  ; TODO: Check type here and free lists (type 1) correctly, i. e. iteratively.
  ;       (Or rather: Decrement the reference count of each list element)
  call void @free(i8* %data)

  br label %free_stack_struct

free_stack_struct:
  %ret = phi i8* [ %data, %top ], [ null, %do_free_data ]

  %mem = bitcast %stack_element* %element to i8*
  call void @free(i8* %mem)

  ret i8* %ret
}

; Increment the reference count of a stack_element.
define void @stack_element_ref(%stack_element* %element) {
  %refCount = call i32 @stack_element_get_refcount(%stack_element* %element)
  %newRefCount = add i32 %refCount, 1
  call void @stack_element_set_refcount(%stack_element* %element, i32 %newRefCount)

  ret void
}

; Get the type of the data in a stack_element struct.
;
; See the definition of %stack_element for a description of
; possible type values.
define i8 @stack_element_get_type(%stack_element* %element) {
  ; dataType is member #0
  %dataType0 = getelementptr %stack_element* %element, i32 0, i32 0
  %dataType1 = load i8* %dataType0
  ret i8 %dataType1
}

; Set the type of the data in a stack_element struct.
;
; See the definition of %stack_element for a description of
; possible type values.
define void @stack_element_set_type(%stack_element* %element, i8 %type) {
  ; dataType is member #0
  %dataTypeDestPtr = getelementptr %stack_element* %element, i32 0, i32 0
  store i8 %type, i8* %dataTypeDestPtr

  ret void
}

; Get the raw, uncasted data pointer of a stack_element struct.
define i8* @stack_element_get_data(%stack_element* %element) {
  ; dataPtr is member #1
  %dataPtr0 = getelementptr %stack_element* %element, i32 0, i32 1
  %dataPtr1 = load i8** %dataPtr0
  ret i8* %dataPtr1
}

; Get data from stack as an in integer numeral
define i64 @stack_element_get_int_data(%stack_element* %element) {
  ; get raw data
  %data = call i8*(%stack_element*)* @stack_element_get_data(%stack_element* %element)

  ; convert to int, check for error
  %top_int0 = call i32 @atol(i8* %data)
  %top_int1 = sext i32 %top_int0 to i64

 ; return
 ret i64 %top_int1
}

; Set the raw data pointer of a stack_element struct.
define void @stack_element_set_data(%stack_element* %element, i8* %data) {
  ; dataPtr is member #1
  %dataPtr = getelementptr %stack_element* %element, i32 0, i32 1
  store i8* %data, i8** %dataPtr

  ret void
}

; Set the reference count of a stack_element struct.
define void @stack_element_set_refcount(%stack_element* %element, i32 %refCount) {
  ; refCount is member #2
  %refCountDestPtr = getelementptr %stack_element* %element, i32 0, i32 2
  store i32 %refCount, i32* %refCountDestPtr

  ret void
}

; Get the reference count of a stack_element struct.
define i32 @stack_element_get_refcount(%stack_element* %element) {
  ; refCount is member #2
  %refCount0 = getelementptr %stack_element* %element, i32 0, i32 2
  %refCount1 = load i32* %refCount0

  ret i32 %refCount1
}

; Get the "next element" pointer of a stack_element struct.
define %stack_element* @stack_element_get_next(%stack_element* %element) {
  ; nextElementPtr is member #3
  %nextElement0 = getelementptr %stack_element* %element, i32 0, i32 3
  %nextElement1 = load %stack_element** %nextElement0

  ret %stack_element* %nextElement1
}

; Set the "next element" pointer of a stack_element struct.
define void @stack_element_set_next(%stack_element* %element, %stack_element* %next) {
  ; nextElementPtr is member #3
  %nextPtr = getelementptr %stack_element* %element, i32 0, i32 3
  store %stack_element* %next, %stack_element** %nextPtr

  ret void
}

; Assert that the data in the stack_element has the passed type.
;
; If the types do not match, crash the program with an appropriate error message.
; This actually checks if the dataType member is equal to %want_type.
define void @stack_element_assert_type(%stack_element* %element, i8 %want_type) {
  %actual_type = call i8 @stack_element_get_type(%stack_element* %element)
  %is_valid = icmp eq i8 %actual_type, %want_type
  br i1 %is_valid, label %valid_type, label %invalid_type

valid_type:
  ; All good. Do nothing.
  ret void

invalid_type:
  ; Bail out!
  %err_type_mismatch = getelementptr [16 x i8]* @err_type_mismatch, i8 0, i8 0
  call %stack_element* @push_string_cpy(i8* %err_type_mismatch)
  call void @crash(i1 0)

  ret void
}

; Get (but do not remove) the topmost stack_element struct.
;
; Crashes the program if the stack is empty.
define %stack_element* @peek() {
  ; 1. Make sure we can peek something.
  call void @underflow_assert()

  ; 2. Do the actual peek.
  %stack = load %stack_element** @stack

  ret %stack_element* %stack
}

; Pop a stack_element struct from the stack.
define %stack_element* @pop_struct() {
  ; 1. Pop the stack.
  %stack = call %stack_element* @peek()
  %next = call %stack_element* @stack_element_get_next(%stack_element* %stack)
  store %stack_element* %next, %stack_element** @stack

  ; 2. Decrement the stack size.
  %stack_size0 = load i64* @stack_size
  %stack_size1 = sub i64 %stack_size0, 1
  store i64 %stack_size1, i64* @stack_size

  ; 3. That's it!
  ret %stack_element* %stack
}

; Push a stack_element struct onto the stack
define void @push_struct(%stack_element* %element) {
  ; 1. Push new element by updating it's pointer to the first element of the current stack
  %curr_head = load %stack_element** @stack
  call void @stack_element_set_next(%stack_element* %element, %stack_element* %curr_head)
  store %stack_element* %element, %stack_element** @stack

  ; 2. Increment stack size.
  %stack_size0 = call i64 @stack_get_size()
  %stack_size1 = add i64 %stack_size0, 1
  store i64 %stack_size1, i64* @stack_size

  ret void
}

; Pop a string from the stack.
;
; Crashes if the type of the topmost element is not "string".
;
; XXX: THIS IS A LEGACY FUNCTION. DO NOT USE IT IN NEW CODE.
;      New code should use proper reference counting.
define i8* @pop_string() {
  ; 1. Pop the stack.
  %stack = call %stack_element* @pop_struct()

  ; 2. Is the type string? If not, crash.
  call void @stack_element_assert_type(%stack_element* %stack, i8 0)

  ; 3. It's a string, everything is fine. Extract the string.
  %buf = call i8* @stack_element_get_data(%stack_element* %stack)

  ; 4. Finally, free the stack element.
  call i8* @stack_element_free(%stack_element* %stack, i1 0)

  ret i8 *%buf
}

; Push a string onto the stack, creating a new stack_element struct
; with a reference count of 1.
;
; The string must already be allocated _ON THE HEAP_.
define %stack_element* @push_string_ptr(i8* %str) {
  ; 1. Create and push a new stack_element.
  %curr_head = load %stack_element** @stack
  %new_head = call %stack_element* @stack_element_new(i8 0, i8* %str, %stack_element* %curr_head)
  store %stack_element* %new_head, %stack_element** @stack

  ; 2. Increment stack size.
  %stack_size0 = call i64 @stack_get_size()
  %stack_size1 = add i64 %stack_size0, 1
  store i64 %stack_size1, i64* @stack_size

  ret %stack_element* %new_head
}

; strdup() a string and push it onto the stack, creating a new stack_element struct
; with a reference count of 1.
define %stack_element* @push_string_cpy(i8* %str) {
  %str_copied = call i8* @xstrdup(i8* %str)
  %ret = call %stack_element* @push_string_ptr(i8* %str_copied)

  ret %stack_element* %ret
}

; pops element from stack and converts to integer
; returns the element, in case of error returns undefined
define i64 @pop_int(){
  ; pop
  %top = call i8* @pop_string()

  ; convert to int, check for error
  %top_int0 = call i32 @atol(i8* %top)
  %top_int1 = sext i32 %top_int0 to i64

  ; return
  ret i64 %top_int1
}

define i64 @pop_bool(){
  ;pop an int element from stack
  %top = call i64 @pop_int()

  ;check whether it is 0 or 1
  switch i64 %top, label %error [ i64 0, label %its_bool
                                  i64 1, label %its_bool ]

error:
  ; Bail out!
  %err_not_bool = getelementptr [29 x i8]* @err_not_bool, i8 0, i8 0
  call %stack_element* @push_string_cpy(i8* %err_not_bool)
  call void @crash(i1 0)
  ret i64 -1

its_bool:
  ret i64 %top
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
  call %stack_element* @push_string_ptr(i8* %buffer_addr)

  ret void
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
  call %stack_element* @push_string_ptr(i8* %buffer_addr)

  ret void
}
