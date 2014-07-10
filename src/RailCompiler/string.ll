; Module      : LLVM backend - string functions
; Description : Contains LLVM functions for operations on strings (e. g. concatenation).
; Maintainers : Maximilian Claus
; License     : MIT
;
; These functions are used by our LLVM backend and most of them operate
; directly on the stack -- see stack.ll.

@true = external global i8
@false = external global i8
@.str_err0 = unnamed_addr constant [41 x i8] c"Crash: strcut called with negative index\00"
@.str_err1 = unnamed_addr constant [58 x i8] c"Crash: strcut called with index larger than string length\00"
@strcut_neg_arg_err = global i8* getelementptr inbounds ([41 x i8]* @.str_err0, i64 0, i64 0), align 8
@strcut_too_large_arg_err = global i8* getelementptr inbounds ([58 x i8]* @.str_err1, i64 0, i64 0), align 8

; error handling
%FILE = type opaque
@stderr = external global %FILE*
declare signext i32 @fprintf(%FILE*, i8*, ...)
declare void @exit(i32 signext)

; stack functions
%stack_element = type opaque

declare void @underflow_assert()
declare %stack_element* @push_string_ptr(i8* %str)
declare %stack_element* @push_string_cpy(i8* %str)
declare void @push_int(i64)
declare %stack_element* @pop_struct()
declare i8* @stack_element_get_data(%stack_element* %element)
declare i64 @stack_element_get_int_data(%stack_element* %element)
declare void @stack_element_unref(%stack_element* %element)

declare i8* @malloc(i16 zeroext) ; void *malloc(size_t) and size_t is 16 bits long (SIZE_MAX)

; TODO: free alloated space of input strings
define void @strapp() {
entry:
  call void @underflow_assert() 
  %elem2 = call %stack_element*()* @pop_struct()
  %str2 = call i8*(%stack_element*)* @stack_element_get_data(%stack_element* %elem2)
  call void @underflow_assert() 
  %elem1 = call %stack_element*()* @pop_struct()
  %str1 = call i8*(%stack_element*)* @stack_element_get_data(%stack_element* %elem1)

  ; compute length of input strings
  %len_str1 = call i64(i8*)* @length(i8* %str1)
  %len_str2 = call i64(i8*)* @length(i8* %str2)

  ; allocate space for result string
  %len_result_1 = add i64 %len_str1, %len_str2
  %len_result_2 = add i64 %len_result_1, 1
  %len_result_3 = trunc i64 %len_result_2 to i16
  %result = call i8* @malloc(i16 %len_result_3)

  ; copy first string into result
  br label %loop1
loop1:
  %i = phi i64 [0, %entry], [ %next_i, %loop1 ]
  %next_i = add i64 %i, 1
  %addr = getelementptr i8* %str1, i64 %i
  %c = load i8* %addr
  %result_addr = getelementptr i8* %result, i64 %i
  store i8 %c, i8* %result_addr
  %cond = icmp eq i8 %c, 0
  br i1 %cond, label %finished, label %loop1
finished:
  ; copy second string into result
  br label %loop2
loop2:
  %j = phi i64 [0, %finished], [ %next_j, %loop2 ]
  %next_j = add i64 %j, 1
  %addr2 = getelementptr i8* %str2, i64 %j
  %c2 = load i8* %addr2
  %k = add i64 %j, %len_str1
  %result_addr2 = getelementptr i8* %result, i64 %k
  store i8 %c2, i8* %result_addr2
  %cond2 = icmp eq i8 %c2, 0
  br i1 %cond2, label %finished2, label %loop2
finished2:
  call void(%stack_element*)* @stack_element_unref(%stack_element* %elem2)
  call void(%stack_element*)* @stack_element_unref(%stack_element* %elem1)
  call %stack_element* @push_string_ptr(i8* %result)
  ret void
}

define i64 @length(i8* %str) {
entry:
  br label %loop
loop:
  %i = phi i64 [0, %entry ], [ %next_i, %loop ]
  %next_i = add i64 %i, 1
  %addr = getelementptr i8* %str, i64 %i
  %c = load i8* %addr
  %cond = icmp eq i8 %c, 0
  br i1 %cond, label %finished, label %loop
finished:
  ret i64 %i
}

define void @strlen() {
entry:  
  ; pop string
  call void @underflow_assert() 
  %elem = call %stack_element*()* @pop_struct()
  %str = call i8*(%stack_element*)* @stack_element_get_data(%stack_element* %elem)

  ; compute length
  %len = call i64(i8*)* @length(i8* %str)

  ; push length
  call void(%stack_element*)* @stack_element_unref(%stack_element* %elem)
  call void(i64)* @push_int(i64 %len)
  ret void
}

define void @strcut() {
entry:
  call void @underflow_assert()
  %elem2 = call %stack_element*()* @pop_struct()
  %indx = call i64(%stack_element*)* @stack_element_get_int_data(%stack_element* %elem2)
  call void @underflow_assert() 
  %elem1 = call %stack_element*()* @pop_struct()
  %str = call i8*(%stack_element*)* @stack_element_get_data(%stack_element* %elem1)

  ; allocate space for result strings
  %len1_1 = add i64 %indx, 1
  %len1 = trunc i64 %len1_1 to i16
  %len_str = call i64(i8*)* @length(i8* %str) 
  %len2_1 = sub i64 %len_str, %indx
  %len2_2 = add i64 %len2_1, 1
  %len2 = trunc i64 %len2_2 to i16
  %result1 = call i8* @malloc(i16 %len1)
  %result2 = call i8* @malloc(i16 %len2)

  ; check whether index argument is within bounds
  %stderr = load %FILE** @stderr

  %err0 = icmp slt i64 %indx, 0
  br i1 %err0, label %neg_arg, label %continue_check
neg_arg:
  %err_msg0 = load i8** @strcut_neg_arg_err
  call i32(%FILE*, i8*, ...)* @fprintf(%FILE* %stderr, i8* %err_msg0)
  call void @exit(i32 1)
  ret void
continue_check:
  %err1 = icmp sgt i64 %indx, %len_str
  br i1 %err1, label %too_large_arg, label %loop1
too_large_arg:
  %err_msg1 = load i8** @strcut_too_large_arg_err
  call i32(%FILE*, i8*, ...)* @fprintf(%FILE* %stderr, i8* %err_msg1)
  call void @exit(i32 1)
  ret void

  ; fill result1 string
loop1:
  %i = phi i64 [0, %continue_check], [ %next_i, %loop1 ]
  %next_i = add i64 %i, 1
  %addr = getelementptr i8* %str, i64 %i
  %c = load i8* %addr
  %result_addr = getelementptr i8* %result1, i64 %i
  store i8 %c, i8* %result_addr
  %cond = icmp eq i64 %i, %indx
  br i1 %cond, label %finished, label %loop1
finished:
  %end_addr = getelementptr i8* %result1, i64 %indx
  store i8 0, i8* %end_addr
  ; fill result2 string
  br label %loop2
loop2:
  %j = phi i64 [0, %finished], [ %next_j, %loop2 ]
  %next_j = add i64 %j, 1
  %k = add i64 %j, %indx
  %addr2 = getelementptr i8* %str, i64 %k
  %c2 = load i8* %addr2
  %result_addr2 = getelementptr i8* %result2, i64 %j
  store i8 %c2, i8* %result_addr2
  %cond2 = icmp eq i8 %c2, 0
  br i1 %cond2, label %finished2, label %loop2
finished2: 
  call void(%stack_element*)* @stack_element_unref(%stack_element* %elem2)
  call void(%stack_element*)* @stack_element_unref(%stack_element* %elem1)
  call %stack_element* @push_string_ptr(i8* %result2)
  call %stack_element* @push_string_ptr(i8* %result1)
  ret void
}

; vim:sw=2 ts=2 et
