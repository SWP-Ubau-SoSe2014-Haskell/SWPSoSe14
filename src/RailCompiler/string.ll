; Module      : LLVM backend - string functions
; Description : Contains LLVM functions for operations on strings (e. g. concatenation).
; Maintainers : Maximilian Claus
; License     : MIT
;
; These functions are used by our LLVM backend and most of them operate
; directly on the stack -- see stack.ll.

@true = external global i8
@false = external global i8

declare i8* @pop()
declare void @push(i8*)
declare i64 @pop_int()
declare void @push_int(i64)
declare void @underflow_assert()

declare i8* @malloc(i16 zeroext) ; void *malloc(size_t) and size_t is 16 bits long (SIZE_MAX)

; TODO: free alloated space of input strings
define void @strapp() {
entry:
  call void @underflow_assert() 
  %str2 = call i8*()* @pop()
  call void @underflow_assert() 
  %str1 = call i8*()* @pop()

  ; compute length of input strings (TODO: maybe isolate strlen function for this purpose)
  call void(i8*)* @push(i8* %str1)
  call void()* @strlen()
  %len_str1 = call i64()* @pop_int()
  call void(i8*)* @push(i8* %str2)
  call void()* @strlen()
  %len_str2 = call i64()* @pop_int()

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
  call void(i8*)* @push(i8* %result)
  ret void
}


define void @strlen() {
entry:
  call void @underflow_assert() 
  %str = call i8*()* @pop()
  br label %loop
loop:
  %i = phi i64 [0, %entry ], [ %next_i, %loop ]
  %next_i = add i64 %i, 1
  %addr = getelementptr i8* %str, i64 %i
  %c = load i8* %addr
  %cond = icmp eq i8 %c, 0
  br i1 %cond, label %finished, label %loop
finished:
  call void(i64)* @push_int(i64 %i)
  ret void
}


; DEPRECATED
define void @streq() {
entry:
  call void @underflow_assert() 
  %str1 = call i8*()* @pop()
  call void @underflow_assert() 
  %str2 = call i8*()* @pop()
  br label %loop
loop:
  ; the phi instruction says that coming from the 'entry' label i is 1
  ; otherwise (coming from 'cont') i will be 'next_i'
  %i = phi i64 [ 1, %entry ], [ %next_i, %cont ]

  ; the the actual character
  %addr1 = getelementptr i8* %str1, i64 %i
  %addr2 = getelementptr i8* %str2, i64 %i
  %c1 = load i8* %addr1
  %c2 = load i8* %addr2

  ; if equal, jump to next character otherwise jump to 'fail' 
  %cond = icmp eq i8 %c1, %c2
  br i1 %cond, label %cont, label %fail

cont:
  %next_i = add i64 %i, 1
  %cond2 = icmp eq i8 %c1, 0
  br i1 %cond2, label %success, label %loop
success:
  %t = getelementptr i8* @true, i64 0
  call void(i8*)* @push(i8* %t)
  ret void
fail:
  %f = getelementptr i8* @false, i64 0
  call void(i8*)* @push(i8* %f)
  ret void
}


define void @strcut() {
entry:
  call void @underflow_assert() 
  %indx = call i64()* @pop_int()
  call void @underflow_assert() 
  %str = call i8*()* @pop()

  ; allocate space for result strings
  %len1_1 = add i64 %indx, 1
  %len1 = trunc i64 %len1_1 to i16
  call void(i8*)* @push(i8* %str)
  call void()* @strlen()
  %len_str = call i64()* @pop_int()
  %len2_1 = sub i64 %len_str, %indx
  %len2_2 = add i64 %len2_1, 1
  %len2 = trunc i64 %len2_2 to i16
  %result1 = call i8* @malloc(i16 %len1)
  %result2 = call i8* @malloc(i16 %len2)

  ; fill result1 string
  br label %loop1
loop1:
  %i = phi i64 [0, %entry], [ %next_i, %loop1 ]
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
  call void(i8*)* @push(i8* %result2)
  call void(i8*)* @push(i8* %result1)
  ret void
}
