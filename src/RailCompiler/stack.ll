@stack = global [1000 x i8*] undef ; stack containing pointer to i8 
@sp = global i64 undef ; global stack pointer
@true = global [2 x i8] c"1\00"
@false = global [2 x i8] c"0\00"

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

define i64 @add() {
  ; get top of stack
  %top_1   = call i8*()* @pop()

  ; get second top of stack
  %top_2   = call i8*()* @pop()

  ; add the two values
  %res = add i64 %top_1, %top_2

  ; store result on stack
  call void@push(i8* %res)
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

; UNTESTED
define i64 @strlen(i8* %str) {
entry:
  br label %loop
loop:
  %i = phi i64 [1, %entry ], [ %next_i, %loop ]
  %next_i = add i64 %i, 1
  %addr = getelementptr i8* %str, i64 %i
  %c = load i8* %addr
  %cond = icmp eq i8 %c, 0
  br i1 %cond, label %finished, label %loop
finished:
  ret i64 %i
}

; UNTESTED
define i8* @streq(i8* %str1, i8* %str2) {
entry:
  br label %loop
loop:
  %i = phi i64 [ 1, %entry ], [ %next_i, %cont ]
  %addr1 = getelementptr i8* %str1, i64 %i
  %addr2 = getelementptr i8* %str2, i64 %i
  %c1 = load i8* %addr1
  %c2 = load i8* %addr2
  %cond = icmp eq i8 %c1, %c2 
  br i1 %cond, label %cont, label %fail
cont:
  %next_i = add i64 %i, 1
  %cond2 = icmp eq i8 %c1, 0 
  br i1 %cond2, label %success, label %loop
success:
  %t = getelementptr [2 x i8]* @true, i64 0, i64 0
  ret i8* %t
fail:	
  %f = getelementptr [2 x i8]* @true, i64 0, i64 0
  ret i8* %f
}

