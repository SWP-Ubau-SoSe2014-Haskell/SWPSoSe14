@stack = global [1000 x i8*] undef ; stack containing pointer to i8 
@sp = global i64 undef ; global stack pointer

declare i64 @strtol(i8 zeroext)
declare i8* @ltostr(i64 zeroext)

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
  %val = load i8* %top

  ; convert to int, check for error
  %top_int = call i64 @strtol(i8 %val)
  
  ; return
  ret i64 %top_int
}

define void @push_int(i64 %top_int)
{
  ; convert to string
  %top_str = call i8* @ltostr(i64 %top_int)

  ; push on stack 
  call void(i8*)* @push(i8* %top_str)

  ret void
}

define void @add() {
  ; get top of stack
  %top_1   = call i64()* @pop_int()

  ; get second top of stack
  %top_2   = call i64()* @pop_int()

  ; add the two values
  %res = add i64 %top_1, %top_2

  ; store result on stack
  call void(i64)* @push_int(i64 %res)

  ret void
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

