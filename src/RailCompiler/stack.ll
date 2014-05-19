@stack = global [1000 x i8*] undef
@sp = global i64 undef

; nice getelementptr FAQ: http://llvm.org/docs/GetElementPtr.html
; %addr = getelementptr [1000 x i8*]* @stack,   i8 0,   i64 %sp
;                       value of pointer type,  index,    field

define void @push(i8* %str_ptr) {
  %sp   = load i64* @sp
  %addr = getelementptr [1000 x i8*]* @stack, i8 0, i64 %sp
  store i8* %str_ptr, i8** %addr
  %newsp = add i64 %sp, 1
  store i64 %newsp, i64* @sp
  ret void
}

define i64 @add(i64 %a, i64 %b) {
  ; get top of stack
  %top_1   = load i64* @sp
  %addr_1 = getelementptr [1000 x i8*]* @stack, i8 0, i64 %top_1

  ; get second top of stack
  %top_2 = sub i64 %sp, 1
  %addr_2 = getelementptr [1000 x i8*]* @stack, i8 0, i64 %top_2

  ; add the two values
  ; here might be some dereferencing of the addresses needed to ensure
  ; that we not add the addresse but the values the point to?
  %res = add i64 %addr_1, %addr_2
  ret i64 %res
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
