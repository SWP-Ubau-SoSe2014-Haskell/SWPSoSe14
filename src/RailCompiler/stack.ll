@stack = global [1000 x i8*] undef
@sp = global i64 undef

define void @push(i8* %str_ptr) {
  %sp   = load i64* @sp
  %addr = getelementptr [1000 x i8*]* @stack, i8 0, i64 %sp
  store i8* %str_ptr, i8** %addr
  %newsp = add i64 %sp, 1
  store i64 %newsp, i64* @sp
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
