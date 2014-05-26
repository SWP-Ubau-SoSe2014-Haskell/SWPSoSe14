@stack = global [1000 x i8*] undef
@sp = global i64 undef
@true = internal constant [2 x i8] c"1\00"
@false = internal constant [2 x i8] c"0\00"

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
