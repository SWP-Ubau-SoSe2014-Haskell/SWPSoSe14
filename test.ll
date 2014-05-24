; ModuleID = 'rail-heaven'

@0 = internal unnamed_addr constant [8 x i8] c"Works!\0A\0A"

declare void @push(i8*)

declare i8* @pop()

declare i8* @peek()

declare i32 @puts(i8*)

define i32 @main() {
l_1:
  call void @push(i8* getelementptr inbounds ([8 x i8]* @0, i8 0, i8 0))
  %0 = call i8* @pop()
  %1 = call i32 @puts(i8* %0)
  ret i32 0
}
