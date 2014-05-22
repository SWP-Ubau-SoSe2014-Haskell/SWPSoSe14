; ModuleID = 'rail-heaven'

@0 = internal unnamed_addr constant [6 x i8] c"hello "
@1 = internal unnamed_addr constant [4 x i8] c"otto"
@2 = internal unnamed_addr constant [13 x i8] c", my name is "
@3 = internal unnamed_addr constant [4 x i8] c"otto"
@4 = internal unnamed_addr constant [11 x i8] c" as well\5Cn\5C"

declare void @push(i8*)

declare i8* @pop()

declare i8* @peek()

declare i32 @puts(i8*)

define void @main() {
l_1:
  call void @push(i8* getelementptr inbounds ([6 x i8]* @0, i8 0, i8 0))
  %0 = call i8* @pop()
  %1 = call i32 @puts(i8* %0)
  call void @push(i8* getelementptr inbounds ([6 x i8]* @0, i8 0, i8 0))
  %2 = call i8* @pop()
  %3 = call i32 @puts(i8* %2)
  call void @push(i8* getelementptr inbounds ([6 x i8]* @0, i8 0, i8 0))
  %4 = call i8* @pop()
  %5 = call i32 @puts(i8* %4)
  call void @push(i8* getelementptr inbounds ([6 x i8]* @0, i8 0, i8 0))
  %6 = call i8* @pop()
  %7 = call i32 @puts(i8* %6)
  call void @push(i8* getelementptr inbounds ([6 x i8]* @0, i8 0, i8 0))
  %8 = call i8* @pop()
  %9 = call i32 @puts(i8* %8)
  ret void
}
