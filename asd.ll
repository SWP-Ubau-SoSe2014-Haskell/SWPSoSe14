; ModuleID = 'rail-heaven'

%stack_element = type opaque
%struct.table = type { i8*, %stack_element*, %struct.table* }
%lambda_element = type { i32 (%struct.table*)**, %struct.table* }

@0 = internal unnamed_addr constant [2 x i8] c"a\00"

declare void @underflow_check()

declare void @print()

declare void @crash(i1)

declare void @start()

declare void @finish()

declare void @input()

declare void @eof_check()

declare %stack_element* @push_string_cpy(i8*)

declare i8* @pop()

declare i8* @peek()

declare void @add()

declare void @sub()

declare void @rem()

declare void @mult()

declare void @div()

declare i8* @streq()

declare i8* @strlen()

declare i8* @strapp()

declare i8* @strcut()

declare i64 @pop_int()

declare void @equal()

declare void @greater()

declare void @pop_into(%struct.table*, i8*)

declare void @push_from(%struct.table*, i8*)

declare i64 @pop_bool()

declare void @initialise(%struct.table*)

declare i8* @malloc(i64)

declare void @type()

declare void @copy_symbol_table(%struct.table*, %struct.table*)

declare void @push_lambda(i32 (%struct.table*)**, %struct.table*)

declare i32 (%struct.table*)* @get_lambda_pointer(%lambda_element*)

declare %lambda_element* @pop_lambda()

declare %struct.table* @get_lambda_table(%lambda_element*)

declare void @gen_list_push_nil()

declare void @gen_list_cons()

declare void @gen_list_breakup()

define i32 @main() {
entry:
  %table_alloc = call i8* @malloc(i64 24)
  %table = bitcast i8* %table_alloc to %struct.table*
  call void @initialise(%struct.table* %table)
  br label %l_1

l_1:                                              ; preds = %entry
  call void @start()
  %0 = call %stack_element* @push_string_cpy(i8* getelementptr inbounds ([2 x i8]* @0, i8 0, i8 0))
  call void @print()
  call void @finish()
  ret i32 0
}
