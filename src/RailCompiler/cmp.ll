; Module      : LLVM backend - comparison functions
; Description : Contains LLVM functions for integer/float comparison.
; Maintainers : Sascha Zinke, Tudor Soroceanu
; License     : MIT
;
; These comparison functions are used by our LLVM backend and operate directly
; on the stack -- see stack.ll.

@err_numeric = external global [56 x i8]
@err_type = external global [14 x i8]
@err_zero = external global [18 x i8]
@popped = external global [13 x i8]
@true = external global [2 x i8]
@false = external global [2 x i8]

%stack_element = type opaque
%struct.stack_elem = type { i32, %union.anon }
%union.anon = type { i8* }

declare i8* @stack_element_get_data(%stack_element* %element)
declare void @stack_element_unref(%stack_element* %element)
declare i32 @get_stack_elem(i8*, %struct.stack_elem*)
declare %stack_element* @push_string_ptr(i8* %str)
declare %stack_element* @push_string_cpy(i8* %str)
declare %stack_element* @pop_struct()
declare signext i32 @printf(i8*, ...)
declare void @push_float(double)
declare void @underflow_assert()
declare void @push_int(i64)
declare i8* @pop_string()
declare void @crash(i1)

@main.number_a = private unnamed_addr constant [4 x i8] c"150\00"
@main.number_b  = private unnamed_addr constant [4 x i8] c"151\00"

define i32 @main_cmp() {
  ; push two numbers on the stack
  %number0 = getelementptr [4 x i8]* @main.number_a, i64 0, i64 0   
  %number1 = getelementptr [4 x i8]* @main.number_b, i64 0, i64 0   

  call %stack_element* @push_string_cpy(i8* %number0)
  call %stack_element* @push_string_cpy(i8* %number1)

  call i32 @equal()
  %result = call i8* @pop_string()
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]*
              @popped, i32 0, i32 0), i8* %result)

  ret i32 0
}


;##############################################################################
;                                 equal
;##############################################################################

define i32 @equal(){
  ; return value of this function
  %func_result = alloca i32, align 4

  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8
 
  ; get top of stack
  call void @underflow_assert()
  %struct_a = call %stack_element*()* @pop_struct()
  %number_a = call i8*(%stack_element*)* @stack_element_get_data(
                                                   %stack_element* %struct_a)

  ; get second top of stack
  call void @underflow_assert()
  %struct_b = call %stack_element*()* @pop_struct()
  %number_b = call i8*(%stack_element*)* @stack_element_get_data(
                                                   %stack_element* %struct_b)

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_numeric_failure, label %get_stack_elem_b

get_stack_elem_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_numeric_failure, label %get_types

get_types:
  ; type of a
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %val_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1

  ; type of b
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %val_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1

  switch i32 %type_a, label %exit_with_invalid_type [
                                        i32 1, label %assume_b_int
                                        i32 2, label %assume_b_float]

;##############################################################################
;                        integer comparison
;##############################################################################

assume_b_int:
  ; check whether it is 1 (aka INT).
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %cmp_int, label %exit_with_invalid_type
                                       
cmp_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_cast = bitcast %union.anon* %val_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_cast = bitcast %union.anon* %val_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; the actual comparison
  %equal_int = icmp eq i32 %ival_a, %ival_b 
  br i1 %equal_int, label %exit_with_true, label %exit_with_false

;##############################################################################
;                        floating point comparison
;##############################################################################

assume_b_float:
  ; check whether it is 2 (aka FLOAT).
  %is_float_b = icmp eq i32 %type_b, 2
  br i1 %is_float_b, label %cmp_float, label %exit_with_invalid_type

cmp_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_cast = bitcast %union.anon* %val_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4

  ; get new_elem_b.fval that contains the float value
  %fval_b_cast = bitcast %union.anon* %val_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4

  %equal_float = fcmp oeq float %fval_a, %fval_b
  br i1 %equal_float, label %exit_with_true, label %exit_with_false


exit_with_numeric_failure:
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                       [56 x i8]* @err_numeric, i64 0, i64 0))
  br label %exit_with_failure

exit_with_invalid_type:
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  call void(%stack_element*)* @stack_element_unref(%stack_element* %struct_a)
  call void(%stack_element*)* @stack_element_unref(%stack_element* %struct_b)
  call void @crash(i1 0)
  br label %exit

exit_with_true: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [2 x i8]* @true, i64 0, i64 0))
  br label %exit_with_success

exit_with_false: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [2 x i8]* @false, i64 0, i64 0))
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit:
  call void(%stack_element*)* @stack_element_unref(%stack_element* %struct_a)
  call void(%stack_element*)* @stack_element_unref(%stack_element* %struct_b)
  %result = load i32* %func_result
  ret i32 %result
}

;##############################################################################
;                                 greater
;##############################################################################

define i32 @greater(){
  ; return value of this function
  %func_result = alloca i32, align 4

  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8
 
  ; get top of stack
  call void @underflow_assert()
  %struct_a = call %stack_element*()* @pop_struct()
  %number_a = call i8*(%stack_element*)* @stack_element_get_data(
                                                   %stack_element* %struct_a)

  ; get second top of stack
  call void @underflow_assert()
  %struct_b = call %stack_element*()* @pop_struct()
  %number_b = call i8*(%stack_element*)* @stack_element_get_data(
                                                   %stack_element* %struct_b)

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_numeric_failure, label %get_stack_elem_b

get_stack_elem_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_numeric_failure, label %get_types

get_types:
  ; type of a
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %val_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1

  ; type of b
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %val_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1

  switch i32 %type_a, label %exit_with_invalid_type [
                                        i32 1, label %assume_b_int
                                        i32 2, label %assume_b_float]

 ;##############################################################################
;                        integer greater
;##############################################################################

assume_b_int:
  ; check whether it is 1 (aka INT).
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %cmp_int, label %exit_with_invalid_type
 
cmp_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_cast = bitcast %union.anon* %val_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_cast = bitcast %union.anon* %val_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; the actual comparison
  %greater_int = icmp sgt i32 %ival_a, %ival_b 
  br i1 %greater_int, label %exit_with_true, label %exit_with_false

;##############################################################################
;                        floating point multiplication
;##############################################################################

assume_b_float:
  ; check whether it is 2 (aka FLOAT).
  %is_float_b = icmp eq i32 %type_b, 2
  br i1 %is_float_b, label %cmp_float, label %exit_with_invalid_type

cmp_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_cast = bitcast %union.anon* %val_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4

  ; get new_elem_b.fval that contains the float value
  %fval_b_cast = bitcast %union.anon* %val_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4

  ; prevent division by zero
  %greater_float = fcmp ogt float %fval_a, %fval_b
  br i1 %greater_float, label %exit_with_true, label %exit_with_false

exit_with_numeric_failure:
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                       [56 x i8]* @err_numeric, i64 0, i64 0))
  br label %exit_with_failure

exit_with_invalid_type:
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  call void(%stack_element*)* @stack_element_unref(%stack_element* %struct_a)
  call void(%stack_element*)* @stack_element_unref(%stack_element* %struct_b)
  call void @crash(i1 0)
  br label %exit

exit_with_true: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [2 x i8]* @true, i64 0, i64 0))
  br label %exit_with_success

exit_with_false: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [2 x i8]* @false, i64 0, i64 0))
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit:
  call void(%stack_element*)* @stack_element_unref(%stack_element* %struct_a)
  call void(%stack_element*)* @stack_element_unref(%stack_element* %struct_b)
  %result = load i32* %func_result
  ret i32 %result
}
