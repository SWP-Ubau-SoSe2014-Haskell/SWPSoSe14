; Module      : LLVM backend - comparison functions
; Description : Contains LLVM functions for integer/float comparison.
; Maintainers : Sascha Zinke, Tudor Soroceanu
; License     : MIT
;
; These comparison functions are used by our LLVM backend and operate directly
; on the stack -- see stack.ll.

@err_type = external global [14 x i8]
@err_zero = external global [18 x i8]
@popped = external global [13 x i8]
@true = external global [2 x i8]
@false = external global [2 x i8]

%stack_element = type opaque
%struct.stack_elem = type { i32, %union.anon }
%union.anon = type { i8* }

declare i8* @pop_string()
declare %stack_element* @push_string_ptr(i8* %str)
declare %stack_element* @push_string_cpy(i8* %str)
declare void @push_int(i64)
declare void @push_float(double)
declare void @underflow_assert()
declare signext i32 @printf(i8*, ...)
declare i32 @get_stack_elem(i8*, %struct.stack_elem*)

@main.number_a = private unnamed_addr constant [4 x i8] c"-57\00"
@main.number_b  = private unnamed_addr constant [4 x i8] c"-56\00"

define i32 @main_greater() {
  ; push two numbers on the stack
  %number0 = getelementptr [4 x i8]* @main.number_a, i64 0, i64 0   
  %number1 = getelementptr [4 x i8]* @main.number_b, i64 0, i64 0   

  call %stack_element* @push_string_cpy(i8* %number0)
  call %stack_element* @push_string_cpy(i8* %number1)

  call i32 @greater()
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
 
  ; get top
  call void @underflow_assert()
  %number_a = call i8* @pop_string()

  ; get top-1
  call void @underflow_assert()
  %number_b = call i8* @pop_string()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %cmp_int, label %type_check_a_float

cmp_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; the actual comparison
  %equal_int = icmp eq i32 %ival_a, %ival_b 
  br i1 %equal_int, label %exit_with_true, label %exit_with_false

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %cmp_float, label %exit_with_invalid_type

cmp_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4

  ; prevent division by zero
  %equal_float = fcmp oeq float %fval_a, %fval_b
  br i1 %equal_float, label %exit_with_true, label %exit_with_false


exit_with_invalid_type: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_true: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [2 x i8]* @true, i64 0, i64 0))
  br label %exit_with_success

exit_with_false: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [2 x i8]* @false, i64 0, i64 0))
  br label %exit_with_success

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit:
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
 
  ; get top
  call void @underflow_assert()
  %number_a = call i8* @pop_string()

  ; get top-1
  call void @underflow_assert()
  %number_b = call i8* @pop_string()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

get_type_b:
  ; get type of number_b
  %ret_b = call i32 @get_stack_elem(i8* %number_b, %struct.stack_elem* %new_elem_b)
  %is_zero_b = icmp slt i32 %ret_b, 0
  br i1 %is_zero_b, label %exit_with_failure, label %type_check_a_int

type_check_a_int:
  ; first, load the new_elem_a.type element. check whether it is 1 (aka INT).
  %type_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %type_a = load i32* %type_a_ptr, align 4
  %is_int_a = icmp eq i32 %type_a, 1
  br i1 %is_int_a, label %type_check_b_int, label %type_check_a_float

type_check_b_int:
  ; first, load the new_elem_b.type element. check whether it is 1 (aka INT).
  %type_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %type_b = load i32* %type_b_ptr, align 4
  %is_int_b = icmp eq i32 %type_b, 1
  br i1 %is_int_b, label %cmp_int, label %type_check_a_float

cmp_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; the actual comparison
  %greater_int = icmp sgt i32 %ival_a, %ival_b 
  br i1 %greater_int, label %exit_with_true, label %exit_with_false

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %cmp_float, label %exit_with_invalid_type

cmp_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4

  ; prevent division by zero
  %greater_float = fcmp ogt float %fval_a, %fval_b
  br i1 %greater_float, label %exit_with_true, label %exit_with_false


exit_with_invalid_type: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_true: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [2 x i8]* @true, i64 0, i64 0))
  br label %exit_with_success

exit_with_false: 
  call %stack_element* @push_string_cpy(i8* getelementptr inbounds(
                                          [2 x i8]* @false, i64 0, i64 0))
  br label %exit_with_success

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result

}


