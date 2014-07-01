; Module      : LLVM backend - general math functions
; Description : Contains LLVM functions for mathematical operations like addition etc.
; Maintainers : Sascha Zinke
; License     : MIT
;
; These functions are used by our LLVM backend and operate directly on the stack --
; see stack.ll.

@err_type = external global [14 x i8]
@err_zero = external global [18 x i8]
@popped = external global [13 x i8]

%struct.stack_elem = type { i32, %union.anon }
%union.anon = type { i8* }

declare i8* @pop()
declare void @push(i8*)
declare void @push_int(i64)
declare void @push_float(double)
declare void @underflow_assert()
declare signext i32 @printf(i8*, ...)
declare i32 @get_stack_elem(i8*, %struct.stack_elem*)

@main.number_a = private unnamed_addr constant [4 x i8] c"-57\00"
@main.number_b  = private unnamed_addr constant [4 x i8] c"-58\00"

define i32 @main_math() {
  ; push two numbers on the stack
  %number0 = getelementptr [4 x i8]* @main.number_a, i64 0, i64 0   
  %number1 = getelementptr [4 x i8]* @main.number_b, i64 0, i64 0   

  call void(i8*)* @push(i8* %number0)
  call void(i8*)* @push(i8* %number1)

  call i32 @div()
  %result = call i8* @pop()
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]*
              @popped, i32 0, i32 0), i8* %result)

  ret i32 0
}


define i32 @mult() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer multiplication
;##############################################################################

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
  br i1 %is_int_b, label %add_int, label %type_check_a_float

add_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i64*
  %ival_a = load i64* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i64*
  %ival_b = load i64* %ival_b_cast, align 4

  ; add the two integers and store result on the stack
  %ires = mul i64 %ival_a, %ival_b
  call void(i64)* @push_int(i64 %ires)
  br label %exit_with_success

;##############################################################################
;                        floating point multiplication
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %mult_float, label %exit_with_invalid_type

mult_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4
  %fval_b_d = fpext float %fval_b to double

  ; sub the two floats and store result on the stack
  %fres= fmul double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}

define i32 @rem() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer remainder
;##############################################################################

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
  br i1 %is_int_b, label %rem_int, label %type_check_a_float

rem_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; add the two integers and store result on the stack
  %ires = srem i32 %ival_a, %ival_b
  %lres = sext i32 %ires to i64
  call void(i64)* @push_int(i64 %lres)
  br label %exit_with_success

;##############################################################################
;                        floating point remainder
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %rem_float, label %exit_with_invalid_type

rem_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4
  %fval_b_d = fpext float %fval_b to double

  ; sub the two floats and store result on the stack
  %fres= frem double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}

define i32 @sub() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer subtraction
;##############################################################################

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
  br i1 %is_int_b, label %sub_int, label %type_check_a_float

sub_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i64*
  %ival_a = load i64* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i64*
  %ival_b = load i64* %ival_b_cast, align 4

  ; add the two integers and store result on the stack
  %ires = sub i64 %ival_a, %ival_b
  call void(i64)* @push_int(i64 %ires)
  br label %exit_with_success

;##############################################################################
;                        floating point subtraction
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %sub_float, label %exit_with_invalid_type

sub_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4
  %fval_b_d = fpext float %fval_b to double

  ; sub the two floats and store result on the stack
  %fres= fsub double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}

define i32 @add() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert()
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert()
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer addition
;##############################################################################

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
  br i1 %is_int_b, label %add_int, label %type_check_a_float

add_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i64*
  %ival_a = load i64* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i64*
  %ival_b = load i64* %ival_b_cast, align 4

  ; add the two integers and store result on the stack
  %ires = add i64 %ival_a, %ival_b
  call void(i64)* @push_int(i64 %ires)
  br label %exit_with_success

;##############################################################################
;                        floating point addition
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %add_float, label %exit_with_invalid_type

add_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4
  %fval_b_d = fpext float %fval_b to double

  ; add the two floats and store result on the stack
  %fres= fadd double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}

define i32 @div() {
  ; return value of this function
  %func_result = alloca i32, align 4

  ; allocate memory on stack to hold our structures that contains the type
  ; of stack element and its casted value
  %new_elem_a = alloca %struct.stack_elem, align 8
  %new_elem_b = alloca %struct.stack_elem, align 8

  ; get top of stack
  call void @underflow_assert() 
  %number_a = call i8* @pop()

  ; get second top of stack
  call void @underflow_assert() 
  %number_b = call i8* @pop()

  ; get type of number_a
  %ret_a = call i32 @get_stack_elem(i8* %number_a, %struct.stack_elem* %new_elem_a)
  %is_zero_a = icmp slt i32 %ret_a, 0
  br i1 %is_zero_a, label %exit_with_failure, label %get_type_b

;##############################################################################
;                        integer division
;##############################################################################

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
  br i1 %is_int_b, label %div_int, label %type_check_a_float

div_int:
  ; get new_elem_a.ival that contains the casted integer value
  %ival_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %ival_a_cast = bitcast %union.anon* %ival_a_ptr to i32*
  %ival_a = load i32* %ival_a_cast, align 4

  ; get new_elem_b.ival that contains the casted integer value
  %ival_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %ival_b_cast = bitcast %union.anon* %ival_b_ptr to i32*
  %ival_b = load i32* %ival_b_cast, align 4

  ; prevent division by zero
  %div_by_zero = icmp eq i32 %ival_b, 0
  br i1 %div_by_zero, label %exit_with_zero, label %div_int_ok

div_int_ok:
  ; divide the two integers and store result on the stack
  %ires = sdiv i32 %ival_a, %ival_b
  %lres = sext i32 %ires to i64

  call void(i64)* @push_int(i64 %lres)
  br label %exit_with_success

;##############################################################################
;                        floating point division
;##############################################################################

type_check_a_float:
  %ftype_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 0
  %ftype_a = load i32* %ftype_a_ptr, align 4
  %is_float_a = icmp eq i32 %ftype_a, 2 
  br i1 %is_float_a, label %type_check_b_float, label %exit_with_invalid_type

type_check_b_float:
  %ftype_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 0
  %ftype_b = load i32* %ftype_b_ptr, align 4
  %is_float_b = icmp eq i32 %ftype_b, 2
  br i1 %is_float_b, label %div_float, label %exit_with_invalid_type

div_float:
  ; get new_elem_a.fval that contains the float value
  %fval_a_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_a, i32 0, i32 1
  %fval_a_cast = bitcast %union.anon* %fval_a_ptr to float*
  %fval_a = load float* %fval_a_cast, align 4
  %fval_a_d = fpext float %fval_a to double

  ; get new_elem_b.fval that contains the float value
  %fval_b_ptr = getelementptr inbounds %struct.stack_elem* %new_elem_b, i32 0, i32 1
  %fval_b_cast = bitcast %union.anon* %fval_b_ptr to float*
  %fval_b = load float* %fval_b_cast, align 4

  ; prevent division by zero
  %div_by_zero_f = fcmp oeq float %fval_b, 0.0
  br i1 %div_by_zero_f, label %exit_with_zero, label %div_float_ok

div_float_ok:
  ; divide the two floats and store result on the stack
  %fval_b_d = fpext float %fval_b to double
  %fres= fdiv double %fval_a_d, %fval_b_d
  call void(double)* @push_float(double %fres)
  br label %exit_with_success

exit_with_success:
  store i32 0, i32* %func_result
  br label %exit

exit_with_zero: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [18 x i8]* @err_zero, i64 0, i64 0))
  br label %exit_with_failure

exit_with_invalid_type: 
  call void(i8*)* @push(i8* getelementptr inbounds(
                                          [14 x i8]* @err_type, i64 0, i64 0))
  br label %exit_with_failure

exit_with_failure:
  store i32 -1, i32* %func_result
  br label %exit

exit:
  %result = load i32* %func_result
  ret i32 %result
}


