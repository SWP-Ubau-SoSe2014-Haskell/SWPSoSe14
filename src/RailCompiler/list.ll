; Module      : LLVM backend - list implementation
; Description : Contains our list implementation - based on a linked list (and in fact
;               uses the same data structures as our linked stack).
; Maintainers : Tilman Blumenbach et al.
; License     : MIT
;
; Lists are implemented as linked lists of stack_wrapper elements, i. e. just like our
; normal linked stack. The data type "list" is represented as a stack_element with type
; 1 and a data pointer pointing to the head of the aforementioned stack_wrapper linked list.
; Thus, an empty list is a stack_element with type 1 and a null data pointer.


; Types

; Types defined in linked_stack.ll.
%stack_element = type opaque
%stack_wrapper = type opaque


; External declarations

; Own external LLVM variables/functions
declare %stack_element* @stack_element_new(i8, i8*)
declare void @stack_element_unref(%stack_element*)
declare void @stack_element_set_data(%stack_element*, i8*)
declare i8* @stack_element_get_data(%stack_element*)
declare %stack_wrapper* @stack_wrapper_new(%stack_element*, %stack_wrapper*)
declare void @stack_wrapper_free(%stack_wrapper*)
declare %stack_element* @stack_wrapper_get_element(%stack_wrapper*)
declare %stack_wrapper* @stack_wrapper_get_next(%stack_wrapper*)
declare void @push_struct(%stack_element*)
declare %stack_element* @pop_struct()
declare void @stack_element_assert_type(%stack_element*, i8)
declare void @stack_element_assert_is_non_empty_list(%stack_element*)
declare i1 @do_equal(%stack_element* %struct_a, %stack_element*)


; Function definitions

; Create a new, empty list (also called "nil") with a refcount of 1.
define %stack_element* @list_new() {
    ; Type 1 is reserved for the "list" type.
    ; Empty lists are simply stack_element structs with a null data pointer.
    %elm = call %stack_element* @stack_element_new(i8 1, i8* null)
    ret %stack_element* %elm
}

; Decrement the refcount of each element of a list.
;
; This does not free the stack_element which holds the list itself.
; That one is free'd by stack_element_unref().
define void @list_unref_elements(%stack_element* %list) {
top:
    %head_wrapper0 = call i8* @stack_element_get_data(%stack_element* %list)
    %head_wrapper1 = bitcast i8* %head_wrapper0 to %stack_wrapper*
    br label %free_list_elements

free_list_elements:
    %curr_wrapper = phi %stack_wrapper* [ %head_wrapper1, %top ], [ %next_wrapper, %free_one_list_element ]

    %is_null = icmp eq %stack_wrapper* %curr_wrapper, null
    br i1 %is_null, label %done, label %free_one_list_element

free_one_list_element:
    %next_wrapper = call %stack_wrapper* @stack_wrapper_get_next(%stack_wrapper* %curr_wrapper)
    %elm = call %stack_element* @stack_wrapper_get_element(%stack_wrapper* %curr_wrapper)
    call void @stack_element_unref(%stack_element* %elm)
    call void @stack_wrapper_free(%stack_wrapper* %curr_wrapper)

    br label %free_list_elements

done:
    ; Nothing to do since the list is empty.
    ret void
}

; Prepend a stack_element to a list (also called "cons").
;
; Returns its first parameter.
define %stack_element* @list_prepend(%stack_element* %list, %stack_element* %element) {
    ; A list contains a stack_wrapper as its data. This is the wrapped head of the list.
    %head_wrapper0 = call i8* @stack_element_get_data(%stack_element* %list)
    %head_wrapper1 = bitcast i8* %head_wrapper0 to %stack_wrapper*

    ; Now create a new wrapper for the element we want to prepend.
    %new_head_wrapper = call %stack_wrapper* @stack_wrapper_new(%stack_element* %element, %stack_wrapper* %head_wrapper1)

    ; This is the new list head, so store it in the topmost stack_element.
    %data = bitcast %stack_wrapper* %new_head_wrapper to i8*
    call void @stack_element_set_data(%stack_element* %list, i8* %data)

    ; That's it!
    ret %stack_element* %list
}

; Pop the head off a non-empty list.
define %stack_element* @list_pop(%stack_element* %list) {
    ; Get the top stack_wrapper of the list.
    %head_wrapper0 = call i8* @stack_element_get_data(%stack_element* %list)
    %head_wrapper1 = bitcast i8* %head_wrapper0 to %stack_wrapper*

    ; Get the contents of the wrapper.
    %top_elm = call %stack_element* @stack_wrapper_get_element(%stack_wrapper* %head_wrapper1)

    ; What's the new head of the list?
    %new_head0 = call %stack_wrapper* @stack_wrapper_get_next(%stack_wrapper* %head_wrapper1)
    %new_head1 = bitcast %stack_wrapper* %new_head0 to i8*
    call void @stack_element_set_data(%stack_element* %list, i8* %new_head1)

    ; Now we can free the old topmost wrapper.
    call void @stack_wrapper_free(%stack_wrapper* %head_wrapper1)

    ; That's it!
    ret %stack_element* %top_elm
}

; Compare two lists.
;
; Returns 1 if both lists are equal or 0 otherwise.
;
; Crashes the program on errors (prints an appropriate error message).
define i1 @list_equal(%stack_element* %list_a, %stack_element* %list_b) {
top:
    ; Get the top stack_wrappers of both lists.
    %head_wrapper_a0 = call i8* @stack_element_get_data(%stack_element* %list_a)
    %head_wrapper_a1 = bitcast i8* %head_wrapper_a0 to %stack_wrapper*

    %head_wrapper_b0 = call i8* @stack_element_get_data(%stack_element* %list_b)
    %head_wrapper_b1 = bitcast i8* %head_wrapper_b0 to %stack_wrapper*

    br label %compare_lists

compare_lists:
    %curr_wrapper_a = phi %stack_wrapper* [ %head_wrapper_a1, %top ], [ %next_wrapper_a, %compare_elements ]
    %curr_wrapper_b = phi %stack_wrapper* [ %head_wrapper_b1, %top ], [ %next_wrapper_b, %compare_elements ]

    ; If at least one wrapper is null, we may either have reached the end
    ; of BOTH lists (which means that they are equal) or one list is shorter
    ; than the other, which means that they are NOT equal.
    %wrapper_a_is_null = icmp eq %stack_wrapper* %curr_wrapper_a, null
    %wrapper_b_is_null = icmp eq %stack_wrapper* %curr_wrapper_b, null
    %at_least_one_null = or i1 %wrapper_a_is_null, %wrapper_b_is_null
    br i1 %at_least_one_null, label %null_check, label %compare_elements

null_check:
    ; If both wrappers are null, we are finished and the lists are equal.
    ; Otherwise one list is shorter than the other and the lists are NOT
    ; equal.
    %both_null = icmp eq %stack_wrapper* %curr_wrapper_a, %curr_wrapper_b
    br i1 %both_null, label %end_equal, label %end_not_equal

compare_elements:
    ; We have two elements which are both not null, so we can compare them.
    ; Set next pointers:
    %next_wrapper_a = call %stack_wrapper* @stack_wrapper_get_next(%stack_wrapper* %curr_wrapper_a)
    %next_wrapper_b = call %stack_wrapper* @stack_wrapper_get_next(%stack_wrapper* %curr_wrapper_b)

    ; Get the wrapped stack elements.
    %elm_a = call %stack_element* @stack_wrapper_get_element(%stack_wrapper* %curr_wrapper_a)
    %elm_b = call %stack_element* @stack_wrapper_get_element(%stack_wrapper* %curr_wrapper_b)

    ; Now we can do the actual comparison.
    %are_equal = call i1 @do_equal(%stack_element* %elm_a, %stack_element* %elm_b)
    ; If the elements are equal, check the next pair, else exit.
    br i1 %are_equal, label %compare_lists, label %end_not_equal

end_equal:
    ret i1 1

end_not_equal:
    ret i1 0
}


; Convenience functions for use in generated code.

; Push nil (an empty list) onto the stack.
define void @gen_list_push_nil() {
    %list = call %stack_element* @list_new()
    call void @push_struct(%stack_element* %list)
    ret void
}

; Prepend the topmost element to the list, which is the stack element after
; the topmost element, and push the resulting list.
define void @gen_list_cons() {
    %elm_to_prepend = call %stack_element* @pop_struct()
    %list = call %stack_element* @pop_struct()

    ; Make sure %list is a list.
    call void @stack_element_assert_type(%stack_element* %list, i8 1)

    ; Now we can prepend the element to the list...
    call %stack_element* @list_prepend(%stack_element* %list, %stack_element* %elm_to_prepend)

    ; ...and push the list again.
    call void @push_struct(%stack_element* %list)

    ret void
}

define void @gen_list_breakup() {
    %list = call %stack_element* @pop_struct()
    call void @stack_element_assert_is_non_empty_list(%stack_element* %list)

    ; Now pop the topmost list element.
    %top = call %stack_element* @list_pop(%stack_element* %list)

    ; Now we can push the list and its former first element again.
    call void @push_struct(%stack_element* %list)
    call void @push_struct(%stack_element* %top)

    ret void
}
