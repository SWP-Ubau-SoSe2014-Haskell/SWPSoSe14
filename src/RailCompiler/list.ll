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



; Function definitions

; Create a new, empty list (also called "nil") with a refcount of 1.
define %stack_element* @list_new() {
    ; Type 1 is reserved for the "list" type.
    ; Empty lists are simply stack_element structs with a null data pointer.
    %elm = call %stack_element* @stack_element_new(i8 1, i8* null)
    return %stack_element* %elm
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
    call void @stack_element_set_data(%stack_element %list, i8* %data)

    ; That's it!
    ret %stack_element* %list
}
