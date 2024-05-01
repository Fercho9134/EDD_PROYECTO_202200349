module cola_niveles_m
    use tipo_nodo_abb_m
    implicit none
    
    public :: Cola, encolar, desencolar
    
    type :: nodo_cola
        type(nodo) :: nodo_capa
        type(nodo_cola), pointer :: siguiente => NULL()
    end type nodo_cola
    
    type :: Cola
        type(nodo_cola), pointer :: cabeza => NULL()
        type(nodo_cola), pointer :: cola => NULL()
    end type Cola
    
contains

    subroutine encolar(q, valor)
        implicit none
        class(Cola), intent(inout) :: q
        type(nodo), intent(in) :: valor
        
        type(nodo_cola), pointer :: nuevoNodo
        allocate(nuevoNodo)
        nuevoNodo%nodo_capa = valor
        nuevoNodo%siguiente => NULL()
        
        if (.not. associated(q%cabeza)) then
            q%cabeza => nuevoNodo
            q%cola => nuevoNodo
        else
            q%cola%siguiente => nuevoNodo
            q%cola => nuevoNodo
        end if
    end subroutine encolar

    function desencolar(q)  result (nodoDesencolado)
        implicit none
        class(Cola), intent(inout) :: q
        
        type(nodo), pointer :: nodoDesencolado

        if (.not. associated(q%cabeza)) then
            print *, "La cola está vacía."
            return
        end if

        nodoDesencolado => q%cabeza%nodo_capa

        if (associated(q%cabeza%siguiente)) then
            q%cabeza => q%cabeza%siguiente
        else
            q%cabeza => NULL()
            q%cola => NULL()
        end if

    end function desencolar


end module cola_niveles_m
