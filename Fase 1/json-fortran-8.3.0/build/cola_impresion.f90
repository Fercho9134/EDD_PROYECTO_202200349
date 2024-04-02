module cola_impresion_module
    implicit none
    
    ! Definición del tipo de nodo
    type :: nodo_impresion
        integer :: id_cliente
        character(len=:), allocatable :: tamano
        integer :: pasos
        type(nodo_impresion), pointer :: next => null()
    end type nodo_impresion

    ! Definición del tipo de cola
    type, public :: cola_impresion
        type(nodo_impresion), pointer :: head => null()   ! Apunta al primer nodo
        type(nodo_impresion), pointer :: tail => null()   ! Apunta al último nodo

        contains
            procedure :: encolar_impresion
            procedure :: desencolar_impresion
            procedure :: imprimir_cola
    end type cola_impresion

contains

    ! Método para encolar un documento en la cola de impresión
    subroutine encolar_impresion(this, id_cliente, tamano)
        class(cola_impresion), intent(inout) :: this
        integer, intent(in) :: id_cliente
        character(len=*), intent(in) :: tamano

        ! Crear un nuevo nodo
        type(nodo_impresion), pointer :: new_node
        allocate(new_node)
        new_node%pasos = 0

        ! Asignar valores al nuevo nodo
        new_node%id_cliente = id_cliente
        allocate(new_node%tamano, source=tamano)
        new_node%next => null()

        ! Encolar el nuevo nodo
        if (associated(this%head)) then
            this%tail%next => new_node
            this%tail => new_node
        else
            this%head => new_node
            this%tail => new_node
        end if
    end subroutine encolar_impresion

    ! Método para desencolar un documento de la cola de impresión
    subroutine desencolar_impresion(this, id_cliente, tamano)
        class(cola_impresion), intent(inout) :: this
        integer, intent(out) :: id_cliente
        character(len=:), allocatable, intent(out) :: tamano

        if (associated(this%head)) then
            id_cliente = this%head%id_cliente
            allocate(tamano, source=this%head%tamano)

            ! Mover la cabeza de la cola al siguiente nodo
            this%head => this%head%next

        else
            print *, "La cola de impresión está vacía"
        end if
    end subroutine desencolar_impresion

    ! Método para imprimir el contenido de la cola de impresión
    subroutine imprimir_cola(this)
        class(cola_impresion), intent(in) :: this
        type(nodo_impresion), pointer :: current

        current => this%head
        do while (associated(current))
            print *, "ID Cliente:",current%id_cliente, "Tamano:", current%tamano
            current => current%next
        end do
    end subroutine imprimir_cola

end module cola_impresion_module
