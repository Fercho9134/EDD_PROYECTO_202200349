module cola_clientes_module
    implicit none
    
    ! Definición del tipo de nodo
    type :: Node_Cola_Cliente
        integer :: id_cliente
        character(len=:), allocatable :: nombre
        integer :: tiempo_espera
        integer :: imagenes_pequenas
        integer :: imagenes_grandes
        type(Node_Cola_Cliente), pointer :: next => null()
    end type Node_Cola_Cliente

    ! Definición del tipo de cola
    type, public :: Cola_Clientes
        type(Node_Cola_Cliente), pointer :: head => null()   ! Apunta al primer nodo
        type(Node_Cola_Cliente), pointer :: tail => null()    ! Apunta al último nodo

        contains
            procedure :: encolar_cliente_nuevo
            procedure :: desencolar_cliente_cola
            procedure :: imprimir_cola_clientes

    end type Cola_Clientes 
    

contains

    subroutine encolar_cliente_nuevo(this, id_cliente, nombre, tiempo_espera, cantidad_imagenes_pequenas, cantidad_imagenes_grandes)
        class(Cola_Clientes), intent(inout) :: this
        integer, intent(in) :: id_cliente
        character(len=*), intent(in) :: nombre
        integer, intent(in) :: tiempo_espera
        integer, intent(in) :: cantidad_imagenes_pequenas
        integer, intent(in) :: cantidad_imagenes_grandes

        ! Crear un nuevo nodo
        type(Node_Cola_Cliente), pointer :: new_node
        allocate(new_node)

        ! Asignar valores al nuevo nodo
        new_node%id_cliente = id_cliente
        new_node%nombre = nombre
        new_node%tiempo_espera = tiempo_espera
        new_node%imagenes_pequenas = cantidad_imagenes_pequenas
        new_node%imagenes_grandes = cantidad_imagenes_grandes
        new_node%next => null()

        ! Encolar el nuevo nodo
        if (associated(this%head)) then
            this%tail%next => new_node
            this%tail => new_node
        else
            this%head => new_node
            this%tail => new_node
        end if
    end subroutine encolar_cliente_nuevo

    subroutine desencolar_cliente_cola(this, nodo)
        class(Cola_Clientes), intent(inout) :: this
        type(Node_Cola_Cliente), pointer :: nodo
    
        ! Desencolar el primer nodo
        if (associated(this%head)) then
            nodo => this%head
            this%head => this%head%next
        else
            print *, "La cola está vacía"
        end if
    end subroutine desencolar_cliente_cola
    
    

    subroutine imprimir_cola_clientes(this)
        class(Cola_Clientes), intent(in) :: this
        type(Node_Cola_Cliente), pointer :: current

        current => this%head
        do while (associated(current))
            print *, "ID cliente:", current%id_cliente, "Nombre:", current%nombre, "Espera: ", current%tiempo_espera,& 
            "Ima Peque:",current%imagenes_pequenas, "Ima Grande:", current%imagenes_grandes
            current => current%next
        end do
    end subroutine imprimir_cola_clientes
    

end module cola_clientes_module
