module lista_clientes_atendidos_module
    implicit none
    
    ! Definición del tipo de nodo
    type :: nodo_cliente_atendido
        character(len=:), allocatable :: nombre_cliente
        integer :: ventanilla_atendida
        integer :: imagenes_impresas
        integer :: pasos_en_sistema
        type(nodo_cliente_atendido), pointer :: next => null()
    end type nodo_cliente_atendido

    ! Definición de la lista de clientes atendidos
    type, public :: lista_clientes_atendidos
        type(nodo_cliente_atendido), pointer :: head => null()   ! Apunta al primer nodo
        type(nodo_cliente_atendido), pointer :: tail => null()    ! Apunta al último nodo

        contains
            procedure :: agregar_cliente_atendido
            procedure :: imprimir_lista_clientes_atendidos
    end type lista_clientes_atendidos
    
contains

    ! Método para agregar un cliente atendido a la lista
    subroutine agregar_cliente_atendido(this, nombre_cliente, ventanilla_atendida, imagenes_impresas, pasos_en_sistema)
        class(lista_clientes_atendidos), intent(inout) :: this
        character(len=*), intent(in) :: nombre_cliente
        integer, intent(in) :: ventanilla_atendida
        integer, intent(in) :: imagenes_impresas
        integer, intent(in) :: pasos_en_sistema

        ! Crear un nuevo nodo
        type(nodo_cliente_atendido), pointer :: new_node
        allocate(new_node)

        ! Asignar valores al nuevo nodo
        new_node%nombre_cliente = nombre_cliente
        new_node%ventanilla_atendida = ventanilla_atendida
        new_node%imagenes_impresas = imagenes_impresas
        new_node%pasos_en_sistema = pasos_en_sistema
        new_node%next => null()

        ! Agregar el nuevo nodo al final de la lista
        if (associated(this%head)) then
            this%tail%next => new_node
            this%tail => new_node
        else
            this%head => new_node
            this%tail => new_node
        end if
    end subroutine agregar_cliente_atendido

    ! Método para imprimir la lista de clientes atendidos
    subroutine imprimir_lista_clientes_atendidos(this)
        class(lista_clientes_atendidos), intent(in) :: this
        type(nodo_cliente_atendido), pointer :: current

        current => this%head
        do while (associated(current))
            print *, "Nombre del cliente: ", current%nombre_cliente
            print *, "Ventanilla atendida: ", current%ventanilla_atendida
            print *, "Número de imágenes impresas: ", current%imagenes_impresas
            print *, "Cantidad total de pasos en el sistema: ", current%pasos_en_sistema
            current => current%next
        end do
    end subroutine imprimir_lista_clientes_atendidos

end module lista_clientes_atendidos_module
