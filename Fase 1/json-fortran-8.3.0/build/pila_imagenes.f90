module pila_imagenes_module
    implicit none
    
    ! Definición del tipo de nodo
    type :: nodo_imagenes
        integer :: id_cliente
        character(len=:), allocatable :: tamano
        type(nodo_imagenes), pointer :: next => null()
    end type nodo_imagenes

    ! Definición del tipo de pila
    type, public :: Pila_Imagenes
        type(nodo_imagenes), pointer :: top => null()   ! Apunta al nodo en la cima de la pila

        contains
            procedure :: apilar_imagen
            procedure :: desapilar_imagen
            procedure :: imprimir_pila_imagenes
    end type Pila_Imagenes
    
contains

    ! Apilar una imagen en la pila
    subroutine apilar_imagen(this, id_cliente, tamano)
        class(Pila_Imagenes), intent(inout) :: this
        integer, intent(in) :: id_cliente
        character(len=*), intent(in) :: tamano

        ! Crear un nuevo nodo
        type(nodo_imagenes), pointer :: new_node
        allocate(new_node)

        ! Asignar valores al nuevo nodo
        new_node%id_cliente = id_cliente
        allocate(new_node%tamano, source=tamano)
        new_node%next => this%top

        ! Actualizar la cima de la pila
        this%top => new_node
    end subroutine apilar_imagen

    ! Desapilar una imagen de la pila
    subroutine desapilar_imagen(this, id_cliente, tamano)
        class(Pila_Imagenes), intent(inout) :: this
        integer, intent(out) :: id_cliente
        character(len=:), allocatable, intent(out) :: tamano

        if (associated(this%top)) then
            id_cliente = this%top%id_cliente
            allocate(tamano, source=this%top%tamano)

            ! Mover la cima de la pila al siguiente nodo
            this%top => this%top%next

        else
            print *, "La pila de imágenes está vacía"
        end if
    end subroutine desapilar_imagen

    ! Imprimir la pila de imágenes
    subroutine imprimir_pila_imagenes(this)
        class(Pila_Imagenes), intent(in) :: this
        type(nodo_imagenes), pointer :: current

        current => this%top
        do while (associated(current))
            print *, "ID del cliente:", current%id_cliente, "Tamano imagen:", current%tamano
            current => current%next
        end do
    end subroutine imprimir_pila_imagenes

    

end module pila_imagenes_module
