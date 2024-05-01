module lista_imagenes_m
    implicit none
    
    ! Definición del tipo de nodo
    type :: nodo_imagen_lista
        integer :: id_imagen, cantidad_capas
        logical :: hoja
        type(nodo_imagen_lista), pointer :: next => null()
    end type nodo_imagen_lista

    ! Definición del tipo de lista
    type, public :: lista_imagenes
        type(nodo_imagen_lista), pointer :: head => null()   ! Apunta al primer nodo
        contains
            procedure :: insertar_imagen_l
            procedure :: buscar_imagen_l
            procedure :: eliminar_imagen_l
            procedure :: tamanoLista
    end type lista_imagenes
    
contains

    ! Agregar una ventanilla a la lista
subroutine insertar_imagen_l(this, id_imagen, cantidad_capas)
    class(lista_imagenes), intent(inout) :: this
    integer, intent(in) :: id_imagen, cantidad_capas

    ! Crear un nuevo nodo
    type(nodo_imagen_lista), pointer :: new_node
    type(nodo_imagen_lista), pointer :: current


    !primero ver si ya existe el cliente

    

    allocate(new_node)

    ! Asignar valores al nuevo nodo
    new_node%id_imagen = id_imagen
    new_node%cantidad_capas = cantidad_capas

    ! Si la lista está vacía, el nuevo nodo será la cabeza
    if (.not. associated(this%head)) then
        this%head => new_node
    else
        ! Si la lista no está vacía, agregar el nuevo nodo al final de la lista
        current => this%head
        do while (associated(current%next))
            current => current%next
        end do
        current%next => new_node
    end if


end subroutine insertar_imagen_l

function buscar_imagen_l(this, id_imagen) result(res)
    class(lista_imagenes), intent(in) :: this
    integer, intent(in) :: id_imagen
    type(nodo_imagen_lista), pointer :: current
    logical :: res


    res = .false.
    current => this%head
    do while (associated(current))
        if (current%id_imagen == id_imagen) then
            res = .true.
            exit
        end if
        current => current%next
    end do

end function buscar_imagen_l

subroutine eliminar_imagen_l(this, id_imagen)
    class(lista_imagenes), intent(inout) :: this
    integer, intent(in) :: id_imagen
    type(nodo_imagen_lista), pointer :: current
    type(nodo_imagen_lista), pointer :: prev

    current => this%head
    prev => null()
    do while (associated(current))
        if (current%id_imagen == id_imagen) then
            if (associated(prev)) then
                prev%next => current%next
            else
                this%head => current%next
            end if
            deallocate(current)
            return
        end if
        prev => current
        current => current%next
    end do


end subroutine eliminar_imagen_l

function tamanoLista(this) result(res)
    class(lista_imagenes), intent(in) :: this
    type(nodo_imagen_lista), pointer :: current
    integer :: res

    current => this%head
    res = 0
    do while (associated(current))
        res = res + 1
        current => current%next
    end do

end function tamanoLista




end module lista_imagenes_m
