module arbolB_m
    use avl_m
    use lista_doble_circular_module
    implicit none
    
    ! Definición del tipo de nodo
    type :: nodo_cliente
        integer (kind=8) :: dpi
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: contrasena
        type(abb) :: arbolCapas
        type(avl) :: arbolImagenes
        type(lista_doble_circular) :: listaAlbumes
        type(nodo_cliente), pointer :: next => null()
    end type nodo_cliente

    ! Definición del tipo de lista
    type, public :: arbolB_clientes
        type(nodo_cliente), pointer :: head => null()   ! Apunta al primer nodo
        contains
            procedure :: insertar
            procedure :: buscar
            procedure :: eliminar
    end type arbolB_clientes
    
contains

    ! Agregar una ventanilla a la lista
subroutine insertar(this, dpi, nombre, contrasena)
    class(arbolB_clientes), intent(inout) :: this
    integer (kind=8), intent(in) :: dpi
    character(len=*), intent(in) :: nombre
    character(len=*), intent(in) :: contrasena

    ! Crear un nuevo nodo
    type(nodo_cliente), pointer :: new_node
    type(nodo_cliente), pointer :: current
    type(nodo_cliente), pointer :: res

    !primero ver si ya existe el cliente

    res => this%buscar(dpi)
    if (associated(res)) then
        print *, "Un cliente con DPI ", dpi, " ya existe. No se puede agregar."
        return
    end if
    

    allocate(new_node)

    ! Asignar valores al nuevo nodo
    new_node%dpi = dpi
    new_node%nombre = nombre
    new_node%contrasena = contrasena

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

    print *, "Cliente con DPI ", dpi, " agregado exitosamente"

end subroutine insertar

function buscar(this, dpi) result(res)
    class(arbolB_clientes), intent(in) :: this
    integer (kind=8), intent(in) :: dpi
    type(nodo_cliente), pointer :: current
    logical :: found
    type(nodo_cliente), pointer :: res

    res => null()

    found = .false.
    current => this%head
    do while (associated(current))
        if (current%dpi == dpi) then
            found = .true.
            res => current
            exit
        end if
        current => current%next
    end do

end function buscar

subroutine eliminar(this, dpi)
    class(arbolB_clientes), intent(inout) :: this
    integer (kind=8), intent(in) :: dpi
    type(nodo_cliente), pointer :: current
    type(nodo_cliente), pointer :: prev

    current => this%head
    prev => null()
    do while (associated(current))
        if (current%dpi == dpi) then
            if (associated(prev)) then
                prev%next => current%next
            else
                this%head => current%next
            end if
            deallocate(current)
            print *, "Cliente con DPI ", dpi, " eliminado exitosamente"
            return
        end if
        prev => current
        current => current%next
    end do

    print *, "Cliente con DPI ", dpi, " no encontrado"

end subroutine eliminar




end module arbolB_m
