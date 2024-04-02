module lista_simple_ventanillas_modulo
    use pila_imagenes_module
    implicit none
    
    ! Definición del tipo de nodo
    type :: nodo_ventanilla
        integer :: numero_ventanilla
        integer :: id_cliente = -1 ! Inicializar id_cliente en -1
        character(len=:), allocatable :: nombre_cliente
        integer :: imagenes_pequenas
        integer :: imagenes_pequenas_original
        integer :: imagenes_grandes
        integer :: imagenes_grandes_original
        integer :: pasos_en_espera = 0
        type(Pila_Imagenes) :: pila_imagenes  ! Pila de imágenes asociada a la ventanilla
        type(nodo_ventanilla), pointer :: next => null()
    end type nodo_ventanilla

    ! Definición del tipo de lista
    type, public :: lista_simple_ventanillas
        type(nodo_ventanilla), pointer :: head => null()   ! Apunta al primer nodo
        contains
            procedure :: agregar_ventanilla
            procedure :: imprimir_lista
    end type lista_simple_ventanillas
    
contains

    ! Agregar una ventanilla a la lista
subroutine agregar_ventanilla(this, numero_ventanilla)
    class(lista_simple_ventanillas), intent(inout) :: this
    integer, intent(in) :: numero_ventanilla

    ! Crear un nuevo nodo
    type(nodo_ventanilla), pointer :: new_node
    type(nodo_ventanilla), pointer :: current

    allocate(new_node)

    ! Asignar valores al nuevo nodo
    new_node%numero_ventanilla = numero_ventanilla
    new_node%id_cliente = -1  ! Inicializar id_cliente en -1
    new_node%nombre_cliente = ""  ! Inicializar nombre_cliente
    new_node%pasos_en_espera = 0  ! Inicializar pasos_en_espera
    new_node%imagenes_pequenas = 0  ! Inicializar imagenes_pequenas
    new_node%imagenes_grandes = 0  ! Inicializar imagenes_grandes

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

end subroutine agregar_ventanilla



    ! Imprimir la lista de ventanillas
    subroutine imprimir_lista(this)
        class(lista_simple_ventanillas), intent(in) :: this
        type(nodo_ventanilla), pointer :: current

        current => this%head
        do while (associated(current))
            print *, "No Vent:",current%numero_ventanilla, "ID Cli:", current%id_cliente, &
            "Nombre Cli:", current%nombre_cliente, "Esperando: ", current%pasos_en_espera
            
            call current%pila_imagenes%imprimir_pila_imagenes()
            ! Puedes acceder a la pila de imágenes asociada a cada nodo de ventanilla usando current%pila_imagenes
            current => current%next
        end do
    end subroutine imprimir_lista


end module lista_simple_ventanillas_modulo
