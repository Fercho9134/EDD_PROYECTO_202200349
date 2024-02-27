module lista_doble_circular_module
    implicit none
    
    ! Definición del tipo de nodo para la lista enlazada simple
    type :: nodo_lista_enlazada_simple
        integer :: id_cliente
        character(len=:), allocatable :: tamano
        type(nodo_lista_enlazada_simple), pointer :: next => null()
    end type nodo_lista_enlazada_simple

    ! Definición del tipo de nodo para la lista doblemente enlazada circular
    type :: nodo_lista_doble_circular
        integer :: id_cliente
        character(len=:), allocatable :: nombre_cliente
        integer :: ventanilla_atendida
        integer :: pasos_esperados
        integer :: cantidad_imagenes_grandes
        integer :: cantidad_imagenes_pequenas
        integer :: cantidad_imagenes_grandes_original
        integer :: cantidad_imagenes_pequenas_original
        type(nodo_lista_doble_circular), pointer :: next => null()   ! Apunta al siguiente nodo
        type(nodo_lista_doble_circular), pointer :: prev => null()   ! Apunta al nodo anterior
        type(nodo_lista_enlazada_simple), pointer :: lista_imagenes => null()   ! Apuntador a la lista enlazada simple
    end type nodo_lista_doble_circular

    ! Definición de la lista circular doblemente enlazada de listas
    type, public :: lista_doble_circular
        type(nodo_lista_doble_circular), pointer :: head => null()   ! Apunta al primer nodo
        type(nodo_lista_doble_circular), pointer :: tail => null()   ! Apunta al último nodo (opcional
        contains
        procedure :: agregar_nodo_lista_doble_circular
        procedure :: agregar_nodo_lista_enlazada_simple
        procedure :: buscar_nodo_por_id
        procedure :: eliminar_nodo_lista_doble_circular
        procedure :: imprimir_lista_doble_circular

    end type lista_doble_circular
    
contains

    ! Método para agregar un nodo a la lista circular doblemente enlazada
    subroutine agregar_nodo_lista_doble_circular(this, id_cliente, nombre_cliente, &
        ventanilla_atendida, pasos_esperados, cantidad_imagenes_grandes_original, &
        cantidad_imagenes_pequenas_original)

        
        class(lista_doble_circular), intent(inout) :: this
        integer, intent(in) :: id_cliente
        character(len=*), intent(in) :: nombre_cliente
        integer, intent(in) :: ventanilla_atendida
        integer, intent(in) :: pasos_esperados
        integer, intent(in) :: cantidad_imagenes_grandes_original
        integer, intent(in) :: cantidad_imagenes_pequenas_original

        ! Crear un nuevo nodo para la lista doblemente enlazada circular
        type(nodo_lista_doble_circular), pointer :: new_node
        allocate(new_node)

        ! Asignar valores al nuevo nodo
        new_node%id_cliente = id_cliente
        new_node%nombre_cliente = nombre_cliente
        new_node%ventanilla_atendida = ventanilla_atendida
        new_node%pasos_esperados = pasos_esperados
        new_node%cantidad_imagenes_grandes = 0
        new_node%cantidad_imagenes_pequenas = 0
        new_node%cantidad_imagenes_grandes_original = cantidad_imagenes_grandes_original
        new_node%cantidad_imagenes_pequenas_original = cantidad_imagenes_pequenas_original

        ! Enlazar el nuevo nodo
        if (.not. associated(this%head)) then
            this%head => new_node
            this%tail => new_node
            this%head%next => this%head
            this%head%prev => this%tail
        else
            this%tail%next => new_node
            new_node%prev => this%tail
            new_node%next => this%head
            this%tail => new_node
            this%head%prev => this%tail
        end if

        ! Asignar el apuntador a la lista enlazada simple como null
        new_node%lista_imagenes => null()
    end subroutine agregar_nodo_lista_doble_circular

    ! Método para agregar un nodo a la lista enlazada simple
    subroutine agregar_nodo_lista_enlazada_simple(this, nodo_lista_doble, id_cliente, tamano)
        class(lista_doble_circular), intent(inout) :: this
        type(nodo_lista_doble_circular), pointer :: nodo_lista_doble
        integer, intent(in) :: id_cliente
        character(len=*), intent(in) :: tamano
        type(nodo_lista_enlazada_simple), pointer :: current
    
        ! Crear un nuevo nodo para la lista enlazada simple
        type(nodo_lista_enlazada_simple), pointer :: new_node
        allocate(new_node)

        !Verificamos que el nodo de la lista doblemente enlazada no sea nulo

        if (.not. associated(nodo_lista_doble)) then
            print *, "El nodo de la lista doblemente enlazada es nulo"
            return
        end if
    
        ! Asignar valores al nuevo nodo
        new_node%id_cliente = id_cliente
        allocate(new_node%tamano, source=tamano)
        new_node%next => null()  ! El siguiente del nuevo nodo siempre es null
    
        ! Verificar si la lista enlazada simple del nodo de la lista doblemente enlazada está vacía
        if (.not. associated(nodo_lista_doble%lista_imagenes)) then
            ! Si la lista está vacía, el nuevo nodo será el primer y único nodo en la lista
            nodo_lista_doble%lista_imagenes => new_node
        else
            ! Si la lista no está vacía, recorremos la lista hasta encontrar el último nodo
            current => nodo_lista_doble%lista_imagenes
            do while (associated(current%next))
                current => current%next
            end do
            ! Enlazamos el nuevo nodo al final de la lista
            current%next => new_node
        end if
    end subroutine agregar_nodo_lista_enlazada_simple
    

    subroutine buscar_nodo_por_id(this, id_cliente, nodo)
        class(lista_doble_circular), intent(in) :: this
        integer, intent(in) :: id_cliente
        type(nodo_lista_doble_circular), pointer :: nodo
        integer :: id_cliente_cabecera
        
        logical :: encontrado
        type(nodo_lista_doble_circular), pointer :: current
    
        encontrado = .false.
        id_cliente_cabecera = this%head%id_cliente
        
        ! Verificar si la lista está vacía
        if (.not. associated(this%head)) then
            print *, "La lista está vacía"
            nullify(nodo)
            return
        end if
        
        ! Inicializar el nodo de inicio
        current => this%head
        
        ! Recorrer la lista doblemente enlazada circular
        do
            ! Verificar si el nodo actual contiene el id de cliente buscado
            if (current%id_cliente == id_cliente) then
                encontrado = .true.
                exit
            end if
            
            ! Mover al siguiente nodo en la lista
            current => current%next
            
            ! Verificar si hemos regresado al nodo inicial
            if (current%id_cliente == id_cliente_cabecera) then
                exit
            end if            
        end do
        
        ! Verificar si se encontró el nodo
        if (.not. encontrado) then
            print *, "No se encontró ningún nodo con el id de cliente dado"
            nullify(nodo)
        else
            ! Asignar el nodo encontrado
            nodo => current
        end if
    end subroutine buscar_nodo_por_id

    
    subroutine eliminar_nodo_lista_doble_circular(this, id_cliente, nodo_eliminado)
        class(lista_doble_circular), intent(inout) :: this
        integer, intent(in) :: id_cliente
        type(nodo_lista_doble_circular), pointer :: nodo_eliminado
        
        type(nodo_lista_doble_circular), pointer :: current, next_node
        logical :: encontrado

        integer :: id_cliente_cabecera, id_cliente_cola
        
        encontrado = .false.
        
        ! Verificar si la lista está vacía
        if (.not. associated(this%head)) then
            print *, "La lista está vacía"
            nullify(nodo_eliminado)
            return
        end if
        
        ! Inicializar el nodo de inicio
        current => this%head

        id_cliente_cabecera = current%id_cliente
        id_cliente_cola = this%tail%id_cliente
        
        ! Recorrer la lista doblemente enlazada circular
        do
            ! Verificar si el nodo actual contiene el id de cliente buscado
            if (current%id_cliente == id_cliente) then
                encontrado = .true.
                exit
            end if
            
            ! Mover al siguiente nodo en la lista
            current => current%next
            
            ! Verificar si hemos regresado al nodo inicial
            if (current%id_cliente == id_cliente_cabecera) then
                exit
            end if            
        end do
        
        ! Verificar si se encontró el nodo
        if (.not. encontrado) then
            print *, "No se encontró ningún nodo con la ID del cliente dado"
            nullify(nodo_eliminado)
        else
            ! Eliminar el nodo encontrado de la lista
            nodo_eliminado => current
            next_node => current%next
            
            ! Enlazar los nodos adyacentes al nodo a eliminar
            if (current%id_cliente == this%head%id_cliente .and. &
            current%id_cliente == this%tail%id_cliente) then
                ! Caso especial: solo hay un nodo en la lista
                nullify(this%head)
                nullify(this%tail)
            else if (current%id_cliente == id_cliente_cabecera) then
                this%head => next_node
                next_node%prev => this%tail
                this%tail%next => this%head
            else if (current%id_cliente == id_cliente_cola) then
                this%tail => current%prev
                this%tail%next => this%head
                this%head%prev => this%tail
            else
                current%prev%next => next_node
                next_node%prev => current%prev
            end if
            
        end if
    end subroutine eliminar_nodo_lista_doble_circular
    
    


    subroutine imprimir_lista_doble_circular(this)
        class(lista_doble_circular), intent(in) :: this
        type(nodo_lista_doble_circular), pointer :: current
        type(nodo_lista_enlazada_simple), pointer :: current_imagen
        integer :: id_cliente_cabecera
        
        ! Verificar si la lista está vacía
        if (.not. associated(this%head)) then
            return
        end if
        
        ! Inicializar el nodo de inicio
        current => this%head
        id_cliente_cabecera = current%id_cliente
        
        ! Recorrer la lista doblemente enlazada circular
        do while (associated(current))
            ! Imprimir los datos del nodo actual
            print *, "ID del cliente: ", current%id_cliente
            print *, "Nombre del cliente: ", current%nombre_cliente
            print *, "Ventanilla atendida: ", current%ventanilla_atendida
            print *, "Pasos esperados: ", current%pasos_esperados
            
            ! Imprimir las imágenes del cliente
            if (associated(current%lista_imagenes)) then
                print *, "Imágenes del cliente:"
                current_imagen => current%lista_imagenes
                do while (associated(current_imagen))
                    print *, "ID de la imagen: ", current_imagen%id_cliente
                    print *, "Tamaño de la imagen: ", current_imagen%tamano
                    current_imagen => current_imagen%next
                end do
            end if
            
            ! Mover al siguiente nodo en la lista
            current => current%next
            
            ! Verificar si hemos regresado al nodo inicial
            if (current%id_cliente == id_cliente_cabecera) then
                exit
            end if
        end do

    end subroutine imprimir_lista_doble_circular
    
end module lista_doble_circular_module
