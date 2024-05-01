module lista_doble_circular_module
    use avl_m
    implicit none
    
    ! Definición del tipo de nodo para la lista enlazada simple
    type :: nodo_lista_enlazada_simple
        integer :: id_imagen
        !apuntador al nodo del avl
        type(nodo_avl), pointer :: imagen_avl => null()
        
        type(nodo_lista_enlazada_simple), pointer :: next => null()
    end type nodo_lista_enlazada_simple

    ! Definición del tipo de nodo para la lista doblemente enlazada circular
    type :: nodo_lista_doble_circular
        character(len=:), allocatable :: nombre_album
        integer :: cantidad_imagenes = 0
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
        procedure :: buscar_nodo_imagen
        procedure :: graficarListaAlbumes
        procedure :: contar_albumes
        procedure :: eliminarNodoListaSimple
    end type lista_doble_circular
    
contains

    ! Método para agregar un nodo a la lista circular doblemente enlazada
    subroutine agregar_nodo_lista_doble_circular(this, nombre_album)

        
        class(lista_doble_circular), intent(inout) :: this
        character(len=*), intent(in) :: nombre_album

        ! Crear un nuevo nodo para la lista doblemente enlazada circular
        type(nodo_lista_doble_circular), pointer :: new_node
        allocate(new_node)

        ! Asignar valores al nuevo nodo
        new_node%nombre_album = nombre_album

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
    subroutine agregar_nodo_lista_enlazada_simple(this, nodo_lista_doble, id_imagen)
        class(lista_doble_circular), intent(inout) :: this
        type(nodo_lista_doble_circular), pointer :: nodo_lista_doble
        integer, intent(in) :: id_imagen
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
        new_node%id_imagen = id_imagen
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
    

    function buscar_nodo_por_id(this, nombre_album) result(nodo)
        class(lista_doble_circular), intent(in) :: this
        character(len=:), allocatable, intent(in) :: nombre_album
        type(nodo_lista_doble_circular), pointer :: nodo
        character(len=:), allocatable :: nombre_primer_album
        
        logical :: encontrado
        type(nodo_lista_doble_circular), pointer :: current
    
        encontrado = .false.
        
        ! Verificar si la lista está vacía
        if (.not. associated(this%head)) then
            nodo => null()
            return
        end if
        
        ! Inicializar el nodo de inicio
        current => this%head
        nombre_primer_album = this%head%nombre_album
        
        ! Recorrer la lista doblemente enlazada circular
        do
            ! Verificar si el nodo actual contiene el id de cliente buscado
            if (current%nombre_album == nombre_album) then
                encontrado = .true.
                exit
            end if
            
            ! Mover al siguiente nodo en la lista
            current => current%next
            
            ! Verificar si hemos regresado al nodo inicial
            if (current%nombre_album == nombre_primer_album) then
                exit
            end if            
        end do
        
        ! Verificar si se encontró el nodo
        if (.not. encontrado) then
            nodo => null()
        else
            ! Asignar el nodo encontrado
            nodo => current
        end if
    end function buscar_nodo_por_id

    
    subroutine eliminar_nodo_lista_doble_circular(this, nombre_album, nodo_eliminado)
        class(lista_doble_circular), intent(inout) :: this
        character(len=:), allocatable, intent(in) :: nombre_album
        type(nodo_lista_doble_circular), pointer :: nodo_eliminado
        
        type(nodo_lista_doble_circular), pointer :: current, next_node
        logical :: encontrado

        character(len=:), allocatable :: nombre_album_cabecera
        character(len=:), allocatable :: nombre_album_cola
        
        encontrado = .false.
        
        ! Verificar si la lista está vacía
        if (.not. associated(this%head)) then
            print *, "La lista está vacía"
            nullify(nodo_eliminado)
            return
        end if
        
        ! Inicializar el nodo de inicio
        current => this%head

        nombre_album_cabecera = current%nombre_album
        nombre_album_cola = this%tail%nombre_album
        
        ! Recorrer la lista doblemente enlazada circular
        do
            ! Verificar si el nodo actual contiene el id de cliente buscado
            if (current%nombre_album == nombre_album) then
                encontrado = .true.
                exit
            end if
            
            ! Mover al siguiente nodo en la lista
            current => current%next
            
            ! Verificar si hemos regresado al nodo inicial
            if (current%nombre_album == nombre_album_cabecera) then
                exit
            end if            
        end do
        
        ! Verificar si se encontró el nodo
        if (.not. encontrado) then
            nullify(nodo_eliminado)
        else
            ! Eliminar el nodo encontrado de la lista
            nodo_eliminado => current
            next_node => current%next
            
            ! Enlazar los nodos adyacentes al nodo a eliminar
            if (current%nombre_album == this%head%nombre_album .and. &
            current%nombre_album == this%tail%nombre_album) then
                ! Caso especial: solo hay un nodo en la lista
                nullify(this%head)
                nullify(this%tail)
            else if (current%nombre_album == nombre_album_cabecera) then
                this%head => next_node
                next_node%prev => this%tail
                this%tail%next => this%head
            else if (current%nombre_album == nombre_album_cola) then
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
        character(len=:), allocatable :: nombre_album_cabecera
        
        ! Verificar si la lista está vacía
        if (.not. associated(this%head)) then
            return
        end if
        
        ! Inicializar el nodo de inicio
        current => this%head
        nombre_album_cabecera = current%nombre_album
        
        ! Recorrer la lista doblemente enlazada circular
        do while (associated(current))
            ! Imprimir los datos del nodo actual
    
            print *, "Nombre del album: ", current%nombre_album
            
            ! Imprimir las imágenes del cliente
            if (associated(current%lista_imagenes)) then
                print *, "Imágenes del cliente:"
                current_imagen => current%lista_imagenes
                do while (associated(current_imagen))
                    print *, "ID de la imagen: ", current_imagen%id_imagen
                    current_imagen => current_imagen%next
                end do
            end if
            
            ! Mover al siguiente nodo en la lista
            current => current%next
            
            ! Verificar si hemos regresado al nodo inicial
            if (current%nombre_album == nombre_album_cabecera) then
                exit
            end if
        end do

    end subroutine imprimir_lista_doble_circular

    function buscar_nodo_imagen(self, nodoListaDoble, id_imagen) result(nodo)
        class(lista_doble_circular), intent(in) :: self
        type(nodo_lista_doble_circular), pointer :: nodoListaDoble
        integer, intent(in) :: id_imagen
        type(nodo_lista_enlazada_simple), pointer :: nodo
        type(nodo_lista_enlazada_simple), pointer :: current
        logical :: encontrado
        
        encontrado = .false.
        
        ! Verificar si la lista está vacía
        if (.not. associated(nodoListaDoble%lista_imagenes)) then
            nodo => null()
            return
        end if
        
        ! Inicializar el nodo de inicio
        current => nodoListaDoble%lista_imagenes
        
        ! Recorrer la lista enlazada simple
        do while (associated(current))
            ! Verificar si el nodo actual contiene el id de imagen buscado
            if (current%id_imagen == id_imagen) then
                encontrado = .true.
                exit
            end if
            
            ! Mover al siguiente nodo en la lista
            current => current%next
        end do
        
        ! Verificar si se encontró el nodo
        if (.not. encontrado) then
            print *, "No se encontró ningún nodo con el id de imagen ingresado"
            nodo => null()
        else
            ! Asignar el nodo encontrado
            nodo => current
        end if
    end function buscar_nodo_imagen

    subroutine graficarListaAlbumes(self, io)
        class(lista_doble_circular), intent(in) :: self
        integer, intent(in) :: io
        type(nodo_lista_doble_circular), pointer :: nodo_lista_clientes_espera
        type(nodo_lista_enlazada_simple), pointer :: nodo_lista_simple_imagenes_espera
        integer :: primera_conexion, index
        character (len=100), allocatable :: comando
        character (len=:), allocatable :: conexiones
        character (len=:), allocatable :: primeros
        character (len=8) :: nombre, id_imagen_str
        character (len=8) :: nombre_imagen
        character (len=24) :: nombre_anterior 
        character (len=:), allocatable :: nombre_album_cabecera, nombre_primer_album
        character (len=:), allocatable :: nombre_imagen_anterior
        
        comando = "dot -Tpng -o lista_albumes.png lista_albumes.dot"
        open(unit=io, file="lista_albumes.dot")
        write(io, *) "digraph G {"

         !Lista de clientes en espera
        nodo_lista_clientes_espera => self%head
        if (associated(nodo_lista_clientes_espera)) then
            write(io, *) "label ="//'"Lista albumes"'
            write(io, *) "color=orange"
            !cambiamos tipo de letra
            write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightblue""]"
            primeros=""
            conexiones=""
            nombre_anterior=""
            primera_conexion = 0
            index = 0
            nombre_album_cabecera = self%head%nombre_album
            do while (associated(nodo_lista_clientes_espera))
                nodo_lista_simple_imagenes_espera => nodo_lista_clientes_espera%lista_imagenes
                write(nombre, "(I5)") index

                if (primeros == "") then
                    primeros = trim(nombre)
                end if

                write(io, *) '"Nodo'//trim(nombre)//'"[label = "'//(nodo_lista_clientes_espera%nombre_album)//'"]'

                !Si es el primer cliente guardamos su nombre
                if (nodo_lista_clientes_espera%nombre_album == nombre_album_cabecera) then
                    nombre_primer_album = trim(nombre)
                end if

                nombre_imagen_anterior = nombre
                !Graficamos la lista de imagenes de cada cliente
                if(associated(nodo_lista_simple_imagenes_espera)) then
                    do while(associated(nodo_lista_simple_imagenes_espera))
                        index = index + 1
                        write(nombre_imagen, "(I5)") index
                        write(id_imagen_str, "(I8)") nodo_lista_simple_imagenes_espera%id_imagen
                        write(io, *) '"Nodo'//trim(nombre_imagen)//'"[label = "'//(id_imagen_str)//'"]'
                        conexiones = conexiones//'"Nodo'//trim(nombre_imagen_anterior)//'" -> "Nodo'//trim(nombre_imagen)//'";'
                        !salto de linea
                        write(io, *) conexiones
                        conexiones = ""
                        nombre_imagen_anterior = nombre_imagen
                        nodo_lista_simple_imagenes_espera => nodo_lista_simple_imagenes_espera%next
                        index = index + 1
                    end do
                end if


                if(primera_conexion > 0) then
                    conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'";'
                    !conexion inversa
                    conexiones = conexiones//'"Nodo'//trim(nombre)//'" -> "Nodo'//trim(nombre_anterior)//'";'
                    write(io, *) conexiones
                    conexiones = ""
                end if


                nodo_lista_clientes_espera => nodo_lista_clientes_espera%next
                index = index + 1
                primera_conexion = primera_conexion + 1

                if(nodo_lista_clientes_espera%nombre_album == nombre_album_cabecera) then
                    conexiones = '"Nodo'//trim(nombre)//'" -> "Nodo'//trim(nombre_primer_album)//'";'
                    !salto de linea
                    write(io, *) conexiones
                    conexiones = ""
                    exit
                end if

                nombre_anterior = nombre

            end do

        end if
        write(io, *) "rankdir=LR"
        write(io, *) "}"

        close(unit=io)

        call execute_command_line(comando, exitstat=index)

        if (index /= 0) then
            print *, "Error al graficar la lista de albumes"
        else
            print *, "Lista de albumes graficada correctamente"
        end if
        



        

    end subroutine graficarListaAlbumes

    function contar_albumes(self) result(contador)
        class(lista_doble_circular), intent(in) :: self
        type(nodo_lista_doble_circular), pointer :: aux
        integer :: contador
        character (len=:), allocatable :: nombre_album_cabecera
        contador = 0
        if (associated(self%head)) then
            aux => self%head
            nombre_album_cabecera = self%head%nombre_album

            do while (associated(aux))
                contador = contador + 1
                aux => aux%next
                if(aux%nombre_album == nombre_album_cabecera) then
                    exit
                end if
            end do
        end if
    end function contar_albumes

    subroutine eliminarNodoListaSimple(self, id_imagen)
        class(lista_doble_circular), intent(inout) :: self
        integer, intent(in) :: id_imagen
        type(nodo_lista_doble_circular), pointer :: nodo_lista_doble
        type(nodo_lista_enlazada_simple), pointer :: nodo_lista_simple, nodo_anterior
        type(nodo_lista_enlazada_simple), pointer :: nodo_eliminado
        logical :: encontrado
        character (len=:), allocatable :: nombre_album_cabecera
        encontrado = .false.
        nodo_lista_doble => self%head
        if (associated(nodo_lista_doble)) then
            nombre_album_cabecera = nodo_lista_doble%nombre_album
            do while (associated(nodo_lista_doble))
                nodo_lista_simple => nodo_lista_doble%lista_imagenes
                if (associated(nodo_lista_simple)) then
                    nodo_anterior => null()
                    do while (associated(nodo_lista_simple))
                        if (nodo_lista_simple%id_imagen == id_imagen) then
                            encontrado = .true.
                            if (associated(nodo_anterior)) then
                                nodo_anterior%next => nodo_lista_simple%next
                            else
                                nodo_lista_doble%lista_imagenes => nodo_lista_simple%next
                            end if
                            nodo_eliminado => nodo_lista_simple
                        end if
                        nodo_anterior => nodo_lista_simple
                        nodo_lista_simple => nodo_lista_simple%next
                    end do
                end if
                nodo_lista_doble => nodo_lista_doble%next
                if (nodo_lista_doble%nombre_album == nombre_album_cabecera) then
                    exit
                end if
            end do
        end if
        if (encontrado) then
            deallocate(nodo_eliminado)
        end if
    end subroutine eliminarNodoListaSimple
    
end module lista_doble_circular_module
