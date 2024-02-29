module lista_clientes_atendidos_module
    implicit none
    
    ! Definición del tipo de nodo
    type :: nodo_cliente_atendido
        character(len=:), allocatable :: nombre_cliente
        integer :: ventanilla_atendida
        integer :: imagenes_impresas
        integer :: pasos_en_sistema
        integer :: imagenes_grandes
        integer :: imagenes_pequenas
        integer :: id_cliente
        integer :: size
        type(nodo_cliente_atendido), pointer :: next => null()
    end type nodo_cliente_atendido

    ! Definición de la lista de clientes atendidos
    type, public :: lista_clientes_atendidos
        type(nodo_cliente_atendido), pointer :: head => null()   ! Apunta al primer nodo
        type(nodo_cliente_atendido), pointer :: tail => null()    ! Apunta al último nodo

        contains
            procedure :: agregar_cliente_atendido
            procedure :: imprimir_lista_clientes_atendidos
            procedure :: devolver_tamano_lista
            procedure :: ordenar_lista_descendente_grandes
            procedure :: existe_cliente
            procedure :: ordenar_lista_ascendente_pequenas
            procedure :: devolver_cliente
            procedure :: cliente_con_mas_pasos
    end type lista_clientes_atendidos
    
contains

    ! Método para agregar un cliente atendido a la lista
    subroutine agregar_cliente_atendido(this,id_cliente, nombre_cliente, ventanilla_atendida, imagenes_impresas, pasos_en_sistema, &
                                        imagenes_grandes, imagenes_pequenas)
        class(lista_clientes_atendidos), intent(inout) :: this
        integer, intent(in) :: id_cliente
        character(len=*), intent(in) :: nombre_cliente
        integer, intent(in) :: ventanilla_atendida
        integer, intent(in) :: imagenes_impresas
        integer, intent(in) :: pasos_en_sistema
        integer, intent(in) :: imagenes_grandes
        integer, intent(in) :: imagenes_pequenas

        ! Crear un nuevo nodo
        type(nodo_cliente_atendido), pointer :: new_node
        allocate(new_node)

        ! Asignar valores al nuevo nodo
        new_node%id_cliente = id_cliente
        new_node%nombre_cliente = nombre_cliente
        new_node%ventanilla_atendida = ventanilla_atendida
        new_node%imagenes_impresas = imagenes_impresas
        new_node%pasos_en_sistema = pasos_en_sistema
        new_node%imagenes_grandes = imagenes_grandes
        new_node%imagenes_pequenas = imagenes_pequenas
        new_node%next => null()

        ! Agregar el nuevo nodo al final de la lista
        if (associated(this%head)) then
            this%tail%next => new_node
            new_node%next => null()
            this%tail => new_node
        else
            this%head => new_node
            this%tail => new_node
            new_node%next => null()
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

    ! Método para devolver el tamaño de la lista de clientes atendidos
    subroutine devolver_tamano_lista(this, tamano)
        class(lista_clientes_atendidos), intent(in) :: this
        integer, intent(out) :: tamano
        type(nodo_cliente_atendido), pointer :: current

        current => this%head
        tamano = 0
        do while (associated(current))
            tamano = tamano + 1
            current => current%next
        end do
    end subroutine devolver_tamano_lista

    !Metodo para ver si un cliente existe en la lista, asignara a la variable existe 
    !un 1 si el cliente existe y un 0 si no existe
    subroutine existe_cliente(this, id_cliente, existe)
        class(lista_clientes_atendidos), intent(in) :: this
        integer, intent(in) :: id_cliente
        integer, intent(out) :: existe
        type(nodo_cliente_atendido), pointer :: current

        current => this%head
        existe = 0
        do while (associated(current))
            if (current%id_cliente == id_cliente) then
                existe = 1
                return
            end if
            current => current%next
        end do
    end subroutine existe_cliente


    !Metodo que llenara la lista que se pasa como parametro con los 5 clientes
    !con mas imagenes grandes en la lista actual
    !Antes de agregar los clientes a la lista, se verifica que no existan ya en la lista
    subroutine ordenar_lista_descendente_grandes(this, lista_clientes)
        class(lista_clientes_atendidos), intent(in) :: this
        class(lista_clientes_atendidos), intent(inout) :: lista_clientes
        type(nodo_cliente_atendido), pointer :: current
        integer :: tamano_lista, i, j, id_cliente, existe
        integer :: imagenes_grandes, ventanilla_atendida, imagenes_impresas, pasos_en_sistema, imagenes_pequenas
        character(len=100) :: nombre_cliente

        call this%devolver_tamano_lista(tamano_lista)
        if (tamano_lista > 0) then
            do i = 1, 5
                current => this%head
                imagenes_grandes = -1
                do j = 1, tamano_lista
                    if (current%imagenes_grandes > imagenes_grandes) then
                        call lista_clientes%existe_cliente(current%id_cliente, existe)
                        if (existe == 0) then
                            id_cliente = current%id_cliente
                            nombre_cliente = current%nombre_cliente
                            ventanilla_atendida = current%ventanilla_atendida
                            imagenes_impresas = current%imagenes_impresas
                            pasos_en_sistema = current%pasos_en_sistema
                            imagenes_grandes = current%imagenes_grandes
                            imagenes_pequenas = current%imagenes_pequenas
                        end if
                    end if
                    current => current%next
                end do
                if (imagenes_grandes > -1) then
                    call lista_clientes%agregar_cliente_atendido(id_cliente, nombre_cliente, &
                    ventanilla_atendida, imagenes_impresas, &                                    
                    pasos_en_sistema, imagenes_grandes, imagenes_pequenas)
                end if
            end do
        end if
    end subroutine ordenar_lista_descendente_grandes
    
    
    !Metodo que llenara la lista que se pasa como parametro con los 5 clientes
    !con mas menos imagenes pequenas en la lista actual
    !Antes de agregar los clientes a la lista, se verifica que no existan ya en la lista 

    subroutine ordenar_lista_ascendente_pequenas(this, lista_clientes)
        class(lista_clientes_atendidos), intent(in) :: this
        class(lista_clientes_atendidos), intent(inout) :: lista_clientes
        type(nodo_cliente_atendido), pointer :: current
        integer :: tamano_lista, i, j, id_cliente, existe
        integer :: imagenes_grandes, ventanilla_atendida, imagenes_impresas, pasos_en_sistema, imagenes_pequenas
        character(len=100) :: nombre_cliente

        call this%devolver_tamano_lista(tamano_lista)
        if (tamano_lista > 0) then
            do i = 1, 5
                current => this%head
                imagenes_pequenas = 1000000
                do j = 1, tamano_lista
                    if (current%imagenes_pequenas < imagenes_pequenas) then
                        call lista_clientes%existe_cliente(current%id_cliente, existe)
                        if (existe == 0) then
                            id_cliente = current%id_cliente
                            nombre_cliente = current%nombre_cliente
                            ventanilla_atendida = current%ventanilla_atendida
                            imagenes_impresas = current%imagenes_impresas
                            pasos_en_sistema = current%pasos_en_sistema
                            imagenes_grandes = current%imagenes_grandes
                            imagenes_pequenas = current%imagenes_pequenas
                        end if
                    end if
                    current => current%next
                end do
                if (imagenes_pequenas /= 1000000) then
                    call lista_clientes%agregar_cliente_atendido(id_cliente, nombre_cliente, &
                    ventanilla_atendida, imagenes_impresas, &                                    
                    pasos_en_sistema, imagenes_grandes, imagenes_pequenas)
                end if
            end do
        end if
    end subroutine ordenar_lista_ascendente_pequenas

    !Metodo que devolverá al argumento nodo_cliente_atendido el cliente con el id que se pasa como parametro
    !Si el cliente no existe, se devolverá un apuntador nulo
    subroutine devolver_cliente(this, id_cliente, cliente)
        class(lista_clientes_atendidos), intent(in) :: this
        integer, intent(in) :: id_cliente
        type(nodo_cliente_atendido), pointer :: cliente
        type(nodo_cliente_atendido), pointer :: current

        current => this%head
        do while (associated(current))
            if (current%id_cliente == id_cliente) then
                cliente => current
                return
            end if
            current => current%next
        end do
        cliente => null()
    end subroutine devolver_cliente

    !metodo para obtener el cliente con mas pasos en el sistema
    subroutine cliente_con_mas_pasos(this, cliente)
        class(lista_clientes_atendidos), intent(in) :: this
        type(nodo_cliente_atendido), pointer :: cliente
        type(nodo_cliente_atendido), pointer :: current
        integer :: pasos, id_cliente

        current => this%head
        pasos = 0
        do while (associated(current))
            if (current%pasos_en_sistema > pasos) then
                pasos = current%pasos_en_sistema
                id_cliente = current%id_cliente
            end if
            current => current%next
        end do
        call this%devolver_cliente(id_cliente, cliente)
    end subroutine cliente_con_mas_pasos



end module lista_clientes_atendidos_module
