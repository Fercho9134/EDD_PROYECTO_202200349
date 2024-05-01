module grafo_m
    implicit none

    real, parameter :: inf = huge(0.0)

    type :: arista
        integer :: destino
        real :: distancia
        real :: impresoras
    end type arista

    

    type :: nodo
        integer :: id
        type(arista), allocatable :: aristas(:)
    end type nodo

    type :: grafo
        type(nodo), allocatable :: nodos(:)
    contains
        procedure :: agregar_nodo
        procedure :: agregar_arista
        procedure :: dijkstra_menos_distancia
        procedure :: dijkstra_max_impresoras
        procedure :: graficar_grafo
        procedure :: existe_nodo
        procedure :: existe_arista

    end type grafo

    type  :: ResultadoRecorrido
        integer :: inicio
        integer :: fin
        real :: distancia
        real :: impresoras
        type(grafo) :: grafo
    end type ResultadoRecorrido

contains
    subroutine agregar_nodo(self, id)
        class(grafo), intent(inout) :: self
        integer, intent(in) :: id
        type(nodo) :: n

        n%id = id
        call extend_nodos(self%nodos, n)
    end subroutine agregar_nodo

    subroutine agregar_arista(self, origen, destino, distancia, impresoras)
        class(grafo), intent(inout) :: self
        integer, intent(in) :: origen, destino
        real, intent(in) :: distancia
        real, intent(in) :: impresoras

        type(arista) :: a
        integer :: i

        a%destino = destino
        a%distancia = distancia
        a%impresoras = impresoras

        do i = 1, size(self%nodos)
            if (self%nodos(i)%id == origen) then
                call extend_aristas(self%nodos(i)%aristas, a)
                exit
            end if
        end do
    end subroutine agregar_arista

    subroutine dijkstra_menos_distancia(self, inicio, fin, resultado)
        class(grafo), intent(inout) :: self
        integer, intent(in) :: inicio, fin
        type(ResultadoRecorrido), intent(out) :: resultado
        integer, allocatable :: prev(:), camino(:)
        real, allocatable :: dist(:), impresoras(:)
        logical, allocatable :: visitado(:)
        integer :: i, u, v, n, contador_camino
        real :: min_dist
        character(len=10) :: str_aux, str_aux2, str_aux3, str_aux4
        character(len=:), allocatable :: nombre_archivo

        nombre_archivo = "CaminoMenosDistanciaV1"
    
        allocate(prev(size(self%nodos)))
        allocate(dist(size(self%nodos)))
        allocate(impresoras(size(self%nodos)))
        allocate(visitado(size(self%nodos)))
        allocate(camino(size(self%nodos)))  ! Arreglo para almacenar el camino
        dist = inf
        visitado = .false.
        dist(inicio) = 0.0
        impresoras = 0.0
        contador_camino = 0  ! Inicializa el contador del camino
    
        do
            u = 0
            min_dist = inf
            do i = 1, size(self%nodos)
                if (.not. visitado(i) .and. dist(i) < min_dist) then
                    min_dist = dist(i)
                    u = i
                end if
            end do
    
            if (u == 0 .or. u == fin) exit
    
            visitado(u) = .true.
            if(.not. allocated(self%nodos(u)%aristas)) cycle
            do i = 1, size(self%nodos(u)%aristas)
                v = self%nodos(u)%aristas(i)%destino
                if (.not. visitado(v) .and. dist(u) + self%nodos(u)%aristas(i)%distancia < dist(v)) then
                    dist(v) = dist(u) + self%nodos(u)%aristas(i)%distancia
                    impresoras(v) = impresoras(u) + self%nodos(u)%aristas(i)%impresoras
                    prev(v) = u
                end if
            end do
        end do
    
        n = fin
    
        do while(n /= inicio)
            contador_camino = contador_camino + 1  ! Incrementa el contador del camino
            camino(contador_camino) = n  ! Almacena el nodo en el camino
            n = prev(n)
        end do
    
        contador_camino = contador_camino + 1  ! Incrementa el contador del camino
        camino(contador_camino) = inicio  ! Almacena el nodo inicial en el camino
    
        resultado%inicio = inicio
        resultado%fin = fin
        resultado%distancia = dist(fin)
        resultado%impresoras = impresoras(fin)

        write(str_aux, '(I10)') inicio
        write(str_aux2, '(I10)') fin
        write(str_aux3, '(F10.2)') resultado%distancia
        write(str_aux4, '(F10.2)') resultado%impresoras

        print *, ""
        print *, "La distancia mas corta desde "// trim(adjustl(str_aux)) //" hasta "// trim(adjustl(str_aux2)) //" es "&
        // trim(adjustl(str_aux3))//" y tiene "//trim(adjustl(str_aux4))//" impresoras. El camino se muestra a continuacion."

        print *, "Camino (Inicio -> Fin):"
        do i = contador_camino, 1, -1
            print *, "Sucursal: ", camino(i)  ! Imprime los nodos del camino en orden inverso
            !Insertamos nodos
            call resultado%grafo%agregar_nodo(camino(i))
        end do
        print *, "Fin"
        print *, ""

        !Insertamos aristas
        do i= 1, contador_camino - 1
            call resultado%grafo%agregar_arista(camino(i), camino(i + 1),&
             buscarDistanciaAristas(self,camino(i+1), camino(i)), &
             buscarImpresorasAristas(self,camino(i+1), camino(i)))
        end do

        call resultado%grafo%graficar_grafo(nombre_archivo)
    
        
    end subroutine dijkstra_menos_distancia
    

    subroutine graficar_grafo(self, nombre_archivo)
        class(grafo), intent(inout) :: self
        character(len=:), allocatable :: nombre_archivo
        type(arista) :: a
        type(nodo) :: n


        integer :: io, i, j
        character(len=100) :: comando
        character(len=20) :: nombre_origen, nombre_destino
        character(len=10) :: str_aux
        real :: distancia_redondeada
        real :: impresoras_redondeada

        io = 1
        comando = "dot -Tpng ./"//trim(adjustl(nombre_archivo))//".dot -o ./"//trim(adjustl(nombre_archivo))//".png"
        open(newunit=io, file='./'//trim(adjustl(nombre_archivo))//'.dot')
        write(io, *) 'digraph g {'
        do i = 1, size(self%nodos)
            !Agregamos los nodos
            write(str_aux, '(I10)') self%nodos(i)%id
            nombre_origen = 'NODO' // trim(adjustl(str_aux))
            write(io, *) nombre_origen, ' [label="', self%nodos(i)%id, '"]'

        end do



        !=Agregamos las aristas
        do i = 1, size(self%nodos)
            n = self%nodos(i)
            write(str_aux, '(I10)') n%id
            nombre_origen = 'NODO' // trim(adjustl(str_aux))
            if (.not. allocated(n%aristas)) cycle
            do j = 1, size(n%aristas)
                a = n%aristas(j)
                write(str_aux, '(I10)') a%destino
                nombre_destino = 'NODO' // trim(adjustl(str_aux))
                distancia_redondeada = nint(a%distancia)
                impresoras_redondeada = nint(a%impresoras)
                write(io, *) nombre_origen, ' -> ', nombre_destino, ' [label="Distancia: ', distancia_redondeada, &
                '\nImpresoras: ', impresoras_redondeada, '"]'
            end do
        end do


        write(io, *) '}'
        close(io)

        call execute_command_line(comando, exitstat=io)

        if (io /= 0) then
            print *, "Error al ejecutar el comando"
        else 
            print *, "Grafo generado correctamente"
        end if
    end subroutine graficar_grafo

    subroutine extend_nodos(arr, n)
        type(nodo), allocatable, intent(inout) :: arr(:)
        type(nodo), intent(in) :: n
        type(nodo), allocatable :: temp(:)
        if (.not. allocated(arr)) then
            allocate(arr(1))
        else
            
            allocate(temp(size(arr) + 1))
            temp(1:size(arr)) = arr
            call move_alloc(temp, arr)
        end if
        arr(size(arr)) = n
    end subroutine extend_nodos
    
    subroutine extend_aristas(arr, a)
        type(arista), allocatable, intent(inout) :: arr(:)
        type(arista), intent(in) :: a
        type(arista), allocatable :: temp(:)
    
        if (.not. allocated(arr)) then
            allocate(arr(1))
        else
            allocate(temp(size(arr) + 1))
            temp(1:size(arr)) = arr
            call move_alloc(temp, arr)
        end if
        arr(size(arr)) = a
    end subroutine extend_aristas

    subroutine dijkstra_max_impresoras(self, inicio, fin, resultado)
        class(grafo), intent(inout) :: self
        integer, intent(in) :: inicio, fin
        type(ResultadoRecorrido), intent(inout):: resultado
        integer, allocatable :: prev(:), camino(:)
        real, allocatable :: impresoras(:), dist(:)
        logical, allocatable :: visitado(:)
        integer :: i, u, v, n, contador_camino
        real :: max_impresoras
        character(len=10) :: str_aux, str_aux2, str_aux3, str_aux4
        character(len=:), allocatable :: nombre_archivo

        nombre_archivo = "CaminoMaxImpresorasV1"
    
        allocate(prev(size(self%nodos)))
        allocate(impresoras(size(self%nodos)))
        allocate(visitado(size(self%nodos)))
        allocate(dist(size(self%nodos)))
        allocate(camino(size(self%nodos)))  ! Arreglo para almacenar el camino
        impresoras = -inf
        visitado = .false.
        impresoras(inicio) = 0.0
        dist = 0.0
        contador_camino = 0  ! Inicializa el contador del camino
    
        do
            u = 0
            max_impresoras = -inf
            do i = 1, size(self%nodos)
                if (.not. visitado(i) .and. impresoras(i) > max_impresoras) then
                    max_impresoras = impresoras(i)
                    u = i
                end if
            end do
    
            if (u == 0 .or. u == fin) exit
    
            visitado(u) = .true.
            if(.not. allocated(self%nodos(u)%aristas)) cycle
            do i = 1, size(self%nodos(u)%aristas)
                v = self%nodos(u)%aristas(i)%destino
                if (.not. visitado(v) .and. impresoras(u) + self%nodos(u)%aristas(i)%impresoras > impresoras(v)) then
                    impresoras(v) = impresoras(u) + self%nodos(u)%aristas(i)%impresoras
                    dist(v) = dist(u) + self%nodos(u)%aristas(i)%distancia
                    prev(v) = u
                end if
            end do
        end do
    
        n = fin
    
        do while(n /= inicio)
            contador_camino = contador_camino + 1  ! Incrementa el contador del camino
            camino(contador_camino) = n  ! Almacena el nodo en el camino
            n = prev(n)
        end do
    
        contador_camino = contador_camino + 1  ! Incrementa el contador del camino
        camino(contador_camino) = inicio  ! Almacena el nodo inicial en el camino

        resultado%inicio = inicio
        resultado%fin = fin
        resultado%distancia = dist(fin)
        resultado%impresoras = impresoras(fin)
        write(str_aux, '(I10)') inicio
        write(str_aux2, '(I10)') fin
        write(str_aux3, '(F10.2)') resultado%distancia
        write(str_aux4, '(F10.2)') resultado%impresoras
        print *, ""
        print *, "La mayor cantidad de impresoras desde "// trim(adjustl(str_aux)) //" hasta "&
        // trim(adjustl(str_aux2)) //" es "// trim(adjustl(str_aux4))
        print *, "Y tiene "//trim(adjustl(str_aux3))//" de distancia. El camino se muestra a continuacion."
    
        print *, "Camino (Inicio -> Fin):"
        do i = contador_camino, 1, -1
            print *, "Sucursal: ", camino(i)  ! Imprime los nodos del camino en orden inverso
            !Insertamos nodos
            call resultado%grafo%agregar_nodo(camino(i))
        end do
        print *, "Fin"
        print *, ""

        !Insertamos aristas
        do i= 1, contador_camino - 1
            call resultado%grafo%agregar_arista(camino(i), camino(i + 1),&
             buscarDistanciaAristas(self,camino(i+1), camino(i)), &
             buscarImpresorasAristas(self,camino(i+1), camino(i)))
        end do

        call resultado%grafo%graficar_grafo(nombre_archivo)

    end subroutine dijkstra_max_impresoras

    function buscarDistanciaAristas(self, inicio, fin) result(res)
        type(grafo), intent(in) :: self
        integer, intent(in) :: inicio, fin
        real :: res
        integer :: i, j

        if (.not. allocated(self%nodos)) then
            res = inf
            print *, "No hay nodos"
            return
        end if
        do i = 1, size(self%nodos)
            if (self%nodos(i)%id == inicio) then
                if (.not. allocated(self%nodos(i)%aristas)) then
                    res = inf
                    print *, "No hay aristas"
                    return
                end if
                do j = 1, size(self%nodos(i)%aristas)
                    if (self%nodos(i)%aristas(j)%destino == fin) then
                        res = self%nodos(i)%aristas(j)%distancia
                        return
                    end if
                end do
            end if
        end do

        res = inf
    end function buscarDistanciaAristas

    function buscarImpresorasAristas(self, inicio, fin) result(res)
        type(grafo), intent(in) :: self
        integer, intent(in) :: inicio, fin
        real :: res
        integer :: i, j

        if (.not. allocated(self%nodos)) then
            res = inf
            return
        end if
        do i = 1, size(self%nodos)
            if (self%nodos(i)%id == inicio) then
                if (.not. allocated(self%nodos(i)%aristas)) then
                    res = inf
                    return
                end if
                do j = 1, size(self%nodos(i)%aristas)
                    if (self%nodos(i)%aristas(j)%destino == fin) then
                        res = self%nodos(i)%aristas(j)%impresoras
                        return
                    end if
                end do
            end if
        end do

        res = inf
    end function buscarImpresorasAristas
    
    

function existe_nodo(self, id) result(res)
    class(grafo), intent(in) :: self
    integer, intent(in) :: id
    logical :: res
    integer :: i

    if (.not. allocated(self%nodos)) then
        res = .false.
        return
    end if
    do i = 1, size(self%nodos)
        if (self%nodos(i)%id == id) then
            res = .true.
            return
        end if
    end do

    res = .false.
end function existe_nodo

function existe_arista(self, origen, destino) result(res)
    class(grafo), intent(in) :: self
    integer, intent(in) :: origen, destino
    logical :: res
    integer :: i, j

    if (.not. allocated(self%nodos)) then
        res = .false.
        return
    end if
    do i = 1, size(self%nodos)
        if (self%nodos(i)%id == origen) then
            if (.not. allocated(self%nodos(i)%aristas)) then
                res = .false.
                return
            end if
            do j = 1, size(self%nodos(i)%aristas)
                if (self%nodos(i)%aristas(j)%destino == destino) then
                    res = .true.
                    return
                end if
            end do
        end if
    end do

    res = .false.
end function existe_arista
    
end module grafo_m
