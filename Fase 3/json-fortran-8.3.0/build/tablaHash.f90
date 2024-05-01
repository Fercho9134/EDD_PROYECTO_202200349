module tabla_hash_m
    implicit none
    integer :: M = 7
    integer, parameter :: long = selected_int_kind(8)
    integer, parameter :: dp = selected_real_kind(15)

    type :: Persona
    integer(kind = 8) :: DPI
    character(len=:), allocatable :: Nombres
    character(len=:), allocatable :: Apellidos
    character(len=:), allocatable :: Genero
    character(len=:), allocatable :: Direccion
    integer :: Telefono
    integer :: trabajos_realizados = 0
    end type Persona


    type, public :: TablaHash
        integer :: n = 0
        type(Persona), allocatable :: tabla(:)

    contains
        procedure :: insert
        procedure :: print
        procedure :: search
        procedure, private :: pruebaDobleDispersion
        procedure :: graficar
        procedure :: incrementar_trabajos_realizados
        procedure :: search_graficar
        procedure :: generarReporteTecnicos
    end type TablaHash

contains
    subroutine insert(self, personal)
        class(TablaHash), intent(inout) :: self
        type(TablaHash) :: ret
        type(Persona), intent(in) :: personal
        type(Persona), allocatable :: temp(:)
        integer ::  i, i2
        integer (kind = 8) :: pos

        if(.not. allocated(self%tabla)) then
            allocate(self%tabla(0:M-1))
            self%tabla(:)%DPI = -1
        end if

        pos = funcion_hash(personal%DPI)

        if(self%tabla(pos)%DPI /= -1 .and. self%tabla(pos)%DPI /= personal%DPI) then
            do i = 1, M
                i2 = i
                call self%pruebaDobleDispersion(pos, i2)
                if(self%tabla(pos)%DPI == -1) exit
            end do
        end if

        self%tabla(pos) = personal
        self%n = self%n + 1

        if(self%n * 1.0_dp/M > 0.70) then
            temp = self%tabla
            deallocate(self%tabla)
            ret = rehashing(temp)
            self%tabla = ret%tabla
            self%n = ret%n
        end if
    end subroutine

    function rehashing(temp) result(val)
        type(Persona), intent(in) :: temp(:)
        integer :: i
        type(TablaHash) :: val

        M = M*2
        allocate(val%tabla(0:M-1))
        val%tabla(:)%DPI = -1

        do i = 1, size(temp)
            if(temp(i)%DPI /= -1) then
                call val%insert(temp(i))
            end if
        end do
    end function

    subroutine pruebaDobleDispersion(self, llv, i)
        class (TablaHash), intent(inout) :: self
        integer (kind = 8), intent(inout) :: llv
        integer, intent(inout) :: i

        llv = mod(llv, 7) + 1 * i
        llv = mod(llv, M)
    end subroutine pruebaDobleDispersion

    subroutine search(self, dpi)
        class(TablaHash), intent(inout) :: self
        integer(kind = 8), intent(in) :: dpi
        logical :: encontrado
        integer :: i, i2
        integer(kind = 8) :: pos

        encontrado = .false.

        pos = funcion_hash(dpi)
        
        if(self%tabla(pos)%DPI == dpi) then
            print *, "-------Persona Encontrada-------"
            print *, "DPI: ", self%tabla(pos)%DPI
            print *, "Nombres: ", trim(self%tabla(pos)%Nombres)
            print *, "Apellidos: ", trim(self%tabla(pos)%Apellidos)
            print *, "Genero: ", trim(self%tabla(pos)%Genero)
            print *, "Direccion: ", trim(self%tabla(pos)%Direccion)
            print *, "Telefono: ", self%tabla(pos)%Telefono
            print *, "---------------------------------"
            encontrado = .true.
        else
            do i = 1, M
                i2 = i
                call self%pruebaDobleDispersion(pos, i2)
                if(self%tabla(pos)%DPI == dpi) then
                    print *, "-------Persona Encontrada-------"
                    print *, "DPI: ", self%tabla(pos)%DPI
                    print *, "Nombres: ", trim(self%tabla(pos)%Nombres)
                    print *, "Apellidos: ", trim(self%tabla(pos)%Apellidos)
                    print *, "Genero: ", trim(self%tabla(pos)%Genero)
                    print *, "Direccion: ", trim(self%tabla(pos)%Direccion)
                    print *, "Telefono: ", self%tabla(pos)%Telefono
                    print *, "---------------------------------"
                    encontrado = .true.
                    exit
                end if
            end do
        end if

        if(.not. encontrado) then
            print *, "La persona con el DPI: ", dpi, " no se encuentra en la tabla"
        end if

    end subroutine search

    subroutine search_graficar(self, dpi)
        !Funcion que buscara a la persona con el DPI dado y graficara en un nodo
        !Los datos de la persona encontrada
        class(TablaHash), intent(inout) :: self
        integer(kind = 8), intent(in) :: dpi
        logical :: encontrado
        integer :: i, i2
        integer(kind = 8) :: pos
        integer :: io
        character(len=100) :: comando
        character(len=100) :: str

        io = 1
        encontrado = .false.

        pos = funcion_hash(dpi)

        if(self%tabla(pos)%DPI == dpi) then
            !Persona encontrada
            open(unit=io, file="./persona.dot", status="unknown")
            write(io, *) "digraph G {"
            write(io, *) 'NodoTabla [ label = <<TABLE border="1" cellspacing="0" cellpadding="4">'
            write(io, *) '<TR>'
            write(io, *) '<TD bgcolor="lightblue"><B> DPI </B></TD>'
            write(io, *) '<TD bgcolor="lightblue"><B> Nombres </B></TD>'
            write(io, *) '<TD bgcolor="lightblue"><B> Apellidos </B></TD>'
            write(io, *) '<TD bgcolor="lightblue"><B> Genero </B></TD>'
            write(io, *) '<TD bgcolor="lightblue"><B> Direccion </B></TD>'
            write(io, *) '<TD bgcolor="lightblue"><B> Telefono </B></TD>'
            write(io, *) '</TR>'
            write(str, '(I100)') self%tabla(pos)%DPI
            write(io, *) '<TR>'
            write(io, *) '<TD>', trim(adjustl(str)), '</TD>'
            write(io, *) '<TD>', trim(self%tabla(pos)%Nombres), '</TD>'
            write(io, *) '<TD>', trim(self%tabla(pos)%Apellidos), '</TD>'
            write(io, *) '<TD>', trim(self%tabla(pos)%Genero), '</TD>'
            write(io, *) '<TD>', trim(self%tabla(pos)%Direccion), '</TD>'
            write(str, '(I100)') self%tabla(pos)%Telefono
            write(io, *) '<TD>', trim(adjustl(str)), '</TD>'
            write(io, *) '</TR>'
            write(io, *) '</TABLE>> margin=0 shape=none]'
            write(io, *) "}"
            close(io)
            comando = "dot -Tpng ./persona.dot -o ./persona.png"
            call execute_command_line(comando)
            call system("start ./persona.png")
            encontrado = .true.
        else
            do i = 1, M
                i2 = i
                call self%pruebaDobleDispersion(pos, i2)
                if(self%tabla(pos)%DPI == dpi) then
                    !Persona encontrada
                    open(unit=io, file="./persona.dot", status="unknown")
                    write(io, *) "digraph G {"
                    write(io, *) 'NodoTabla [ label = <<TABLE border="1" cellspacing="0" cellpadding="4">'
                    write(io, *) '<TR>'
                    write(io, *) '<TD bgcolor="lightblue"><B> DPI </B></TD>'
                    write(io, *) '<TD bgcolor="lightblue"><B> Nombres </B></TD>'
                    write(io, *) '<TD bgcolor="lightblue"><B> Apellidos </B></TD>'
                    write(io, *) '<TD bgcolor="lightblue"><B> Genero </B></TD>'
                    write(io, *) '<TD bgcolor="lightblue"><B> Direccion </B></TD>'
                    write(io, *) '<TD bgcolor="lightblue"><B> Telefono </B></TD>'
                    write(io, *) '</TR>'
                    write(str, '(I100)') self%tabla(pos)%DPI
                    write(io, *) '<TR>'
                    write(io, *) '<TD>', trim(adjustl(str)), '</TD>'
                    write(io, *) '<TD>', trim(self%tabla(pos)%Nombres), '</TD>'
                    write(io, *) '<TD>', trim(self%tabla(pos)%Apellidos), '</TD>'
                    write(io, *) '<TD>', trim(self%tabla(pos)%Genero), '</TD>'
                    write(io, *) '<TD>', trim(self%tabla(pos)%Direccion), '</TD>'
                    write(str, '(I100)') self%tabla(pos)%Telefono
                    write(io, *) '<TD>', trim(adjustl(str)), '</TD>'
                    write(io, *) '</TR>'
                    write(io, *) '</TABLE>> margin=0 shape=none]'
                    write(io, *) "}"
                    close(io)
                    comando = "dot -Tpng ./persona.dot -o ./persona.png"
                    call execute_command_line(comando)
                    call system("start ./persona.png")
                    encontrado = .true.
                    exit
                end if
            end do
        end if

        if(.not. encontrado) then
            print *, "La persona con el DPI: ", dpi, " no se encuentra en la tabla"
        end if
    end subroutine search_graficar

    subroutine print(self)
        class(TablaHash), intent(in) :: self
        integer :: i, contador
        contador = 1
    
        do i = 0, M - 1
            if (self%tabla(i)%DPI /= -1) then
                print *, "Persona: ", contador
                print *, "DPI: ", self%tabla(i)%DPI
                print *, "Nombres: ", trim(self%tabla(i)%Nombres)
                print *, "Apellidos: ", trim(self%tabla(i)%Apellidos)
                print *, "Genero: ", trim(self%tabla(i)%Genero)
                print *, "Direccion: ", trim(self%tabla(i)%Direccion)
                print *, "Telefono: ", self%tabla(i)%Telefono
                print *, "---------------------------------"
                contador = contador + 1
            end if
        end do
    end subroutine print

    function funcion_hash(x) result(v)
        integer(kind = 8), intent(in) :: x
        integer(kind = 8) :: v

        v = mod(x, M)
    end function funcion_hash

    subroutine graficar(self)
        class(TablaHash), intent(in) :: self
        integer :: io, i
        character(len=100) :: comando
        character(len=100) :: str
    
        io = 1
    
        comando = "dot -Tpng ./tabla_hash.dot -o ./tabla_hash.png"
        open(unit=io, file="./tabla_hash.dot")
    
        write(io, *) "digraph G {"
        write(io, *) 'NodoTabla [ label = <<TABLE border="1" cellspacing="0" cellpadding="4">'
    
        ! Encabezado de la tabla
        ! Encabezado de la tabla
        write(io, *) '<TR>'
        write(io, *) '<TD bgcolor="lightblue"><B> DPI </B></TD>'
        write(io, *) '<TD bgcolor="lightblue"><B> Nombres </B></TD>'
        write(io, *) '<TD bgcolor="lightblue"><B> Apellidos </B></TD>'
        write(io, *) '<TD bgcolor="lightblue"><B> Genero </B></TD>'
        write(io, *) '<TD bgcolor="lightblue"><B> Direccion </B></TD>'
        write(io, *) '<TD bgcolor="lightblue"><B> Telefono </B></TD>'
        write(io, *) '</TR>'

        if(allocated(self%tabla)) then
        do i = 0, M - 1
            if (self%tabla(i)%DPI /= -1) then
                ! Escribir valores de la fila
                write(str, '(I100)') self%tabla(i)%DPI
                write(io, *) '<TR>'
                write(io, *) '<TD>', trim(adjustl(str)), '</TD>'
                write(io, *) '<TD>', trim(self%tabla(i)%Nombres), '</TD>'
                write(io, *) '<TD>', trim(self%tabla(i)%Apellidos), '</TD>'
                write(io, *) '<TD>', trim(self%tabla(i)%Genero), '</TD>'
                write(io, *) '<TD>', trim(self%tabla(i)%Direccion), '</TD>'
                write(str, '(I100)') self%tabla(i)%Telefono
                write(io, *) '<TD>', trim(adjustl(str)), '</TD>'
                write(io, *) '</TR>'
            end if
        end do
        end if
    
        write(io, *) '</TABLE>> margin=0 shape=none]'
    
        write(io, *) "}"
    
        close(io)
    
        call execute_command_line(comando, exitstat=i)
    
        if(i /= 0) then
            print *, "Error al generar la imagen de la tabla hash"
        else
            print *, "Imagen de la tabla hash generada con exito"
            call system("start ./tabla_hash.png")
        end if
    end subroutine graficar

    subroutine incrementar_trabajos_realizados(self, dpi)
        class(TablaHash), intent(inout) :: self
        integer(kind = 8), intent(in) :: dpi
        integer :: i, i2
        integer(kind = 8) :: pos

        pos = funcion_hash(dpi)
        
        if(self%tabla(pos)%DPI == dpi) then
            self%tabla(pos)%trabajos_realizados = self%tabla(pos)%trabajos_realizados + 1
        else
            do i = 1, M
                i2 = i
                call self%pruebaDobleDispersion(pos, i2)
                if(self%tabla(pos)%DPI == dpi) then
                    self%tabla(pos)%trabajos_realizados = self%tabla(pos)%trabajos_realizados + 1
                    exit
                end if
            end do
        end if
    end subroutine incrementar_trabajos_realizados

    subroutine generarReporteTecnicos(self)
        class(TablaHash), intent(in) :: self
        integer :: i, j, contador
        type(Persona), allocatable :: temp(:)
        type(Persona) :: aux

        contador = 0
        allocate(temp(size(self%tabla)))

        temp = self%tabla
        do i = 1, size(temp) -1
            do j = 1, size(temp) - i
                if(temp(j)%trabajos_realizados < temp(j+1)%trabajos_realizados) then
                    aux = temp(j)
                    temp(j) = temp(j+1)
                    temp(j+1) = aux
                end if
            end do
        end do

        !Devolveremos los 5 tecnicos con mas trabajos realizados
        !Si hay menos de 5 tecnicos, devolveremos todos
        !Para ello usaremos graphviz

        open(unit=1, file="./tecnicos_top.dot", status="unknown")

        write(1, *) "digraph G {"
        write(1, *) 'NodoTabla [ label = <<TABLE border="1" cellspacing="0" cellpadding="4">'
        write(1, *) '<TR>'
        write(1, *) '<TD bgcolor="lightblue"><B> DPI </B></TD>'
        write(1, *) '<TD bgcolor="lightblue"><B> Nombres </B></TD>'
        write(1, *) '<TD bgcolor="lightblue"><B> Apellidos </B></TD>'
        write(1, *) '<TD bgcolor="lightblue"><B> Genero </B></TD>'
        write(1, *) '<TD bgcolor="lightblue"><B> Direccion </B></TD>'
        write(1, *) '<TD bgcolor="lightblue"><B> Telefono </B></TD>'
        write(1, *) '<TD bgcolor="lightblue"><B> Trabajos Realizados </B></TD>'
        write(1, *) '</TR>'

        do i = 1, size(temp)
            if(contador <= 5) then
                if(temp(i)%DPI /= -1) then
                write(1, *) '<TR>'
                write(1, *) '<TD>', temp(i)%DPI, '</TD>'
                write(1, *) '<TD>', trim(temp(i)%Nombres), '</TD>'
                write(1, *) '<TD>', trim(temp(i)%Apellidos), '</TD>'
                write(1, *) '<TD>', trim(temp(i)%Genero), '</TD>'
                write(1, *) '<TD>', trim(temp(i)%Direccion), '</TD>'
                write(1, *) '<TD>', temp(i)%Telefono, '</TD>'
                write(1, *) '<TD>', temp(i)%trabajos_realizados, '</TD>'
                write(1, *) '</TR>'
                contador = contador + 1
                end if
            end if
        end do

        write(1, *) '</TABLE>> margin=0 shape=none ]'
        write(1, *) "}"

        close(1)

        call execute_command_line("dot -Tpng ./tecnicos_top.dot -o ./tecnicos_top.png")

        call system("start ./tecnicos_top.png")
    end subroutine generarReporteTecnicos
        
    
end module tabla_hash_m
