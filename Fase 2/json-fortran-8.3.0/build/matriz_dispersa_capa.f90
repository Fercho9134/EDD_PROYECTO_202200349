module matrix_m
    implicit none
    type :: node_matriz_val
        logical :: exists = .false.
        character(len=7) :: valor
    end type node_matriz_val

    type :: node_matriz
        private
        integer :: i,j
        character(len=7) :: valor
        type(node_matriz), pointer :: arriba => null()
        type(node_matriz), pointer :: abajo => null()
        type(node_matriz), pointer :: derecha => null()
        type(node_matriz), pointer :: izquierda => null()
    end type node_matriz

    type, public :: matrix
        type(node_matriz), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
        integer :: id
    contains
        procedure :: insert
        procedure :: insertarCabeceraFila
        procedure :: insertarCabeceraColumna
        procedure :: buscarFila
        procedure :: buscarColumna
        procedure :: existeNodo
        procedure :: print
        procedure :: imprimirEncabezadoColumnas
        procedure :: graficar
        procedure :: obtenerValor
        procedure :: generarImagen
    end type matrix

contains
    subroutine insert(self, i, j, valor)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(len=7) :: valor

        type(node_matriz), pointer :: nuevo
        type(node_matriz), pointer :: fila
        type(node_matriz), pointer :: columna

        allocate(nuevo)
        nuevo = node_matriz(i=i, j=j, valor=valor)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node_matriz(i=-1, j=-1, valor=valor)
        end if

        fila => self%buscarFila(j)
        columna => self%buscarColumna(i)

        if(i > self%width) self%width = i
        if(j > self%height) self%height = j

        if(.not. self%existeNodo(nuevo)) then
            if(.not. associated(columna)) then
                columna => self%insertarCabeceraColumna(i)
            end if

            if(.not. associated(fila)) then
                fila => self%insertarCabeceraFila(j)
            end if
            call insertarEnColumna(nuevo, fila)
            call insertarEnFila(nuevo, columna)
        end if
    end subroutine insert

    function insertarCabeceraColumna(self, i) result(nuevaCabeceraColumna)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: i

        type(node_matriz), pointer :: nuevaCabeceraColumna
        allocate(nuevaCabeceraColumna)

        nuevaCabeceraColumna = node_matriz(i=i, j=-1, valor="")
        call insertarEnColumna(nuevaCabeceraColumna, self%root)
    end function insertarCabeceraColumna
    
    function insertarCabeceraFila(self, j) result(nuevaCabeceraFila)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: j

        type(node_matriz), pointer :: nuevaCabeceraFila
        allocate(nuevaCabeceraFila)

        nuevaCabeceraFila = node_matriz(i=-1, j=j, valor="")
        call insertarEnFila(nuevaCabeceraFila, self%root)
    end function insertarCabeceraFila

    subroutine insertarEnFila(nuevo, CabeceraFila)
        type(node_matriz), pointer :: nuevo
        type(node_matriz), pointer :: cabeceraFila !head 

        type(node_matriz), pointer :: actual
        actual => cabeceraFila

        do while(associated(actual%abajo))
            if(nuevo%j < actual%abajo%j) then

                nuevo%abajo => actual%abajo
                nuevo%arriba => actual
                actual%abajo%arriba => nuevo
                actual%abajo => nuevo
                exit
            end if
            actual => actual%abajo
        end do

        if(.not. associated(actual%abajo)) then
            actual%abajo => nuevo
            nuevo%arriba => actual
        end if
    end subroutine insertarEnFila

    subroutine insertarEnColumna(nuevo, CabeceraColumna)
        type(node_matriz), pointer :: nuevo
        type(node_matriz), pointer :: CabeceraColumna !head 

        type(node_matriz), pointer :: actual
        actual => CabeceraColumna

        do while(associated(actual%derecha))
            if(nuevo%i < actual%derecha%i) then

                nuevo%derecha => actual%derecha
                nuevo%izquierda => actual
                actual%derecha%izquierda => nuevo
                actual%derecha => nuevo
                exit
            end if
            actual => actual%derecha
        end do

        if(.not. associated(actual%derecha)) then
            actual%derecha => nuevo
            nuevo%izquierda => actual
        end if
    end subroutine insertarEnColumna  

    function buscarFila(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node_matriz), pointer :: actual
        actual => self%root

        do while(associated(actual)) 
            if(actual%j == j) return
            actual => actual%abajo
        end do
    end function buscarFila

    function buscarColumna(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node_matriz), pointer :: actual
        actual => self%root 
        
        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%derecha
        end do
    end function buscarColumna

    function existeNodo(self, nodo) result(existe)
        class(matrix), intent(inout) :: self
        type(node_matriz), pointer, intent(in) :: nodo

        logical :: existe
        type(node_matriz), pointer :: encabezadoFila
        type(node_matriz), pointer :: columna
        encabezadoFila => self%root
        existe = .false.

        do while(associated(encabezadoFila))
            if(encabezadoFila%j == nodo%j) then
                columna => encabezadoFila
                do while(associated(columna)) 
                    if(columna%i == nodo%i) then
                        columna%valor = nodo%valor
                        existe = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            encabezadoFila => encabezadoFila%abajo
        end do
        return
    end function existeNodo

    subroutine graficar(self)
        class(matrix), intent(in) :: self
        
        integer :: io
        integer :: i
        character(len=10) :: str_i
        character(len=10) :: str_j
        character(len=10) :: str_i_aux
        character(len=10) :: str_j_aux
        character(len=250) :: node_matriz_dec
        character(len=20) :: nombre

        character(len=100) :: comando
        character(len=100) :: contenido
        character(:), allocatable :: rank
        character(:), allocatable :: conexion
        character(:), allocatable :: conexionRev
        type(node_matriz), pointer :: fila_aux
        type(node_matriz), pointer :: columna_aux
        io = 1
        fila_aux => self%root
        comando = "dot -Tpng ./matrix_imagen.dot -o ./matrix_imagen.png"

        open(newunit=io, file="./matrix_imagen.dot")

        write(io, *) "digraph Matrix {"
        write(io, *) 'node[shape = box]'

        do while (associated(fila_aux))
            rank = "{rank=same"
            columna_aux => fila_aux
            do while(associated(columna_aux)) 
                write(str_i, '(I10)') columna_aux%i + 1
                write(str_j, '(I10)') columna_aux%j + 1
                nombre = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'

                if (columna_aux%i == -1 .and. columna_aux%j == -1) then
                    node_matriz_dec = trim(adjustl(nombre))//'[label = "root", group="'//trim(adjustl(str_i))//'"]'

                else if(columna_aux%i == -1) then
                    write(str_j_aux, '(I10)') columna_aux%j
                    contenido = trim(adjustl(str_j_aux))
                    node_matriz_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_matriz_dec = trim(adjustl(node_matriz_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                    
                else if(columna_aux%j == -1) then
                    write(str_i_aux, '(I10)') columna_aux%i
                    contenido = trim(adjustl(str_i_aux))
                    node_matriz_dec = trim(adjustl(nombre))//'[label="'//trim(adjustl(contenido))
                    node_matriz_dec = trim(adjustl(node_matriz_dec))//'",group="'//trim(adjustl(str_i))//'"]'
                        
                else
                    contenido = columna_aux%valor
                    node_matriz_dec = trim(adjustl(nombre))//'[label = "'//""//'", group="'//trim(adjustl(str_i))//'"'
                    node_matriz_dec = trim(adjustl(node_matriz_dec))//', style=filled, fillcolor="'//contenido
                    node_matriz_dec = trim(adjustl(node_matriz_dec))//'"]'
                end if
                write(io, *) node_matriz_dec

                if(associated(columna_aux%derecha)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'

                    write(str_i_aux, '(I10)') columna_aux%derecha%i + 1
                    write(str_j_aux, '(I10)') columna_aux%derecha%j + 1

                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if

                if(associated(columna_aux%abajo)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'

                    write(str_i_aux, '(I10)') columna_aux%abajo%i + 1
                    write(str_j_aux, '(I10)') columna_aux%abajo%j + 1

                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if

                rank = rank//';"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'
                columna_aux => columna_aux%derecha
            end do
            rank = rank//'}'
            write(io, *) rank

            fila_aux => fila_aux%abajo
        end do
        write(io, *) "}"
        close(io)
        
        call execute_command_line(comando, exitstat=i)

        if ( i == 1 ) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficar


    subroutine print(self)
        class(matrix), intent(in) :: self
        integer :: i
        integer :: j
        type(node_matriz), pointer :: aux
        type(node_matriz_val) :: val
        aux => self%root%abajo

        call self%imprimirEncabezadoColumnas()

        do j = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') j

            do i = 0, self%width
                val = self%obtenerValor(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%valor
                end if

            end do
        end do
    end subroutine print

    subroutine imprimirEncabezadoColumnas(self)
        class(matrix), intent(in) :: self
        integer :: i

        do i=-1, self%width
            write(*, fmt='(I3)', advance='no') i
        end do
    end subroutine imprimirEncabezadoColumnas


    function obtenerValor(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j

        type(node_matriz), pointer :: cabeceraFila
        type(node_matriz), pointer :: columna
        type(node_matriz_val) :: val
        cabeceraFila => self%root

        do while(associated(cabeceraFila))
            if(cabeceraFila%j == j) then
                columna => cabeceraFila
                do while(associated(columna)) 
                    if(columna%i == i) then
                        val%valor = columna%valor
                        val%exists = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            cabeceraFila => cabeceraFila%abajo
        end do
        val%exists = .false.
        return
    end function obtenerValor

    subroutine generarImagen(self, nombre)
        class(matrix), intent(in) :: self
        !Generamos con graphviz la vista de la matriz como una tabla html
        !renderizamos la tabla en un archivo png
        !la tabla debera ser de la cantidad de filas y columnas que tenga la matriz
        !El campo valor de nodo_matriz_val contiene un color en formato hexadecimal
        !para el fondo de la celda
        !si el nodo no existe, el color de la celda debera ser blanco
        !si el nodo existe, el color de la celda debera ser el valor del nodo
        !No incluir los encabezados de filas y columnas
        !No incluir el nodo root
        !El archivo generado debera ser ImagenGenerada.png
        !El archivo debera ser guardado en la carpeta del proyecto

        !Recordar que j es la fila y i es la columna
        character(:), allocatable, intent(in) :: nombre
        integer :: io
        integer :: i
        integer :: j
        type(node_matriz_val) :: val
        character(len=100) :: comando
        character(:), allocatable :: contenido

        character(len=256) :: nombre_a
        character(len=7) :: color_a, ancho_str

        write(nombre_a,'(A256)') nombre
        io = 1

        comando = "dot -Tpng ./ImagenGenerada.dot -o ./ImagenGenerada.png"

        open(newunit=io, file="./ImagenGenerada.dot")

        write(io, *) "digraph Matrix {"

        !Agregamos un nodo con el nombre de la matriz

        write(io, *) 'NodoTabla [ label = <<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">'

        do j = 0, self%height
            write(io, *) '<TR>'
            do i = 0, self%width
                val = self%obtenerValor(i,j)
                if(.not. val%exists) then
                    contenido = "#FFFFFF"
                    color_a = "#FFFFFF"
                else
                    contenido = val%valor
                    write(color_a, '(A7)') contenido
                end if
                
                write(io, *) '<TD WIDTH="50" HEIGHT="50" BGCOLOR="'//trim(color_a)//'"></TD>'
            end do
            write(io, *) '</TR>'
        end do

        !Agregamos una celda con el nombre de la matriz, con codesplan del ancho
        write(ancho_str, '(I7)') (self%width + 1)
        write(io,*) '<TR><TD COLSPAN="'//ancho_str//'" ALIGN="center"> '//trim(adjustl(nombre_a))//'</TD></TR>'

        write(io, *) '</TABLE>> margin=0 shape=none]'

        write(io, *) '}'
        close(io)

        call execute_command_line(comando, exitstat=i)

        if ( i == 1 ) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine generarImagen
end module matrix_m