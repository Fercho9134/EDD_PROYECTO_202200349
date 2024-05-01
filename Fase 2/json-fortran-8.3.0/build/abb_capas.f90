module abb_m
    !Arbol de capas global
    use uuid_module
    use tipo_nodo_abb_m
    use cola_niveles_m
    use lista_imagenes_m
    implicit none


    type, public :: abb
        type(nodo), pointer :: raiz => null()
    
    contains
        procedure :: insertarCapa
        procedure :: deleteCapa
        procedure :: preordenCapa
        procedure :: graficarCapa
        procedure :: buscarCapa
        procedure :: obtenerRecorridoPreorden
        procedure :: obtenerRecorridoInorden
        procedure :: obtenerRecorridoPostorden
        procedure :: obtenerRecorridoNiveles
        procedure :: obtenerNodoHoja
        procedure :: obtenerProfundidad
        procedure :: listarCapasOrdenes
        procedure :: contarCapas
    end type abb

    type nodo_texto
        character(:), allocatable :: respuesta
        type(matrix) :: matriz_general
    end type nodo_texto

contains
    subroutine insertarCapa(self, val)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val

        call insertRecCapa(self%raiz, val)
    end subroutine insertarCapa

    subroutine deleteCapa(self, val)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val

        self%raiz => deleteRecCapa(self%raiz, val)
    end subroutine deleteCapa

    subroutine preordenCapa(self)
        class(abb), intent(in) :: self
        
        call preordenRecCapa(self%raiz)
    end subroutine preordenCapa

    subroutine graficarCapa(self)
        class(abb), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./abb_capas.dot")
        comando = "dot -Tpng ./abb_capas.dot -o ./abb_capas.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%raiz)) then
            call imprimirRecCapa(self%raiz, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficarCapa

    recursive subroutine insertRecCapa(raiz, val)
        type(nodo), pointer, intent(inout) :: raiz
        integer, intent(in) :: val

        if(.not. associated(raiz)) then
            allocate(raiz)
            raiz = nodo(valor=val)
        
        else if(val < raiz%valor) then 
            call insertRecCapa(raiz%izquierda, val)

        else if(val > raiz%valor) then
            call insertRecCapa(raiz%derecha, val)
        else if (val == raiz%valor) then
            print *, "La capa con el id ", val, " ya existe en el arbol"
        end if
    end subroutine insertRecCapa

    recursive function deleteRecCapa(raiz, val) result(res)
        type(nodo), pointer :: raiz
        integer, intent(in) :: val

        type(nodo), pointer :: temp
        type(nodo), pointer :: res 
        
        if(.not. associated(raiz)) then
            res => raiz
            return
        end if

        if(val < raiz%valor) then
            raiz%izquierda => deleteRecCapa(raiz%izquierda, val)
        
        else if(val > raiz%valor) then
            raiz%derecha => deleteRecCapa(raiz%derecha, val)

        else
            if(.not. associated(raiz%izquierda)) then
                temp => raiz%derecha
                deallocate(raiz)
                res => temp
                return

            else if (.not. associated(raiz%derecha)) then
                temp => raiz%izquierda
                deallocate(raiz)
                res => temp
                return
            
            else
                call obtenerMayorDeMenoresCapa(raiz%izquierda, temp)
                raiz%valor = temp%valor
                raiz%izquierda => deleteRecCapa(raiz%izquierda, temp%valor)
            end if
        end if

        res => raiz
    end function deleteRecCapa

    recursive subroutine obtenerMayorDeMenoresCapa(raiz, mayor)
        type(nodo), pointer :: raiz, mayor
        if(associated(raiz%derecha)) then
            call obtenerMayorDeMenoresCapa(raiz%derecha, mayor)
        else
            mayor => raiz
        end if
    end subroutine obtenerMayorDeMenoresCapa

    recursive subroutine preordenRecCapa(raiz)
        type(nodo), pointer, intent(in) :: raiz

        if(associated(raiz)) then
            print *, raiz%valor
            call preordenRecCapa(raiz%izquierda)
            call preordenRecCapa(raiz%derecha)
        end if
    end subroutine preordenRecCapa

    recursive subroutine imprimirRecCapa(raiz, nombre, io)
        type(nodo), pointer, intent(in) :: raiz
        character(len=36), intent(in) :: nombre
        integer :: io

        character(len=36) :: derecha
        character(len=36) :: izquierda

        derecha = generate_uuid()
        izquierda = generate_uuid()

        if(associated(raiz)) then
            !"Nodo_uuid"[Label="1"]
            write(io, *) '"Nodo'//nombre//'"[label= "', raiz%valor, '"]'

            if(associated(raiz%izquierda)) then
                !"Nodo_uuid"->"Nodo_uuidHijoIzquierdo"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//izquierda//'"'
            end if

            if(associated(raiz%derecha)) then
                !"Nodo_uuid"->"Nodo_uuidHijoDerecho"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//derecha//'"'
            end if
            call imprimirRecCapa(raiz%izquierda, izquierda, io)
            call imprimirRecCapa(raiz%derecha, derecha, io)
        end if
    end subroutine imprimirRecCapa

    function buscarCapa(self, val) result(res)
        class(abb), intent(in) :: self
        integer, intent(in) :: val

        type(nodo), pointer :: temp
        type(nodo), pointer :: res

        temp => self%raiz

        if(associated(temp)) then
            do while(associated(temp))
                if(val < temp%valor) then
                    temp => temp%izquierda
                else if(val > temp%valor) then
                    temp => temp%derecha
                else
                    res => temp
                    return
                end if
            end do
        end if

        res => null()

    end function buscarCapa

    subroutine obtenerRecorridoPreorden(self, cantidad)
        class(abb), intent(in) :: self
        integer, intent(in) :: cantidad
        integer :: contador
        type(nodo_texto) :: nodo_respuesta
        character(len=256):: comando_abrir_imagen
    
        nodo_respuesta%respuesta = "Preorden: "
    
        ! Variable de control para contar los nodos visitados
        contador = 0
    
        ! Se realizara un recorrido preorden sobre el arbol
        ! Para obtener las capas (El valor de la capa) 
        ! Y se concatenara a la cadena
        ! El recorrido es limitado por la cantidad de capas que se desean obtener
        ! Es decir, si se desea obtener 5 capas, se obtendran las primeras 5 capas en preorden
        ! si no hay 5 capas, se obtendran las que haya
        call obtenerRecorridoPreordenRec(self%raiz, cantidad, contador, nodo_respuesta)


        
        call nodo_respuesta%matriz_general%generarImagen(nodo_respuesta%respuesta)

        comando_abrir_imagen = "start ImagenGenerada.png"
        call system(comando_abrir_imagen)

    end subroutine obtenerRecorridoPreorden
    
    recursive subroutine obtenerRecorridoPreordenRec(raiz, cantidad, contador, nodo_respuesta)
        type(nodo), pointer, intent(in) :: raiz
        integer, intent(in) :: cantidad
        character(len=20) :: valor_str
        integer, intent(inout) :: contador
        type(nodo_texto), intent(inout) :: nodo_respuesta
        integer :: i, j
        character(len=7) :: color_a
        type(node_matriz_val) :: val
    
        if(associated(raiz)) then
            if (contador >= cantidad) then
                return
            endif
    
            ! Incrementar el contador de nodos visitados
            contador = contador + 1
    
            write(valor_str, '(I20)') raiz%valor
            nodo_respuesta%respuesta = trim(adjustl(nodo_respuesta%respuesta)) //& 
            " " // trim(adjustl(valor_str))

            !Agregamos a la matriz del nodo respuesta los nodos de la capa
            do j=0, raiz%capa%height
                do i=0, raiz%capa%width
                    val = raiz%capa%obtenerValor(i, j)

                    if (val%exists) then
                        write(color_a, '(A7)') val%valor
                        call nodo_respuesta%matriz_general%insert(i,j, color_a)
                    end if

                end do
            end do
    
            ! Recorrer el subárbol izquierdo
            call obtenerRecorridoPreordenRec(raiz%izquierda, cantidad, contador, nodo_respuesta)
    
            ! Recorrer el subárbol derecho
            call obtenerRecorridoPreordenRec(raiz%derecha, cantidad, contador, nodo_respuesta)
        endif
    end subroutine obtenerRecorridoPreordenRec
    
    

    subroutine obtenerRecorridoInorden(self, cantidad)
        class(abb), intent(in) :: self
        integer, intent(in) :: cantidad
        integer :: contador
        type(nodo_texto) :: nodo_respuesta
        character(len=256):: comando_abrir_imagen
    
        ! Inicializar la cadena
        nodo_respuesta%respuesta = "Indorden: "
        contador = 0
    
        ! Se realizará un recorrido inorden sobre el árbol
        ! Para obtener las capas (El valor de la capa) 
        ! Y se concatenará a la cadena
        ! El recorrido es limitado por la cantidad de capas que se desean obtener
        ! Es decir, si se desea obtener 5 capas, se obtendrán las primeras 5 capas en inorden
        ! si no hay 5 capas, se obtendrán las que haya
        call obtenerRecorridoInordenRec(self%raiz, cantidad, contador, nodo_respuesta)

        call nodo_respuesta%matriz_general%generarImagen(nodo_respuesta%respuesta)

        comando_abrir_imagen = "start ImagenGenerada.png"
        call system(comando_abrir_imagen)
    end subroutine obtenerRecorridoInorden
    
    recursive subroutine obtenerRecorridoInordenRec(raiz, cantidad, contador, nodo_respuesta)
        type(nodo), pointer, intent(in) :: raiz
        integer, intent(in) :: cantidad
        character(len=20) :: valor_str
        integer, intent(inout) :: contador
        type(nodo_texto), intent(inout) :: nodo_respuesta
        integer :: i, j
        type(node_matriz_val) :: val
        character(len=7) :: color_a
    
        if(associated(raiz)) then
            ! Si se ha alcanzado la cantidad deseada de nodos, detener la recursión
            if (contador >= cantidad) then
                return
            endif
    
            ! Recorrer el subárbol izquierdo
            call obtenerRecorridoInordenRec(raiz%izquierda, cantidad, contador, nodo_respuesta)
            
            ! Incrementar el contador después de visitar el subárbol izquierdo
            contador = contador + 1
    
            ! Concatenar el valor del nodo actual a la cadena
            write(valor_str, '(I20)') raiz%valor
            nodo_respuesta%respuesta = trim(adjustl(nodo_respuesta%respuesta)) //&
             " " // trim(adjustl(valor_str))
            do j=0, raiz%capa%height
                do i=0, raiz%capa%width
                    val = raiz%capa%obtenerValor(i, j)

                    if (val%exists) then
                        write(color_a, '(A7)') val%valor
                        call nodo_respuesta%matriz_general%insert(i,j, color_a)
                    end if

                end do
            end do
    
            ! Recorrer el subárbol derecho
            call obtenerRecorridoInordenRec(raiz%derecha, cantidad, contador, nodo_respuesta)
        endif
    end subroutine obtenerRecorridoInordenRec
    
    

    subroutine obtenerRecorridoPostorden(self, cantidad)
        class(abb), intent(in) :: self
        integer, intent(in) :: cantidad
        integer :: contador
        type(nodo_texto) :: nodo_respuesta
        character(len=256):: comando_abrir_imagen
    
        ! Inicializar la cadena
        nodo_respuesta%respuesta = "Postorden: "
        contador = 0
    
        ! Se realizará un recorrido postorden sobre el árbol
        ! Para obtener las capas (El valor de la capa) 
        ! Y se concatenará a la cadena
        ! El recorrido es limitado por la cantidad de capas que se desean obtener
        ! Es decir, si se desea obtener 5 capas, se obtendrán las primeras 5 capas en postorden
        ! si no hay 5 capas, se obtendrán las que haya
        call obtenerRecorridoPostordenRec(self%raiz, cantidad, contador, nodo_respuesta)

        call nodo_respuesta%matriz_general%generarImagen(nodo_respuesta%respuesta)

        comando_abrir_imagen = "start ImagenGenerada.png"
        call system(comando_abrir_imagen)
    end subroutine obtenerRecorridoPostorden
    
    recursive subroutine obtenerRecorridoPostordenRec(raiz, cantidad, contador, nodo_respuesta)
        type(nodo), pointer, intent(in) :: raiz
        integer, intent(in) :: cantidad
        character(len=20) :: valor_str
        integer, intent(inout) :: contador
        type(nodo_texto), intent(inout) :: nodo_respuesta
        integer :: i, j
        type(node_matriz_val) :: val
        character(len=7) :: color_a
    
        if(associated(raiz) .and. cantidad > 0) then
    
            ! Recorrer el subárbol izquierdo
            call obtenerRecorridoPostordenRec(raiz%izquierda, cantidad, contador, nodo_respuesta)
    
            ! Recorrer el subárbol derecho
            call obtenerRecorridoPostordenRec(raiz%derecha, cantidad, contador, nodo_respuesta)
    
            ! Incrementar el contador después de procesar los nodos hijos
            contador = contador + 1
    
            ! Verificar si se ha alcanzado la cantidad deseada de nodos
            if (contador <= cantidad) then
                write(valor_str, '(I20)') raiz%valor
                nodo_respuesta%respuesta = trim(adjustl(nodo_respuesta%respuesta)) //&
                 " " // trim(adjustl(valor_str))

                do j=0, raiz%capa%height
                    do i=0, raiz%capa%width
                        val = raiz%capa%obtenerValor(i, j)
    
                        if (val%exists) then
                            write(color_a, '(A7)') val%valor
                            call nodo_respuesta%matriz_general%insert(i,j, color_a)
                        end if
    
                    end do
                end do
            endif
        endif
    end subroutine obtenerRecorridoPostordenRec
    

    subroutine obtenerRecorridoNiveles(self)
        class(abb), intent(in) :: self
        type(Cola) :: colas
        type(nodo_texto) :: nodo_respuesta
        character(len=256):: comando_abrir_imagen
        call encolar(colas, self%raiz)
        !Inicializar la cadena
        nodo_respuesta%respuesta = "Recorrido por niveles: "
        !Se realizara un recoorrido por niveles sobre el arbol
        !PAra obtener las capas (El valor de la capa) 
        !Y se concatenara a la cadena
        call obtenerRecorridoNivelesRec(colas, nodo_respuesta)

        print *, "Recorrido por niveles: ", trim(nodo_respuesta%respuesta)
        call nodo_respuesta%matriz_general%generarImagen(nodo_respuesta%respuesta)
        comando_abrir_imagen = "start ImagenGenerada.png"
        call system(comando_abrir_imagen)
    end subroutine obtenerRecorridoNiveles

    recursive subroutine obtenerRecorridoNivelesRec(colas, nodo_respuesta)
        class(Cola), intent(inout) :: colas
        type(nodo_texto), intent(inout) :: nodo_respuesta
        type(nodo), pointer :: temp
        character (len=20) :: valor_str
        integer :: i, j
        type(node_matriz_val) :: val
        character(len=7) :: color_a

        if(associated(colas%cabeza)) then
            temp => desencolar(colas)
            write(valor_str, '(I20)') temp%valor
            nodo_respuesta%respuesta = trim(adjustl(nodo_respuesta%respuesta))//&
            " " // trim(adjustl(valor_str))
            do j=0, temp%capa%height
                do i=0, temp%capa%width
                    val = temp%capa%obtenerValor(i, j)

                    if (val%exists) then
                        write(color_a, '(A7)') val%valor
                        call nodo_respuesta%matriz_general%insert(i,j, color_a)
                    end if

                end do
            end do
            if(associated(temp%izquierda)) then
                call encolar(colas, temp%izquierda)
            end if
            if(associated(temp%derecha)) then
                call encolar(colas, temp%derecha)
            end if
            call obtenerRecorridoNivelesRec(colas, nodo_respuesta)
        end if
    end subroutine obtenerRecorridoNivelesRec

    subroutine obtenerNodoHoja(self, io)

        class(abb), intent(in) :: self
        integer :: io
        type(lista_imagenes) :: lista
        type(nodo_imagen_lista), pointer :: temp
        character(len=12) :: id_capa_str


        call obtenerNodoHojaRec(self%raiz, lista)
        !Metemos a la lista tdoso los nodos hoja, para luego graficarlos
        !Se usara un recorrido preorden para obtener los nodos hoja

        write(io, *) "subgraph cluster_nodo_hoja {"
        write(io, *) '"NodoEtiqueta_hojas" [label="Capas que son hoja"]'

        temp => lista%head

        do while (associated(temp))
            write(id_capa_str, '(I12)') temp%id_imagen
            write(io, *) '"Nodo'//generate_uuid()//'" [label="Capa con id '//&
            trim(adjustl(id_capa_str))//'"]'
            temp => temp%next
        end do

        write(io, *) "}"

    end subroutine obtenerNodoHoja

    recursive subroutine obtenerNodoHojaRec(raiz, lista)
        type(nodo), pointer, intent(in) :: raiz
        type(lista_imagenes), intent(inout) :: lista
        if(associated(raiz)) then
            if((.not. associated(raiz%izquierda)) .and. (.not. associated(raiz%derecha))) then
                call lista%insertar_imagen_l(raiz%valor, 0)
            end if
            call obtenerNodoHojaRec(raiz%izquierda, lista)
            call obtenerNodoHojaRec(raiz%derecha, lista)
        end if
    end subroutine obtenerNodoHojaRec

    subroutine obtenerProfundidad(self, io)

        class(abb), intent(in) :: self
        integer :: io
        integer :: profundidad
        character(len=12) :: profundidad_str

        profundidad = 0
        call obtenerProfundidadRec(self%raiz, 0, profundidad)
        write(profundidad_str, '(I12)') profundidad

        write(io, *) "subgraph cluster_profundidad {"
        write(io, *) '"NodoEtiqueta_profundidad" [label="Profundidad del arbol"]'
        write(io, *) '"NodoProfundidad" [label="Profundidad: '//trim(adjustl(profundidad_str))//'"]'
        write(io, *) "}"

    end subroutine obtenerProfundidad

    recursive subroutine obtenerProfundidadRec(raiz, nivel, profundidad)
        type(nodo), pointer, intent(in) :: raiz
        integer, intent(in) :: nivel
        integer, intent(inout) :: profundidad

        if(associated(raiz)) then
            if(nivel > profundidad) then
                profundidad = nivel
            end if
            call obtenerProfundidadRec(raiz%izquierda, nivel+1, profundidad)
            call obtenerProfundidadRec(raiz%derecha, nivel+1, profundidad)
        end if
    end subroutine obtenerProfundidadRec

    subroutine listarCapasOrdenes(self, io)
        !Se listaran las capas en los diferentes ordenes
        class(abb), intent(in) :: self
        integer :: io
        type(nodo_texto) :: listado

        write(io, *) "subgraph cluster_listar_capas {"
        write(io, *) '"NodoEtiqueta_listar_capas" [label="Listado de capas en diferentes ordenes"]'


        listado%respuesta = ""
        call obtenerRecorridoPreordenRecListar(self%raiz, listado)
        write(io, *) '"NodoPreorden" [label=" Preorden: '//&
        trim(adjustl(listado%respuesta))//'"]'

        listado%respuesta = ""
        call obtenerRecorridoInordenRecListar(self%raiz, listado)
        write(io, *) '"NodoInorden" [label="Inorden: '//&
        trim(adjustl(listado%respuesta))//'"]'

        listado%respuesta = ""
        call obtenerRecorridoPostordenRecListar(self%raiz, listado)
        write(io, *) '"NodoPostorden" [label="Postorden: '//&
        trim(adjustl(listado%respuesta))//'"]'


        write(io, *) "}"

    end subroutine listarCapasOrdenes

    recursive subroutine obtenerRecorridoPreordenRecListar(raiz, listado)
        type(nodo), pointer, intent(in) :: raiz
        type(nodo_texto), intent(inout) :: listado
        character(len=20) :: valor_str

        if(associated(raiz)) then
            write(valor_str, '(I20)') raiz%valor
            listado%respuesta = trim(adjustl(listado%respuesta)) //&
            ", " // trim(adjustl(valor_str))
            call obtenerRecorridoPreordenRecListar(raiz%izquierda, listado)
            call obtenerRecorridoPreordenRecListar(raiz%derecha, listado)
        end if
    end subroutine obtenerRecorridoPreordenRecListar

    recursive subroutine obtenerRecorridoInordenRecListar(raiz, listado)
        type(nodo), pointer, intent(in) :: raiz
        type(nodo_texto), intent(inout) :: listado
        character(len=20) :: valor_str

        if(associated(raiz)) then
            call obtenerRecorridoInordenRecListar(raiz%izquierda, listado)
            write(valor_str, '(I20)') raiz%valor
            listado%respuesta = trim(adjustl(listado%respuesta)) //&
            ", " // trim(adjustl(valor_str))
            call obtenerRecorridoInordenRecListar(raiz%derecha, listado)
        end if
    end subroutine obtenerRecorridoInordenRecListar

    recursive subroutine obtenerRecorridoPostordenRecListar(raiz, listado)
        type(nodo), pointer, intent(in) :: raiz
        type(nodo_texto), intent(inout) :: listado
        character(len=20) :: valor_str

        if(associated(raiz)) then
            call obtenerRecorridoPostordenRecListar(raiz%izquierda, listado)
            call obtenerRecorridoPostordenRecListar(raiz%derecha, listado)
            write(valor_str, '(I20)') raiz%valor
            listado%respuesta = trim(adjustl(listado%respuesta)) //&
            ", " // trim(adjustl(valor_str))
        end if
    end subroutine obtenerRecorridoPostordenRecListar

    function contarCapas(self) result(res)
        class(abb), intent(in) :: self
        integer :: res
        res = 0
        call contarCapasRec(self%raiz, res)
    end function contarCapas

    recursive subroutine contarCapasRec(raiz, res)
        type(nodo), pointer, intent(in) :: raiz
        integer, intent(inout) :: res
        if(associated(raiz)) then
            res = res + 1
            call contarCapasRec(raiz%izquierda, res)
            call contarCapasRec(raiz%derecha, res)
        end if
    end subroutine contarCapasRec


end module abb_m