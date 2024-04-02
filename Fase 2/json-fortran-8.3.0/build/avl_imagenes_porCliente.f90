module avl_m
    use uuid_module
    use abb_m
    implicit none

    type :: nodo_avl
        !nodo representa a una imagen
    !Valor es el id de la imagen
        integer :: valor
        integer :: altura = 1
        !A su vez cada imagen contiene un abb de las capas que la componen
        type(abb) :: capas
        integer :: cantidad_capas = 0
        type(nodo_avl), pointer :: derecha => null()
        type(nodo_avl), pointer :: izquierda => null() 
    end type

    type, public :: avl
        type(nodo_avl), pointer :: raiz => null()
    
    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorden
        procedure :: graficar
        procedure :: buscar_imagen
    end type avl

contains
    subroutine insert(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        call insertRec(self%raiz, val)
    end subroutine insert

    subroutine delete(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        self%raiz => deleteRec(self%raiz, val)
    end subroutine delete

    subroutine preorden(self)
        class(avl), intent(in) :: self
        
        call preordenRec(self%raiz)
    end subroutine preorden

    recursive subroutine insertRec(raiz, val)
        type(nodo_avl), pointer, intent(inout) :: raiz
        integer, intent(in) :: val

        if(.not. associated(raiz)) then
            allocate(raiz)
            raiz = nodo_avl(valor=val)
        
        else if(val < raiz%valor) then 
            call insertRec(raiz%izquierda, val)

        else if(val > raiz%valor) then
            call insertRec(raiz%derecha, val)
        else if(val == raiz%valor) then
            print *, "El valor ya existe, no se puede insertar"
        end if

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1

        if(obtenerBalance(raiz) > 1) then
            if(obtenerBalance(raiz%derecha) < 0) then
                raiz%derecha => rotacionDerecha(raiz%derecha)
                raiz => rotacionIzquierda(raiz)
            else
                raiz => rotacionIzquierda(raiz)
            end if
        end if

        if(obtenerBalance(raiz) < -1) then
            if(obtenerBalance(raiz%izquierda) > 0) then
                raiz%izquierda => rotacionIzquierda(raiz%izquierda)
                raiz => rotacionDerecha(raiz)

            else
                raiz => rotacionDerecha(raiz)
            end if
        end if
    end subroutine insertRec

    recursive function deleteRec(raiz, val) result(res)
        type(nodo_avl), pointer :: raiz
        integer, intent(in) :: val

        type(nodo_avl), pointer :: temp
        type(nodo_avl), pointer :: res 
        
        if(.not. associated(raiz)) then
            res => raiz
            return
        end if

        if(val < raiz%valor) then
            raiz%izquierda => deleteRec(raiz%izquierda, val)
        
        else if(val > raiz%valor) then
            raiz%derecha => deleteRec(raiz%derecha, val)

        else
            if(.not. associated(raiz%izquierda)) then
                temp => raiz%derecha
                deallocate(raiz)
                res => temp

            else if (.not. associated(raiz%derecha)) then
                temp => raiz%izquierda
                deallocate(raiz)
                res => temp
            
            else
                call obtenerMayorDeMenores(raiz%izquierda, temp)
                raiz%valor = temp%valor
                raiz%izquierda => deleteRec(raiz%izquierda, temp%valor)
            end if
        end if

        res => raiz
        if(.not. associated(raiz)) return

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha))

        if(obtenerBalance(raiz) > 1) then
            if(obtenerBalance(raiz%derecha) < 0) then
                raiz%derecha => rotacionDerecha(raiz%derecha)
                raiz => rotacionIzquierda(raiz)
            else
                raiz => rotacionIzquierda(raiz)
            end if
        end if

        if(obtenerBalance(raiz) < -1) then
            if(obtenerBalance(raiz%izquierda) > 0) then
                raiz%izquierda => rotacionIzquierda(raiz%izquierda)
                raiz => rotacionDerecha(raiz)

            else
                raiz => rotacionDerecha(raiz)
            end if
        end if

        res => raiz
    end function deleteRec

    function rotacionIzquierda(raiz) result(raizDerecha)
        type(nodo_avl), pointer, intent(in) :: raiz
        type(nodo_avl), pointer :: raizDerecha
        type(nodo_avl), pointer :: temp

        raizDerecha => raiz%derecha
        temp => raizDerecha%izquierda

        raizDerecha%izquierda => raiz
        raiz%derecha => temp

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        raizDerecha%altura = maximo(obtenerAltura(raizDerecha%izquierda), obtenerAltura(raizDerecha%derecha)) + 1
    end function rotacionIzquierda

    function rotacionDerecha(raiz) result(raizIzquierda)
        type(nodo_avl), pointer, intent(in) :: raiz
        type(nodo_avl), pointer :: raizIzquierda
        type(nodo_avl), pointer :: temp

        raizIzquierda => raiz%izquierda
        temp => raizIzquierda%derecha

        raizIzquierda%derecha => raiz
        raiz%izquierda => temp

        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        raizIzquierda%altura = maximo(obtenerAltura(raizIzquierda%izquierda), obtenerAltura(raizIzquierda%derecha)) + 1
    end function rotacionDerecha

    recursive subroutine obtenerMayorDeMenores(raiz, mayor)
        type(nodo_avl), pointer :: raiz, mayor
        if(associated(raiz%derecha)) then
            call obtenerMayorDeMenores(raiz%derecha, mayor)
        else
            mayor => raiz
        end if
    end subroutine obtenerMayorDeMenores

    recursive subroutine preordenRec(raiz)
        type(nodo_avl), pointer, intent(in) :: raiz

        if(associated(raiz)) then
            print *, raiz%valor
            call preordenRec(raiz%izquierda)
            call preordenRec(raiz%derecha)
        end if
    end subroutine preordenRec

    function maximo(izquierda, derecha) result(res)
        integer, intent(in) :: izquierda
        integer, intent(in) :: derecha

        integer :: res
        res = derecha

        if(izquierda >= derecha) then
            res = izquierda
            return
        end if
    end function maximo

    function obtenerBalance(raiz) result(res)
        type(nodo_avl), pointer, intent(in) :: raiz
        integer :: res
        
        res = obtenerAltura(raiz%derecha) - obtenerAltura(raiz%izquierda)
    end function

    function obtenerAltura(n) result(res)
        type(nodo_avl), pointer :: n
        integer :: res
        res = 0

        if(.not. associated(n)) return
        res = n%altura
    end function obtenerAltura

    recursive subroutine imprimirRec(raiz, nombre, io)
        type(nodo_avl), pointer, intent(in) :: raiz
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
            call imprimirRec(raiz%izquierda, izquierda, io)
            call imprimirRec(raiz%derecha, derecha, io)
        end if
    end subroutine imprimirRec

    subroutine graficar(self)
        class(avl), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./avl_imagenes.dot")
        comando = "dot -Tpng ./avl_imagenes.dot -o ./avl_imagenes.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%raiz)) then
            call imprimirRec(self%raiz, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficar

    function buscar_imagen (self, id_imagen) result (res)
        class(avl), intent(in) :: self
        integer, intent(in) :: id_imagen
        type(nodo_avl), pointer :: nodo_iterador
        type(nodo_avl), pointer :: res
        
        if(associated(self%raiz)) then
            nodo_iterador => self%raiz
                if(nodo_iterador%valor == id_imagen) then
                    res => nodo_iterador
                    return
                else if(id_imagen < nodo_iterador%valor) then
                    res => buscar_imagen_rec(nodo_iterador%izquierda, id_imagen)
                else
                    res => buscar_imagen_rec(nodo_iterador%derecha, id_imagen)
                end if
        else
            res => null()
        end if

    end function buscar_imagen

    recursive function buscar_imagen_rec (raiz, id_imagen) result (res)
        type(nodo_avl), pointer :: raiz
        integer, intent(in) :: id_imagen
        type(nodo_avl), pointer :: res

        if(.not. associated(raiz)) then
            res => null()
            return
        end if

        if(raiz%valor == id_imagen) then
            res => raiz
            return
        else if(id_imagen < raiz%valor) then
            res => buscar_imagen_rec(raiz%izquierda, id_imagen)
        else
            res => buscar_imagen_rec(raiz%derecha, id_imagen)
        end if
    end function buscar_imagen_rec
end module avl_m