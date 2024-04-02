module abb_m
    !Arbol de capas global
    use uuid_module
    use matrix_m
    implicit none

    type :: nodo
        integer :: valor
        type(matrix) :: capa
        type(nodo), pointer :: derecha => null()
        type(nodo), pointer :: izquierda => null()
    end type

    type, public :: abb
        type(nodo), pointer :: raiz => null()
    
    contains
        procedure :: insertarCapa
        procedure :: deleteCapa
        procedure :: preordenCapa
        procedure :: graficarCapa
        procedure :: buscarCapa
    end type abb

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
end module abb_m