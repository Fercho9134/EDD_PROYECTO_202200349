module btree_m
    use page_m
    type, public :: btree
        type(page), pointer :: root
        integer :: orden = 5

    contains 
        procedure :: inserta
        procedure :: insertInLeaf
        procedure :: buscar
    end type btree

contains 

    
    function buscar (self, value) result(res)
        class(btree), intent(inout) :: self
        integer (kind=8), intent(in) :: value
        type (key), pointer :: res

        type(key), pointer :: p
        type(page), pointer :: root

        root => self%root
        
        if (associated(root)) then
            p => root%first
        else
            res => null()
            return
            
        end if

        !Primero revisamos la pagina raiz, si no usamos una funcion recursiva auxiliar
        !Solo la mandamos a llamar a la izquierda o derecha dependiendo del valor, si es menor o mayor

        do while (associated(p))
            if (value == p%value) then
                res => p
                return
            else if (value < p%value) then
                res => buscarAux(value, p%left)
                return
            !Si la siguiente clave es null, entonces buscamos en la derecha
            else if (.not. associated(p%next)) then
                res => buscarAux(value, p%right)
                return
            else
                p => p%next
            end if

        end do

    end function buscar

    recursive function buscarAux (value, root) result(res)
        integer (kind=8), intent(in) :: value
        type(page), pointer, intent(in) :: root
        type (key), pointer :: res

        type(key), pointer :: p

        if (associated(root)) then
            p => root%first
        else
            res => null()
            return
        end if

        do while (associated(p))
            if (value == p%value) then
                res => p
                return
            else if (value < p%value) then
                res => buscarAux(value, p%left)
                return
            else if (.not. associated(p%next)) then
                res => buscarAux(value, p%right)
                return
            else
                p => p%next
            end if
        end do



        res => null()
    end function buscarAux


    subroutine inserta(self, value, nombre, contrasena)
        class(btree), intent(inout) :: self
        integer (kind=8), intent(in) :: value
        character(len=*), intent(in) :: nombre
        character(len=*), intent(in) :: contrasena

        type(key), pointer :: newRoot

        
        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = page()
        end if

        newRoot => self%insertInLeaf(value, self%root, nombre, contrasena)
        if(associated(newRoot)) then
            self%root = page()
            call self%root%insertKey(newRoot)
            self%root%leaf = .false.
        end if
    end subroutine inserta

    recursive function insertInLeaf(self, value, root, nombre, contrasena) result(res)
        class(btree), intent(inout) :: self
        integer (kind=8) , intent(in) :: value
        character(len=*), intent(in) :: nombre
        character(len=*), intent(in) :: contrasena

        type(page), pointer, intent(inout) :: root
        type(key), pointer :: rootKey
        type(key), pointer :: new
        type(key), pointer :: res
        type(key), pointer :: p
        
        if(root%leaf) then
            allocate(new)
            new%value = value
            new%nombre = nombre
            new%contrasena = contrasena
            call root%insertKey(new)

            if(root%numberKeys == self%orden) then
                res => divide(root)
            else
                res => null()
            end if
            return
        else
            p => root%first
            if(value == p%value) then
                res => null()
                return
               
            else if (value < p%value) then
                rootKey => self%insertInLeaf(value, p%left, nombre, contrasena)
                if(associated(rootKey)) then
                    call root%insertKey(rootKey)

                    if(root%numberKeys == self%orden) then
                        res => divide(root)
                        return
                    else
                        res => null()
                        return
                    end if
                end if
                res => null()
                return

            else
                do while (.true.)
                    if(value == p%value) then
                        res => null()
                        return

                    else if(value < p%value) then
                        rootKey => self%insertInLeaf(value, p%left, nombre, contrasena)
                        if(associated(rootKey)) then
                            call root%insertKey(rootKey)
                            if(root%numberKeys == self%orden) then
                                res => divide(root)
                                return
                            else
                                res => null()
                                return
                            end if
                        end if
                        res => null()
                        return

                    else if(.not. associated(p%next)) then
                        rootKey => self%insertInLeaf(value, p%right, nombre, contrasena)
                        if(associated(rootKey)) then
                            call root%insertKey(rootKey)
                            if(root%numberKeys == self%orden) then
                                res => divide(root)
                                return

                            else 
                                res => null()
                                return
                            end if
                        end if
                        res => null()
                        return
                    end if

                    p => p%next
                    if(.not. associated(p)) exit
                end do
            end if
        end if
        res => null()
        return
    end function insertInLeaf

    function divide(n) result(res)
        type(page), pointer :: n
        type(key), pointer :: res
        type(key), pointer :: temp
        type(page), pointer :: left
        type(page), pointer :: right
        type(key), pointer :: new

        integer :: cont
        integer (kind=8) :: rootValue
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: contrasena
        allocate(left)
        allocate(right)
        allocate(res)
        temp => n%first
        cont = 1

        do while(associated(temp))
            if(cont < 3) then
                allocate(new)
                new = key(value=temp%value, nombre=temp%nombre, contrasena=temp%contrasena)
                new%right => temp%right
                new%left => temp%left
                call left%insertKey(new)
                if(new%hasKids()) then
                    left%leaf = .false.
                end if

            else if(cont == 3) then
                rootValue = temp%value
                nombre = temp%nombre
                contrasena = temp%contrasena

            else
                allocate(new)
                new = key(value=temp%value, nombre=temp%nombre, contrasena=temp%contrasena)
                new%right => temp%right
                new%left => temp%left
                call right%insertKey(new)
                if(new%hasKids()) then
                    right%leaf = .false.
                end if
            end if
            cont = cont + 1
            temp => temp%next
        end do

        res = key(value=rootValue, nombre=nombre, contrasena=contrasena)
        res%right => right
        res%left => left
    end function divide

    



end module btree_m