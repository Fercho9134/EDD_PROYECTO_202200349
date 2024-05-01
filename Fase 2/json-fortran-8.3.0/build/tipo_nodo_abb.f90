module tipo_nodo_abb_m
    use matrix_m
implicit none

type :: nodo
        integer :: valor
        type(matrix) :: capa
        type(nodo), pointer :: derecha => null()
        type(nodo), pointer :: izquierda => null()
end type nodo
end module tipo_nodo_abb_m