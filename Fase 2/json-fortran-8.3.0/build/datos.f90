module datos_m

    use arbolB_m

    implicit none

    type(arbolB_clientes) :: arbolClientes
    type(nodo_cliente), pointer :: clienteActual
    

end module datos_m