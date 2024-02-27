module estructua_datos_module

    use lista_doble_circular_module
    use cola_clientes_module
    use cola_impresion_module
    use lista_clientes_atendidos_module
    use lista_simple_ventanillas_modulo
    use pila_imagenes_module

    implicit none

    type(lista_doble_circular) :: lista_clientes_espera_global
    type(Cola_Clientes) :: cola_clientes_global
    type(cola_impresion) :: cola_impresion_pequenas_global
    type(cola_impresion) :: cola_impresion_grandes_global
    type(lista_clientes_atendidos) :: lista_clientes_atendidos_global
    type(lista_simple_ventanillas) :: lista_ventanillas_global
    type(pila_imagenes) :: pila_imagenes_global

    !Subrutina para graficar todas las listas con graphviz, se escribira desde cero

    contains

    subroutine graficar_estructuras(io)
        integer, intent(out) :: io
        integer :: i
        integer :: index
        character (len=100), allocatable :: comando
        character (len=:), allocatable :: conexiones
        character (len=:), allocatable :: primeros
        character (len=8) :: nombre
        character (len=8) :: nombre_imagen
        character (len=8) :: nombre_anterior
        character (len=8) :: cantidad_imagenes_grandes_cadena
        character (len=8) :: cantidad_imagenes_pequenas_cadena
        character (len=8) :: numero_ventanilla_cadena
        character (len=:), allocatable :: cliente_atendido_actual
        character (len=8) :: nombre_imagen_anterior
        character (len=1) :: tamano_imagen

        !Un nodo para cada estructura
        type(nodo_lista_doble_circular), pointer :: nodo_lista_clientes_espera
        type(Node_Cola_Cliente), pointer :: nodo_cola_clientes
        type(nodo_impresion), pointer :: nodo_cola_impresion_pequenas
        type(nodo_impresion), pointer :: nodo_cola_impresion_grandes
        type(nodo_cliente_atendido), pointer :: nodo_lista_clientes_atendidos
        type(nodo_ventanilla), pointer :: nodo_lista_ventanillas
        type(nodo_imagenes), pointer :: nodo_pila_imagenes
        integer :: primera_conexion

        

        comando = "dot -Tpng -o estructuras.png estructuras.dot"
        io = 1
        index = 0
        conexiones = ""
        primeros = ""
        open(newunit=io, file="estructuras.dot")
        write(io, *) "digraph G {"

        !Primero la cola de clientes
        if (associated(cola_clientes_global%head)) then
            write(io, *) "subgraph cluster_0 {"
            write(io, *) "label ="//'"Cola Recepción"'
            write(io, *) "color=brown"
            !cambiamos tipo de letra
            write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""beige""]"
            primeros=""
            conexiones=""
            nombre_anterior=""
            nodo_cola_clientes => cola_clientes_global%head
            primera_conexion = 0
            do while (associated(nodo_cola_clientes))

                write(nombre, "(I5)") index

                if (primeros == "") then
                    primeros = trim(nombre)
                end if

                write(cantidad_imagenes_grandes_cadena, "(I5)") nodo_cola_clientes%imagenes_grandes
                write(cantidad_imagenes_pequenas_cadena, "(I5)") nodo_cola_clientes%imagenes_pequenas

                write(io, *) '"Nodo'//trim(nombre)//'"[label = "', (nodo_cola_clientes%nombre), &
                "\n", "IMG-G: "//cantidad_imagenes_grandes_cadena ,&
                "\n", "IMG-P: "//cantidad_imagenes_pequenas_cadena,'"]'


                if(primera_conexion > 0) then
                    conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'" [dir=back];'
                    !salto de linea
                    write(io, *) conexiones
                    conexiones = ""
                end if
                

                nodo_cola_clientes => nodo_cola_clientes%next
                index = index + 1
                primera_conexion = primera_conexion + 1
                nombre_anterior = nombre

            end do

            write(io, *) "}"


        end if

        !Ahora la lista de ventanillas
        if (associated(lista_ventanillas_global%head)) then
            write(io, *) "subgraph cluster_1 {"
            write(io, *) "label ="//'"Lista Ventanillas"'
            write(io, *) "color=blue"
            !cambiamos tipo de letra
            write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightblue""]"
            primeros=""
            conexiones=""
            nombre_anterior=""
            nodo_lista_ventanillas => lista_ventanillas_global%head
            primera_conexion = 0
            do while (associated(nodo_lista_ventanillas))

                write(nombre, "(I5)") index

                if (primeros == "") then
                    primeros = trim(nombre)
                end if

                if(nodo_lista_ventanillas%id_cliente == -1) then
                    cliente_atendido_actual = "Vacia"
                else

                    write(cantidad_imagenes_pequenas_cadena, "(I5)") &
                    nodo_lista_ventanillas%imagenes_pequenas_original

                    write(cantidad_imagenes_grandes_cadena, "(I5)") &
                    nodo_lista_ventanillas%imagenes_grandes_original

                    cliente_atendido_actual = (nodo_lista_ventanillas%nombre_cliente)&
                    //"\nIMG-G: "//cantidad_imagenes_grandes_cadena&
                    //"\nIMG-P: "//cantidad_imagenes_pequenas_cadena
                end if

                write(numero_ventanilla_cadena, "(I5)") nodo_lista_ventanillas%numero_ventanilla
                write(io, *) '"Nodo'//trim(nombre)//'"[label = "', &
                "Ventanilla "//trim(numero_ventanilla_cadena), &
                "\n"//cliente_atendido_actual,'"]'

                index = index + 1
                nombre_imagen_anterior = nombre
                !Accedemos a la pilas de imagenes dentro del nodo ventanilla y graficamos
                if(associated(nodo_lista_ventanillas%pila_imagenes%top)) then
                    nodo_pila_imagenes => nodo_lista_ventanillas%pila_imagenes%top
                    do while(associated(nodo_pila_imagenes))
                        if (nodo_pila_imagenes%tamano == "Grande") then
                            tamano_imagen = "G"
                        else
                            tamano_imagen = "P"
                        end if
                        write(nombre_imagen, "(I5)") index
                        write(io, *) '"Nodo'//trim(nombre_imagen)//'"[label = "', &
                        "IMG: "//tamano_imagen,'"]'
                        conexiones = conexiones//'"Nodo'//trim(nombre_imagen_anterior)//'" -> "Nodo'//trim(nombre_imagen)//'";'
                        !salto de linea
                        write(io, *) conexiones
                        conexiones = ""
                        nombre_imagen_anterior = nombre_imagen
                        nodo_pila_imagenes => nodo_pila_imagenes%next
                        index = index + 1
                    end do
                end if
                

                if(primera_conexion > 0) then
                    conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'";'
                    !salto de linea
                    write(io, *) conexiones
                    conexiones = ""
                end if
                

                nodo_lista_ventanillas => nodo_lista_ventanillas%next
                index = index + 1
                primera_conexion = primera_conexion + 1
                nombre_anterior = nombre

            end do

            write(io, *) "}"

        end if
        
        write(io,*) "rankdir=LR"
        write(io, *) "}"
        close(io)
        call execute_command_line(comando, exitstat=i)

        if(i==1) then
            print *, "Error al ejecutar el comando"
        else
            print *, "Grafica de la cola de clientes generada"
        end if


    end subroutine graficar_estructuras
        

end module estructua_datos_module