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
        character (len=24) :: nombre_anterior
        character (len=8) :: cantidad_imagenes_grandes_cadena
        character (len=8) :: cantidad_imagenes_pequenas_cadena
        character (len=8) :: numero_ventanilla_cadena
        character (len=:), allocatable :: cliente_atendido_actual
        character (len=24) :: nombre_imagen_anterior
        character (len=1) :: tamano_imagen
        integer :: id_cliente_cabecera_org
        character (len=8) :: nombre_primer_cliente

        !Un nodo para cada estructura
        type(nodo_lista_doble_circular), pointer :: nodo_lista_clientes_espera
        type(Node_Cola_Cliente), pointer :: nodo_cola_clientes
        type(nodo_impresion), pointer :: nodo_cola_impresion_pequenas
        type(nodo_impresion), pointer :: nodo_cola_impresion_grandes
        type(nodo_cliente_atendido), pointer :: nodo_lista_clientes_atendidos
        type(nodo_ventanilla), pointer :: nodo_lista_ventanillas
        type(nodo_imagenes), pointer :: nodo_pila_imagenes
        type(nodo_lista_enlazada_simple), pointer :: nodo_lista_simple_imagenes_espera
        integer :: primera_conexion
        character (len=8) :: id_str

        

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

        !Ahora la cola de impresion de imagenes grandes
        !iniciamos el subgrafo con etiqueta afuera del bucle
        write(io, *) "subgraph cluster_2 {"
        write(io, *) "label ="//'"Cola Impresion Grandes"'
        write(io, *) "color=green"
        !cambiamos tipo de letra
        write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightgreen""]"
        !Nodo con el titulo impresora grande
        write(io, *) '"NodoImpresoraGrande"[label = "Impresora Grande"]'
        primeros=""
        conexiones=""
        nombre_anterior=""

        nodo_cola_impresion_grandes => cola_impresion_grandes_global%head
        primera_conexion = 0
        do while (associated(nodo_cola_impresion_grandes))

            write(nombre, "(I5)") index

            if (primeros == "") then
                primeros = trim(nombre)
            end if

            write(io, *) '"Nodo'//trim(nombre)//'"[label = "'//(nodo_cola_impresion_grandes%tamano)//'"]'

            !Si es el primer nodo lo conectamos con el nodo impresora grande
            if (primera_conexion == 0) then
                conexiones = '"NodoImpresoraGrande" -> "Nodo'//trim(nombre)//'";'
                !salto de linea
                write(io, *) conexiones
                conexiones = ""
            end if

            if(primera_conexion > 0) then
                conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'";'
                !salto de linea
                write(io, *) conexiones
                conexiones = ""
            end if

            nodo_cola_impresion_grandes => nodo_cola_impresion_grandes%next

            index = index + 1
            primera_conexion = primera_conexion + 1
            nombre_anterior = nombre

        end do

        write(io, *) "}"

        !Ahora la cola de impresion de imagenes pequeñas

        write(io, *) "subgraph cluster_3 {"
        write(io, *) "label ="//'"Cola Impresion Pequeñas"'
        write(io, *) "color=red"
        !cambiamos tipo de letra
        write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightcoral""]"
        !Nodo con el titulo impresora pequena
        write(io, *) '"NodoImpresoraPequena"[label = "Impresora Pequeña"]'
        primeros=""
        conexiones=""
        nombre_anterior=""
        nodo_cola_impresion_pequenas => cola_impresion_pequenas_global%head
        primera_conexion = 0
        do while (associated(nodo_cola_impresion_pequenas))

            write(nombre, "(I5)") index

            if (primeros == "") then
                primeros = trim(nombre)
            end if

            write(io, *) '"Nodo'//trim(nombre)//'"[label = "'//(nodo_cola_impresion_pequenas%tamano)//'"]'

            !Si es el primer nodo lo conectamos con el nodo impresora grande
            if (primera_conexion == 0) then
                conexiones = '"NodoImpresoraPequena" -> "Nodo'//trim(nombre)//'";'
                !salto de linea
                write(io, *) conexiones
                conexiones = ""
            end if

            if(primera_conexion > 0) then
                conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'";'
                !salto de linea
                write(io, *) conexiones
                conexiones = ""
            end if

            nodo_cola_impresion_pequenas => nodo_cola_impresion_pequenas%next

            index = index + 1
            primera_conexion = primera_conexion + 1
            nombre_anterior = nombre

        end do

        write(io, *) "}"

        !Lista de clientes en espera
        nodo_lista_clientes_espera => lista_clientes_espera_global%head
        if (associated(nodo_lista_clientes_espera)) then
            write(io, *) "subgraph cluster_4 {"
            write(io, *) "label ="//'"Lista Clientes Espera"'
            write(io, *) "color=orange"
            !cambiamos tipo de letra
            write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightyellow""]"
            primeros=""
            conexiones=""
            nombre_anterior=""
            primera_conexion = 0
            id_cliente_cabecera_org = lista_clientes_espera_global%head%id_cliente
            do while (associated(nodo_lista_clientes_espera))
                nodo_lista_simple_imagenes_espera => nodo_lista_clientes_espera%lista_imagenes
                write(nombre, "(I5)") index

                if (primeros == "") then
                    primeros = trim(nombre)
                end if

                write(io, *) '"Nodo'//trim(nombre)//'"[label = "'//(nodo_lista_clientes_espera%nombre_cliente)//'"]'

                !Si es el primer cliente guardamos su nombre
                if (nodo_lista_clientes_espera%id_cliente == id_cliente_cabecera_org) then
                    nombre_primer_cliente = trim(nombre)
                end if

                nombre_imagen_anterior = nombre
                !Graficamos la lista de imagenes de cada cliente
                if(associated(nodo_lista_simple_imagenes_espera)) then
                    do while(associated(nodo_lista_simple_imagenes_espera))
                        index = index + 1
                        write(nombre_imagen, "(I5)") index
                        write(io, *) '"Nodo'//trim(nombre_imagen)//'"[label = "'//(nodo_lista_simple_imagenes_espera%tamano)//'"]'
                        conexiones = conexiones//'"Nodo'//trim(nombre_imagen_anterior)//'" -> "Nodo'//trim(nombre_imagen)//'";'
                        !salto de linea
                        write(io, *) conexiones
                        conexiones = ""
                        nombre_imagen_anterior = nombre_imagen
                        nodo_lista_simple_imagenes_espera => nodo_lista_simple_imagenes_espera%next
                        index = index + 1
                    end do
                end if


                if(primera_conexion > 0) then
                    conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'";'
                    write(io, *) conexiones
                    conexiones = ""
                end if


                nodo_lista_clientes_espera => nodo_lista_clientes_espera%next
                index = index + 1
                primera_conexion = primera_conexion + 1

                if(nodo_lista_clientes_espera%id_cliente == id_cliente_cabecera_org) then
                    conexiones = '"Nodo'//trim(nombre)//'" -> "Nodo'//trim(nombre_primer_cliente)//'";'
                    !salto de linea
                    write(io, *) conexiones
                    conexiones = ""
                    exit
                end if

                nombre_anterior = nombre

            end do

            write(io, *) "}"

        end if

        !Lista de clientes atendidos
        nodo_lista_clientes_atendidos => lista_clientes_atendidos_global%head
        if (associated(nodo_lista_clientes_atendidos)) then
            write(io, *) "subgraph cluster_5 {"
            write(io, *) "label ="//'"Lista Clientes Atendidos"'
            write(io, *) "color=grey"
            !cambiamos tipo de letra
            write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightgrey""]"
            primeros=""
            conexiones=""
            nombre_anterior=""
            primera_conexion = 0
            do while (associated(nodo_lista_clientes_atendidos))
                write(nombre, "(I5)") index

                if (primeros == "") then
                    primeros = trim(nombre)
                end if

                write(numero_ventanilla_cadena, "(I5)") nodo_lista_clientes_atendidos%ventanilla_atendida
                write(cantidad_imagenes_grandes_cadena, "(I5)") nodo_lista_clientes_atendidos%imagenes_impresas
                write(cantidad_imagenes_pequenas_cadena, "(I5)") nodo_lista_clientes_atendidos%pasos_en_sistema
                write(id_str, "(I5)") nodo_lista_clientes_atendidos%id_cliente

                write(io, *) '"Nodo'//trim(nombre)//'"[label = "Id Cliente: '//id_str&
                //"\nNombre cliente:"//(nodo_lista_clientes_atendidos%nombre_cliente) &
                //"\nVentanilla que lo atendió: "//numero_ventanilla_cadena &
                //"\nImagenes impresas: "//cantidad_imagenes_grandes_cadena &
                //"\nPasos en el sistema: "//cantidad_imagenes_pequenas_cadena //'"]'

                if(primera_conexion > 0) then
                    conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'";'
                    write(io, *) conexiones
                    conexiones = ""
                end if

                nodo_lista_clientes_atendidos => nodo_lista_clientes_atendidos%next
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

    !Subrutina para generar reportes, los resultados se generaran en forma de graficas con graphviz
    !Top 5 clientes atendidos con mayor cantidad de imagenes grandes
    !Top 5 clientes atendidos con menor cantidad de imagenes pequeñas
    !Cliente con mayor cantidad de pasos en el sistema
    !Datos sobre un cliente en especifico, se pedira su numero de id

    !Para ordenar los nodos en base a la cantidad de imagenes grandes impresas, usaremos un algoritmo de ordenamiento
    !de burbuja, ya que el numero de nodos es pequeño y no afectara el rendimiento del programa
    !Lo mismo para la cantidad de imagenes pequeñas impresas y la cantidad de pasos en el sistema

    subroutine generar_reportes(io, id_cliente)

        type(lista_clientes_atendidos) :: lista_clientes_atendidos_grandes
        type(lista_clientes_atendidos) :: lista_clientes_atendidos_pequenas
        integer, intent(in) :: id_cliente
        type(nodo_cliente_atendido), pointer :: cliente_especifico
        type(nodo_cliente_atendido), pointer :: cliente_mas_pasos
        type(nodo_cliente_atendido), pointer :: current_cliente

        integer, intent(out) :: io
        integer :: i
        integer :: index, numero_conexion
        character (len=100), allocatable :: comando
        character (len=:), allocatable :: conexiones
        character (len=:), allocatable :: primeros
        character (len=8) :: nombre
        character (len=8) :: id_str, img_G_str, img_P_str, numero_ventanilla_cadena
        character (len=24) :: cantidad_pasos_cadena
        character (len=24) :: nombre_anterior

        call lista_clientes_atendidos_global%ordenar_lista_descendente_grandes(lista_clientes_atendidos_grandes)
        call lista_clientes_atendidos_global%ordenar_lista_ascendente_pequenas(lista_clientes_atendidos_pequenas)
        call lista_clientes_atendidos_global%cliente_con_mas_pasos(cliente_mas_pasos)
        call lista_clientes_atendidos_global%devolver_cliente(id_cliente, cliente_especifico)

        !Graficamos los resultados en un archivo .dot desde cero
        comando = "dot -Tpng -o reportes.png reportes.dot"
        io = 1
        index = 0
        conexiones = ""
        primeros = ""
        numero_conexion = 0
        open(newunit=io, file="reportes.dot")
        write(io, *) "digraph G {"

        !Top 5 clientes atendidos con mayor cantidad de imagenes grandes
        write(io, *) "subgraph cluster_0 {"
        write(io, *) "label ="//'"Top 5 Clientes Atendidos con mayor cantidad de imagenes grandes"'
        write(io, *) "color=brown"
        !cambiamos tipo de letra
        write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""beige""]"

        current_cliente => lista_clientes_atendidos_grandes%head

        do while (associated(current_cliente))
            write(nombre, "(I5)") index

            write(id_str, "(I5)") current_cliente%id_cliente
            write(img_G_str, "(I5)") current_cliente%imagenes_grandes
            write(img_P_str, "(I5)") current_cliente%imagenes_pequenas
            write(numero_ventanilla_cadena, "(I5)") current_cliente%ventanilla_atendida
            write(cantidad_pasos_cadena, "(I5)") current_cliente%pasos_en_sistema


            write(io, *) '"Nodo'//trim(nombre)//'"[label = "', &
            "ID: "//id_str &
            //"\nNombre cliente: "//current_cliente%nombre_cliente &
            //"\nIMG-G: "//img_G_str &
            //"\nIMG-P: "//img_P_str&
            //"\nVentanilla que lo atendió: "//numero_ventanilla_cadena &
            //"\nPasos en el sistema: "//cantidad_pasos_cadena //'"]'

            if(numero_conexion > 0) then
                conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'";'
                !salto de linea
                write(io, *) conexiones
                conexiones = ""
            end if

            nombre_anterior = nombre
            current_cliente => current_cliente%next
            index = index + 1
            numero_conexion = numero_conexion + 1
        end do

        write(io, *) "}"

        !Top 5 clientes atendidos con menor cantidad de imagenes pequeñas
        write(io, *) "subgraph cluster_1 {"
        write(io, *) "label ="//'"Top 5 Clientes Atendidos con menor cantidad de imagenes pequeñas"'
        write(io, *) "color=blue"
        !cambiamos tipo de letra
        write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightblue""]"

        current_cliente => lista_clientes_atendidos_pequenas%head
        numero_conexion = 0
        numero_conexion = 0
        conexiones = ""
        nombre_anterior = ""
        primeros = ""

        do while (associated(current_cliente))
            write(nombre, "(I5)") index

            write(id_str, "(I5)") current_cliente%id_cliente
            write(img_G_str, "(I5)") current_cliente%imagenes_grandes
            write(img_P_str, "(I5)") current_cliente%imagenes_pequenas
            write(numero_ventanilla_cadena, "(I5)") current_cliente%ventanilla_atendida
            write(cantidad_pasos_cadena, "(I5)") current_cliente%pasos_en_sistema


            write(io, *) '"Nodo'//trim(nombre)//'"[label = "', &
            "ID: "//id_str &
            //"\nNombre cliente: "//current_cliente%nombre_cliente &
            //"\nIMG-G: "//img_G_str &
            //"\nIMG-P: "//img_P_str&
            //"\nVentanilla que lo atendió: "//numero_ventanilla_cadena &
            //"\nPasos en el sistema: "//cantidad_pasos_cadena //'"]'

            if(numero_conexion > 0) then
                conexiones = conexiones//'"Nodo'//trim(nombre_anterior)//'" -> "Nodo'//trim(nombre)//'";'
                !salto de linea
                write(io, *) conexiones
                conexiones = ""
            end if

            nombre_anterior = nombre
            current_cliente => current_cliente%next
            index = index + 1
            numero_conexion = numero_conexion + 1
        end do

        write(io, *) "}"

        !Cliente con mayor cantidad de pasos en el sistema
        write(io, *) "subgraph cluster_2 {"
        write(io, *) "label ="//'"Cliente con mayor cantidad de pasos en el sistema"'
        write(io, *) "color=green"
        !cambiamos tipo de letra
        write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightgreen""]"

        if (associated(cliente_mas_pasos)) then

        write(nombre, "(I5)") index

        write(id_str, "(I5)") cliente_mas_pasos%id_cliente
        write(img_G_str, "(I5)") cliente_mas_pasos%imagenes_grandes
        write(img_P_str, "(I5)") cliente_mas_pasos%imagenes_pequenas
        write(numero_ventanilla_cadena, "(I5)") cliente_mas_pasos%ventanilla_atendida
        write(cantidad_pasos_cadena, "(I5)") cliente_mas_pasos%pasos_en_sistema


        write(io, *) '"Nodo'//trim(nombre)//'"[label = "', &
        "ID: "//id_str &
        //"\nNombre cliente: "//cliente_mas_pasos%nombre_cliente &
        //"\nIMG-G: "//img_G_str &
        //"\nIMG-P: "//img_P_str&
        //"\nVentanilla que lo atendió: "//numero_ventanilla_cadena &
        //"\nPasos en el sistema: "//cantidad_pasos_cadena //'"]'

        index = index + 1

        end if

        write(io, *) "}"

        !Datos sobre un cliente en especifico
        write(io, *) "subgraph cluster_3 {"
        write(io, *) "label ="//'"Datos sobre un cliente en especifico"'
        write(io, *) "color=red"
        !cambiamos tipo de letra
        write(io, *) "node [shape = box, fontname = ""Arial"", style = filled, color = ""lightcoral""]"

        if (associated(cliente_especifico)) then

        write(nombre, "(I5)") index

        write(id_str, "(I5)") cliente_especifico%id_cliente
        write(img_G_str, "(I5)") cliente_especifico%imagenes_grandes
        write(img_P_str, "(I5)") cliente_especifico%imagenes_pequenas
        write(numero_ventanilla_cadena, "(I5)") cliente_especifico%ventanilla_atendida
        write(cantidad_pasos_cadena, "(I5)") cliente_especifico%pasos_en_sistema


        write(io, *) '"Nodo'//trim(nombre)//'"[label = "', &
        "ID: "//id_str &
        //"\nNombre cliente: "//cliente_especifico%nombre_cliente &
        //"\nIMG-G: "//img_G_str &
        //"\nIMG-P: "//img_P_str&
        //"\nVentanilla que lo atendió: "//numero_ventanilla_cadena &
        //"\nPasos en el sistema: "//cantidad_pasos_cadena //'"]'

        end if

        write(io, *) "}"

        write(io,*) "rankdir=LR"
        write(io, *) "}"

        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i==1) then
            print *, "Error al ejecutar el comando"
        else
            print *, "Grafica de los reportes generada"
        end if



        end subroutine generar_reportes
        

end module estructua_datos_module