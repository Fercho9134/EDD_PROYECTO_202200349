program main
    use json_module
    use estructua_datos_module
    implicit none

    integer :: opcion
    integer :: cantidad_ventanillas
    integer :: iterador

    type(Node_Cola_Cliente) :: nuevo_cliente


    character(len=:), allocatable :: id_str, img_g_str, img_p_str

    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, clientePointer, attributePointer
    logical :: found
    integer :: size
    character(len=256) :: file_path
    integer :: i
    integer :: contador_pasos = 1


    
    ! Bucle principal del programa
    do
        ! Mostrar el menú
        call mostrar_menu()
        
        ! Leer la opción seleccionada
        print *, ""
        print *, "Seleccione una opcion:"
        read(*, *) opcion
        print *, ""
        
        ! Realizar la acción correspondiente según la opción seleccionada
        select case(opcion)
            case(1)
                call parametros_iniciales()
            case(2)
                call ejecutar_paso()
            case(3)
                call estado_en_memoria()
            case(4)
                call reportes()
            case(5)
                call acerca_de()
            case(6)
                exit  ! Salir del bucle y terminar el programa
            case default
                print *, "Opcion invalida. Por favor, seleccione una opcion valida."
        end select
    end do

contains

    ! Subrutina para mostrar el menú
    subroutine mostrar_menu()
        print *, ""
        print *, "-------------Menu Principal-------------"
        print *, "1. Parametros iniciales"
        print *, "2. Ejecutar paso"
        print *, "3. Estado en memoria de las estructuras"
        print *, "4. Reportes"
        print *, "5. Acerca de"
        print *, "6. Salir"
    end subroutine mostrar_menu

    ! Subrutina para manejar la opción de parámetros iniciales
    subroutine parametros_iniciales()
        ! Implementar la lógica para cargar masivamente clientes y establecer cantidad de ventanillas
        print *, "Parametros iniciales"

        ! Mostramos las dos subopciones
        print *, "1. Cargar masivamente clientes"
        print *, "2. Establecer cantidad de ventanillas"
        print *, "Seleccione una opcion:"
        read(*, *) opcion

        ! Realizar la acción correspondiente según la opción seleccionada
        select case(opcion)
            case(1)
                call cargar_masivamente_clientes()
            case(2)
                call establecer_cantidad_ventanillas()
            case default
                print *, "Opcion invalida. Por favor, seleccione una opcion valida."
        end select

    end subroutine parametros_iniciales

    ! Subrutina para manejar la opción de cargar masivamente clientes
    subroutine cargar_masivamente_clientes()
        file_path = 'clientes.json'

        call json%initialize()
        call json%load(filename=file_path)
        call json%info('', n_children=size)

        call json%get_core(jsonc)
        call json%get('', listPointer, found)

        ! Iterar sobre los clientes
        do i=1, size

            call jsonc%get_child(listPointer, i, clientePointer, found)

            ! Leer el id del cliente
            call jsonc%get_child(clientePointer, 'id', attributePointer, found)
            call jsonc%get(attributePointer, id_str)
            
            read(id_str, *) nuevo_cliente%id_cliente

            ! Leer el nombre del cliente
            call jsonc%get_child(clientePointer, 'nombre', attributePointer, found)
            call jsonc%get(attributePointer, nuevo_cliente%nombre)

            ! Leer el número de imágenes grandes del cliente
            call jsonc%get_child(clientePointer, 'img_g', attributePointer, found)
            call jsonc%get(attributePointer, img_g_str)
            
            read(img_g_str, *) nuevo_cliente%imagenes_grandes

            ! Leer el número de imágenes pequeñas del cliente
            call jsonc%get_child(clientePointer, 'img_p', attributePointer, found)
            call jsonc%get(attributePointer, img_p_str)

            read(img_p_str, *) nuevo_cliente%imagenes_pequenas


            ! Agregar el cliente a la cola de clientes
            call cola_clientes_global%encolar_cliente_nuevo(nuevo_cliente%id_cliente, &
                                                 nuevo_cliente%nombre, 0, &
                                                 nuevo_cliente%imagenes_pequenas, &
                                                 nuevo_cliente%imagenes_grandes)

        end do


        print *, "Carga masiva de clientes completada."

        print *, ""

        
    end subroutine cargar_masivamente_clientes

    ! Subrutina para manejar la opción de establecer cantidad de ventanillas
    subroutine establecer_cantidad_ventanillas()
        print *, ""
        print *, "Ingrese la cantidad de ventanillas: "
        read(*, *) cantidad_ventanillas

        do iterador = 1, cantidad_ventanillas
            call lista_ventanillas_global%agregar_ventanilla(iterador)
        end do

        print *, "Se han ingresado ", cantidad_ventanillas, " ventanillas"

        print *, ""

        !Creamos una ventanilla en la lista de ventanillas por cada una que se haya ingresado
        
    end subroutine establecer_cantidad_ventanillas


    ! Subrutina para manejar la opción de ejecutar paso
    subroutine ejecutar_paso()
        !Paso 1: Genereamos entre 0 y 3 clientes aleatorios y los agregamos a la cola de clientes
        real :: cantidad_clientes_aleatorios
        integer :: iterador_clientes_aleatorios, cantidad_entero
        type(nodo_ventanilla), pointer :: ventanilla_actual
        type(Node_Cola_Cliente), pointer :: cliente_atendido
        type(nodo_imagenes), pointer :: imagen_actual
        !La variable cliente_cola_bool nos indicará si ya se pasó un cliente de la cola en el paso actual
        !solo puede pasar un cliente de la cola por paso
        logical :: cliente_cola_bool
        integer :: id_cliente_desapilado
        character(len=:), allocatable :: tamano_imagen_desapilada
        type(nodo_impresion), pointer :: impresion_actual
        type(nodo_lista_doble_circular), pointer :: cliente_en_espera, cliente_finalizado, cliente_eliminado_espera
        integer :: id_cliente_tmp_impresion
        character(len=:), allocatable :: tamano_tmp_impresion
        integer :: imagenes_totales
        integer :: id_cliente_cabeza_espera

        if (.not. associated(lista_ventanillas_global%head)) then
            print *, "No se han ingresado ventanillas, por favor ingrese la cantidad de ventanillas"
            return
        end if

        print *, "-------------Paso ", contador_pasos, "-------------"

        ventanilla_actual => lista_ventanillas_global%head

        call random_number(cantidad_clientes_aleatorios)
        cantidad_entero = int(cantidad_clientes_aleatorios * 4)

        print *, "Se generaron ", cantidad_entero, " clientes aleatorios"

        do iterador_clientes_aleatorios = 1, cantidad_entero
            call generar_cliente_aleatorio(nuevo_cliente)
            call cola_clientes_global%encolar_cliente_nuevo(nuevo_cliente%id_cliente, &
                                                 nuevo_cliente%nombre, 0, &
                                                 nuevo_cliente%imagenes_pequenas, &
                                                 nuevo_cliente%imagenes_grandes)
        end do

        !Paso intermedio: Verificamos si los clientes en espera ya tienen todas sus imagenes
        !Si ya tienen todas sus imagenes, los eliminamos de la lista de espera y los enviamos a la
        !lista de clientes atendidos
        cliente_finalizado => lista_clientes_espera_global%head
        if (associated(cliente_finalizado)) then
            id_cliente_cabeza_espera = cliente_finalizado%id_cliente
        end if

        
        do while (associated(cliente_finalizado))
            if (cliente_finalizado%cantidad_imagenes_grandes_original == cliente_finalizado%cantidad_imagenes_grandes) then
                
                if (cliente_finalizado%cantidad_imagenes_pequenas_original == cliente_finalizado%cantidad_imagenes_pequenas) then
                
                    !Elimine al cliente 2, revisar
                imagenes_totales = cliente_finalizado%cantidad_imagenes_grandes + cliente_finalizado%cantidad_imagenes_pequenas
                call lista_clientes_espera_global%eliminar_nodo_lista_doble_circular(cliente_finalizado%id_cliente,&
                                                                                    cliente_eliminado_espera)
                call lista_clientes_atendidos_global%agregar_cliente_atendido(cliente_eliminado_espera%nombre_cliente, &
                                                                             cliente_eliminado_espera%ventanilla_atendida, &
                                                                             imagenes_totales, &
                                                                             cliente_eliminado_espera%pasos_esperados)
                print *, "El cliente ", cliente_eliminado_espera%nombre_cliente, &
                " ha sido atendido completamente"

                exit
                end if
                
            end if
            cliente_finalizado => cliente_finalizado%next
                

            !Verificamos si hemos vuelto al inicio de la lista
            if (cliente_finalizado%id_cliente == id_cliente_cabeza_espera) then
                exit
            end if

        end do

        !Paso 3: Procesamos las impresiones grandes y pequeñas, la impresion pequeña tardará un paso
        !y la impresión grande tardará 2 pasos la variable pasos_impresion nos indicará cuantos pasos
        !lleva la impresión en la cola, si es 0, no se ha procesado, si es 1, se procesó un paso, si es 2, se procesaron 2 pasos

        !Después de sumar los pasos, verificamos si es Grande o pequeña y la cantidad de pasos que llevan
        !Si ya se procesaron los pasos necesarios, eliminamos la impresión de la cola
        !Buscamos coincidencias entre la lista de clientes en espera, la id_cliente de la imagen
        !Y la id_cliente de cada cliente en la lista de espera
        !Si hay coincidencia, eliminamos la impresion de la cola y añadimos a la lista de imagenes procesadas
        !Del cliente en espera la imagen que se acaba de procesar, sumando a la cantidad de imagenes procesadas
        impresion_actual => cola_impresion_grandes_global%head
        if (associated(impresion_actual)) then
            impresion_actual%pasos = impresion_actual%pasos + 1
            if (impresion_actual%pasos == 2) then
                call lista_clientes_espera_global%buscar_nodo_por_id(impresion_actual%id_cliente, cliente_en_espera)
                if (associated(cliente_en_espera)) then
                    call lista_clientes_espera_global%agregar_nodo_lista_enlazada_simple(cliente_en_espera, & 
                                                impresion_actual%id_cliente, "Grande")
                    call cola_impresion_grandes_global%desencolar_impresion(id_cliente_tmp_impresion, & 
                                                tamano_tmp_impresion)
                    cliente_en_espera%cantidad_imagenes_grandes = cliente_en_espera%cantidad_imagenes_grandes + 1
                    print *, "Se ha procesado una impresion grande"
                end if
            end if
        end if

        impresion_actual => cola_impresion_pequenas_global%head
        if (associated(impresion_actual)) then
            impresion_actual%pasos = impresion_actual%pasos + 1
            if (impresion_actual%pasos == 1) then
                call lista_clientes_espera_global%buscar_nodo_por_id(impresion_actual%id_cliente, cliente_en_espera)
                if (associated(cliente_en_espera)) then
                    call lista_clientes_espera_global%agregar_nodo_lista_enlazada_simple(cliente_en_espera,&
                                                                    impresion_actual%id_cliente, "Pequena")
                    call cola_impresion_pequenas_global%desencolar_impresion(id_cliente_tmp_impresion, tamano_tmp_impresion)
                    cliente_en_espera%cantidad_imagenes_pequenas = cliente_en_espera%cantidad_imagenes_pequenas + 1
                    print *, "Se ha procesado una impresion pequeña"
                end if
            end if
        end if


        !Paso 2: Atendemos a los clientes en las ventanillas antes de pasar a los clientes de la cola
        !Porque una ventanilla puede vaciarce y atender a un cliente de la cola

        !Iteramos sobre las ventanillas, hay 2 opciones, que esté vacía o que esté atendiendo a un cliente
        !Si la id es -1, está vacía, si no, está atendiendo a un cliente, pasamos a la siguiente ventanilla
        !Si está atendiendo a un cliente, verificamos si le faltan imagenes pequeñas por mandar a la pila de imagenes
        !de la ventanilla, solo mandamos una por iteración, si ya no le faltan imagenes pequeñas, verificamos si le faltan
        !imagenes grandes, si no le faltan, atendemos al siguiente cliente de la cola.
        !Si justamente en ese paso manda su ultima imagen grande, lo eliminamos de la ventanilla y atendemos al siguiente cliente

        !Iteramos sobre las ventanillas
        cliente_cola_bool = .false.
        do while (associated(ventanilla_actual))

            if (ventanilla_actual%id_cliente == -1) then

                if (cliente_cola_bool .eqv. .false.) then
                !La ventanilla está vacía pasamos al cliente de la cola
                    call cola_clientes_global%desencolar_cliente_cola(cliente_atendido)
                    if (associated(cliente_atendido)) then
                        !Si hay un cliente en la cola, lo pasamos a la ventanilla
                        ventanilla_actual%id_cliente = cliente_atendido%id_cliente
                        ventanilla_actual%nombre_cliente = cliente_atendido%nombre
                        ventanilla_actual%pasos_en_espera = cliente_atendido%tiempo_espera
                        ventanilla_actual%imagenes_pequenas = cliente_atendido%imagenes_pequenas
                        ventanilla_actual%imagenes_grandes = cliente_atendido%imagenes_grandes
                        ventanilla_actual%imagenes_pequenas_original = cliente_atendido%imagenes_pequenas
                        ventanilla_actual%imagenes_grandes_original = cliente_atendido%imagenes_grandes
                        cliente_cola_bool = .true.
                        print *, "El cliente ", cliente_atendido%nombre, " ha pasado a la ventanilla ",& 
                        ventanilla_actual%numero_ventanilla
                    end if
                end if
                ventanilla_actual => ventanilla_actual%next
            else if (ventanilla_actual%id_cliente /= -1) then
                !Ya hay un cliente en la ventanilla

                !Primero verificamos si le fatan imagenes grandes por mandar a la pila de imagenes
                !Si le faltan, mandamos una imagen grande a la pila de imagenes y pasamos a la siguiente ventanilla
                !Si no le faltan, verificamos si le faltan imagenes pequeñas
                !Si le faltan, mandamos una imagen pequeña a la pila de imagenes y pasamos a la siguiente ventanilla
                !Si no le faltan, vaciamos la ventanilla y queda en el mismo paso podemos pasar al siguiente cliente de la cola
                !pasaremos un nuevo cliente solo si no se pasó uno en este paso
                if (ventanilla_actual%imagenes_grandes > 0) then
                    call ventanilla_actual%pila_imagenes%apilar_imagen(ventanilla_actual%id_cliente, "Grande")
                    ventanilla_actual%imagenes_grandes = ventanilla_actual%imagenes_grandes - 1
                    print *, "Se ha apilado una imagen grande del cliente ", ventanilla_actual%nombre_cliente,&
                     " en la ventanilla ", ventanilla_actual%numero_ventanilla
                else if (ventanilla_actual%imagenes_pequenas > 0) then
                    call ventanilla_actual%pila_imagenes%apilar_imagen(ventanilla_actual%id_cliente, "Pequena")
                    ventanilla_actual%imagenes_pequenas = ventanilla_actual%imagenes_pequenas - 1
                    print *, "Se ha apilado una imagen pequeña del cliente ", ventanilla_actual%nombre_cliente, &
                    " en la ventanilla ", ventanilla_actual%numero_ventanilla
                else if (ventanilla_actual%imagenes_pequenas == 0 .and. ventanilla_actual%imagenes_grandes == 0) then
                    !Enviamos al cliente a la lista de espera y sus imagenes a sus respectivas colas de impresión
                    call lista_clientes_espera_global%agregar_nodo_lista_doble_circular(ventanilla_actual%id_cliente, &
                                                                       ventanilla_actual%nombre_cliente, &
                                                                       ventanilla_actual%numero_ventanilla, &
                                                                       ventanilla_actual%pasos_en_espera, &
                                                                       ventanilla_actual%imagenes_grandes_original, &
                                                                       ventanilla_actual%imagenes_pequenas_original)
                    
                    !Vaciamos la pila de imagenes de la ventanilla, desapilando una por una
                    imagen_actual => ventanilla_actual%pila_imagenes%top
                    do while (associated(imagen_actual))
                        call ventanilla_actual%pila_imagenes%desapilar_imagen(id_cliente_desapilado, tamano_imagen_desapilada)
                        if (tamano_imagen_desapilada == "Grande") then
                            call cola_impresion_grandes_global%encolar_impresion(id_cliente_desapilado, tamano_imagen_desapilada)
                        else
                            call cola_impresion_pequenas_global%encolar_impresion(id_cliente_desapilado, tamano_imagen_desapilada)
                        end if
                        imagen_actual => ventanilla_actual%pila_imagenes%top
                    end do

                    print *, "El cliente ", ventanilla_actual%nombre_cliente, " ha pasado a la lista de espera"
                    
                    !Vaciamos la ventanilla
                    ventanilla_actual%id_cliente = -1
                    ventanilla_actual%nombre_cliente = ""
                    ventanilla_actual%pasos_en_espera = 0
                    ventanilla_actual%imagenes_pequenas = 0
                    ventanilla_actual%imagenes_grandes = 0
                    ventanilla_actual%imagenes_pequenas_original = 0
                    ventanilla_actual%imagenes_grandes_original = 0

                    !Pasamos al siguiente cliente de la cola
                    
                    if (cliente_cola_bool .eqv. .false.) then
                        !Si no se pasó un cliente de la cola en este paso, pasamos al siguiente cliente
                        call cola_clientes_global%desencolar_cliente_cola(cliente_atendido)
                        if (associated(cliente_atendido)) then
                            !Si hay un cliente en la cola, lo pasamos a la ventanilla
                            ventanilla_actual%id_cliente = cliente_atendido%id_cliente
                            ventanilla_actual%nombre_cliente = cliente_atendido%nombre
                            ventanilla_actual%pasos_en_espera = cliente_atendido%tiempo_espera
                            ventanilla_actual%imagenes_pequenas = cliente_atendido%imagenes_pequenas
                            ventanilla_actual%imagenes_grandes = cliente_atendido%imagenes_grandes
                            ventanilla_actual%imagenes_pequenas_original = cliente_atendido%imagenes_pequenas
                            ventanilla_actual%imagenes_grandes_original = cliente_atendido%imagenes_grandes
                            cliente_cola_bool = .true.
                            print *, "El cliente ", cliente_atendido%nombre, " ha pasado a la ventanilla ", &
                            ventanilla_actual%numero_ventanilla
                        end if
                    end if
                end if
                
                ventanilla_actual => ventanilla_actual%next

            else
                !Si la ventanilla está vacía y ya se pasó un cliente de la cola, pasamos a la siguiente ventanilla
                ventanilla_actual => ventanilla_actual%next
            
            end if
        end do

        !Sumamos un paso a los clientes en todas las listas
        cliente_atendido => cola_clientes_global%head
        do while (associated(cliente_atendido))
            cliente_atendido%tiempo_espera = cliente_atendido%tiempo_espera + 1
            cliente_atendido => cliente_atendido%next
        end do

        cliente_finalizado => lista_clientes_espera_global%head
        if (associated(cliente_finalizado)) then
            id_cliente_cabeza_espera = cliente_finalizado%id_cliente
        end if
        do while (associated(cliente_finalizado))
            cliente_finalizado%pasos_esperados = cliente_finalizado%pasos_esperados + 1
            cliente_finalizado => cliente_finalizado%next
            if (cliente_finalizado%id_cliente == id_cliente_cabeza_espera) then
                exit
            end if
        end do

        ventanilla_actual => lista_ventanillas_global%head
        do while (associated(ventanilla_actual))
            ventanilla_actual%pasos_en_espera = ventanilla_actual%pasos_en_espera + 1
            ventanilla_actual => ventanilla_actual%next
        end do
        
        contador_pasos = contador_pasos + 1
        
    end subroutine ejecutar_paso

    ! Subrutina para manejar la opción de estado en memoria de las estructuras
    subroutine estado_en_memoria()
        ! Implementar la lógica para mostrar el estado en memoria de las estructuras
        print *, "Estado en memoria de las estructuras"
    end subroutine estado_en_memoria

    ! Subrutina para manejar la opción de reportes
    subroutine reportes()

        integer :: io
        io = 1

        print *, "Reportes"
        print *, "1. Reporte de clientes"
        call cola_clientes_global%imprimir_cola_clientes()
        print *, "2. Reporte de ventanillas"
        call lista_ventanillas_global%imprimir_lista()
        print *, "3. Reporte de clientes en espera"
        call lista_clientes_espera_global%imprimir_lista_doble_circular()
        print *, "4. Reporte de impresiones grandes"
        call cola_impresion_grandes_global%imprimir_cola()
        print *, "5. Reporte de impresiones pequeñas"
        call cola_impresion_pequenas_global%imprimir_cola()
        print *, "6. Reporte de clientes atendidos"
        call lista_clientes_atendidos_global%imprimir_lista_clientes_atendidos()

        call graficar_estructuras(io)

    end subroutine reportes

    ! Subrutina para manejar la opción de acerca de
    subroutine acerca_de()
        ! Implementar la lógica para mostrar información acerca de los estudiantes
        print *, "Acerca de"
        print *, "Proyecto 1 de Estructuras de Datos"
        print *, "Estudiante: Irving Fernando Alvarado Asensio"
        print *, "Carne: 202200349"
        print *, "CUI: 3291 59046 1103"
        print *, "Curso: Estructura de Datos"
        print *, "Sección: A"
    end subroutine acerca_de

    subroutine generar_cliente_aleatorio(cliente)
        implicit none
        type(Node_Cola_Cliente) :: cliente
        real :: id_cliente
        character(len=6) :: nombres(15) = ["Juan  ", "Maria ", "Pedro ", "Luis  ", "Ana   ", &
                                              "Carlos", "Sofia ", "Diego ", "Laura ", "Pablo ", &
                                              "Elena ", "Manuel", "Carmen", "Javier", "Isabel"]
        character(len=6) :: apellidos(15) = ["Lopez ", "Garcia", "Martin", "Silva ", "Paz   ", &
                                               "Perez ", "Gomez ", "Sainz ", "Diaz  ", "Martin", &
                                               "Ruiz  ", "Hernan", "Aldana", "Moreno", "Osorio"]
        real :: num_nombres, num_apellidos, imgG, imgP
        integer :: indice_nombre, indice_apellido
    
        ! Generar un ID de cliente aleatorio
        call random_number(id_cliente)
        cliente%id_cliente = int(abs(id_cliente) * 100000)
    
        ! Seleccionar aleatoriamente un nombre y un apellido
        call random_number(num_nombres)
        call random_number(num_apellidos)
        indice_nombre = floor(num_nombres * 15) + 1
        indice_apellido = floor(num_apellidos * 15) + 1
        cliente%nombre = trim(nombres(indice_nombre)) // " " // trim(apellidos(indice_apellido))
    
        ! Generar la cantidad de imágenes grandes y pequeñas aleatorias
        call random_number(imgG)
        call random_number(imgP)
        cliente%imagenes_grandes = int(imgG * 5)
        cliente%imagenes_pequenas = int(imgP * 5)
    end subroutine generar_cliente_aleatorio
    
    
    

end program main