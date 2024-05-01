program main
    use grafo_m
    use avl_m
    use password_module
    use json_module
    implicit none

    integer :: opcion
    type(avl) :: sucursales
    type(grafo) :: mapa
    type(nodo_avl), pointer :: Sucursal_actual

    do 
        print *, ""
        print *, "************MENU PRINCIPAL************"
        print *, "1. Iniciar sesion como administrador"
        print *, "2. Salir"
        print *, ""

        read *, opcion

        select case(opcion)
            case(1)
                call inicioSesion()
            case(2)
                exit
            case default
                print *, "Opcion no valida"
        end select

    end do
    
    
    contains

    subroutine inicioSesion()
        character(len=50) :: usuario, contrasena

        print *, "Ingrese su usuario: "
        read *, usuario
        print *, "Ingrese su contrasena: "
        read *, contrasena

        if (usuario == "admin" .and. contrasena == "admin") then
            call menuAdmin()
        else
            print *, "Usuario o contrasena incorrectos"
        end if

    end subroutine inicioSesion

    subroutine menuAdmin()
        integer :: opcion

        do
            print *, ""
            print *, "************MENU ADMINISTRADOR************"
            print *, "1. Cargar de archivos"
            print *, "2. Sucursales"
            print *, "3. Reportes"
            print *, "4. Salir"
            print *, ""

            read *, opcion

            select case(opcion)
                case(1)
                    call cargarArchivos()
                case(2)
                    call menuSucursales()
                case(3)
                    call menuReportes()
                case(4)
                    exit
                case default
                    print *, "Opcion no valida"
            end select

        end do

    end subroutine menuAdmin

    subroutine cargarArchivos()
        integer :: opcion
        print *, ""
        print *, "************CARGAR ARCHIVOS************"
        print *, "1. Cargar sucursales"
        print *, "2. Cargar rutas"
        print *, "3. Salir"
        print *, ""

        read *, opcion

        select case(opcion)
            case(1)
                call cargarSucursales()
            case(2)
                call cargarRutas()
            case(3)
                return
            case default
                print *, "Opcion no valida"
        end select

    end subroutine cargarArchivos

    subroutine cargarSucursales()

        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: lista_sucursales, sucursal_pointer, atributos_pointer
        character(len=100) :: file_path
        integer :: size_sucursales, i
        type(nodo_avl), pointer :: nodo_insertado
        logical :: found
        integer :: id
        character (len=:), allocatable :: departamento, direccion, password, password_encriptada 


        print *, "Ingrese la ruta del archivo de sucursales: "
        read *, file_path

        call json%initialize()
        call json%load(filename=file_path)
        call json%info('', n_children=size_sucursales)

        call json%get_core(jsonc)
        call json%get('', lista_sucursales, found)

        do i = 1, size_sucursales
            departamento = ""
            direccion = ""
            password = ""
            password_encriptada = ""

            call jsonc%get_child(lista_sucursales, i, sucursal_pointer, found)
            call jsonc%get_child(sucursal_pointer, 'id', atributos_pointer, found)
            call jsonc%get(atributos_pointer, id)

            call jsonc%get_child(sucursal_pointer, 'departamento', atributos_pointer, found)
            call jsonc%get(atributos_pointer, departamento)

            call jsonc%get_child(sucursal_pointer, 'direccion', atributos_pointer, found)
            call jsonc%get(atributos_pointer, direccion)

            call jsonc%get_child(sucursal_pointer, 'password', atributos_pointer, found)
            call jsonc%get(atributos_pointer, password)

            call sucursales%inserta_avl(id)
            nodo_insertado => sucursales%buscar_imagen(id)

            nodo_insertado%departamento = departamento
            nodo_insertado%direccion = direccion

            !Enviamos la contraseña a la función de encriptación
            password_encriptada = encrypt_password(password, 123456)
            nodo_insertado%contrasena = password_encriptada

            !por cada sucursal se agrega un vertice al grafo
            call mapa%agregar_nodo(id)

        end do

    end subroutine cargarSucursales

    

    subroutine cargarRutas()

        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: lista_general, ruta_pointer, atributos_pointer
        character(len=100) :: file_path
        integer :: size_rutas, i
        logical :: found
        integer :: inicio, fin, distancia, impresoras 
    
        print *, "Ingrese la ruta del archivo de rutas: "
        read *, file_path
    
        call json%initialize()
        call json%load(filename=file_path)
    
        call json%get_core(jsonc)
        call json%get('grafo', lista_general, found)
        print *, "Encontrado: ", found
    
        call jsonc%info(lista_general, n_children=size_rutas)
        print *, "Size rutas: ", size_rutas
    
        do i = 1, size_rutas
            call jsonc%get_child(lista_general, i, ruta_pointer, found)
    
            call jsonc%get_child(ruta_pointer, 's1', atributos_pointer, found)
            call jsonc%get(atributos_pointer, inicio)

    
            call jsonc%get_child(ruta_pointer, 's2', atributos_pointer, found)
            call jsonc%get(atributos_pointer, fin)
  
    
            call jsonc%get_child(ruta_pointer, 'distancia', atributos_pointer, found)
            call jsonc%get(atributos_pointer, distancia)
     
    
            call jsonc%get_child(ruta_pointer, 'imp_mantenimiento', atributos_pointer, found)
            call jsonc%get(atributos_pointer, impresoras)
        
    

            call mapa%agregar_arista(inicio, fin, real(distancia), real(impresoras))
    
        end do
    
    end subroutine cargarRutas
    

    subroutine menuSucursales()
        integer :: opcion

        do
            print *, ""
            print *, "************INICIO SUCURSALES************"
            print *, "1. Ingresar a una sucursal"
            print *, "2. Salir"

            print *, ""

            read *, opcion

            select case(opcion)
                case(1)
                    call ingresarSucursal()
                case(2)
                    return
                case default
                    print *, "Opcion no valida"
            end select

        end do
            
    end subroutine menuSucursales

    subroutine ingresarSucursal()
        integer :: id
        type(nodo_avl), pointer :: nodo
        character(len=:), allocatable :: variable_contrasena
        logical :: found
        character(len=100) :: input_line

        variable_contrasena = ""

        print *, "Ingrese el id de la sucursal: "
        read *, id

        nodo => sucursales%buscar_imagen(id)

        if (.not.associated(nodo)) then
            print *, "No se encontro la sucursal"
            return
        end if

        print *, "Ingrese la contrasena: "
        read *, input_line
        variable_contrasena = trim(input_line)
        found = check_password(variable_contrasena, nodo%contrasena, 123456)

        if (found) then
            print *, ""
            print *, "**********Bienvenido a la sucursal ", nodo%valor, "**********"
            sucursal_actual => nodo
            call menuSucursal()
        else
            print *, "Contrasena incorrecta"
            return
        end if

    end subroutine ingresarSucursal

    subroutine menuSucursal()
        integer :: opcion

        do
            print *, ""
            print *, "************MENU SUCURSAL************"
            print *, "1. Cargar tecnicos"
            print *, "2. Generar recorrido mas optimo"
            print *, "3. Informacion tecnico especifico"
            print *, "4. Listar tecnicos"
            print *, "5. Generar reporte"
            print *, "6. Salir"

            print *, ""

            read *, opcion

            select case(opcion)
                case(1)
                    call cargarTecnicos()
                case(2)
                    call generarRecorrido()
                case(3)
                    call infoTecnico()
                case(4)
                    call listarTecnicos()
                case(5)
                    call generarReporte()
                case(6)
                    sucursal_actual => null()
                    return
                case default
                    print *, "Opcion no valida"
            end select

        end do 
        
    end subroutine menuSucursal

    subroutine cargarTecnicos()

        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: lista_tecnicos, tecnico_pointer, atributos_pointer
        character(len=100) :: file_path
        integer :: size_tecnicos, i
        logical :: found
        integer :: telefono
        character (len=:), allocatable :: dpi, nombre, apellido, genero, direccion
        integer (kind=8) :: dpi_int
        type(Persona) :: nodo_persona

        print *, "Ingrese la ruta del archivo de tecnicos: "
        read *, file_path

        call json%initialize()
        call json%load(filename=file_path)
        call json%info('', n_children=size_tecnicos)

        call json%get_core(jsonc)
        call json%get('', lista_tecnicos, found)

        do i = 1, size_tecnicos
            dpi = ""
            nombre = ""
            apellido = ""
            genero = ""
            direccion = ""

            call jsonc%get_child(lista_tecnicos, i, tecnico_pointer, found)
            call jsonc%get_child(tecnico_pointer, 'dpi', atributos_pointer, found)
            call jsonc%get(atributos_pointer, dpi)
            read(dpi, *) dpi_int


            call jsonc%get_child(tecnico_pointer, 'nombre', atributos_pointer, found)
            call jsonc%get(atributos_pointer, nombre)

            call jsonc%get_child(tecnico_pointer, 'apellido', atributos_pointer, found)
            call jsonc%get(atributos_pointer, apellido)

            call jsonc%get_child(tecnico_pointer, 'genero', atributos_pointer, found)
            call jsonc%get(atributos_pointer, genero)

            call jsonc%get_child(tecnico_pointer, 'direccion', atributos_pointer, found)
            call jsonc%get(atributos_pointer, direccion)

            call jsonc%get_child(tecnico_pointer, 'telefono', atributos_pointer, found)
            call jsonc%get(atributos_pointer, telefono)

            nodo_persona = Persona(dpi_int, nombre, apellido, genero, direccion, telefono)

            call sucursal_actual%tabla_tecnicos%insert(nodo_persona)

        end do


    
    end subroutine cargarTecnicos

    subroutine generarRecorrido()
        integer :: id_fin
        integer (kind=8) :: dpi_tecnico
        type(ResultadoRecorrido) :: resultado1, resultado2, resultadoFinal
        real :: total1, total2


        print *, "Ingrese el id de la sucursal de destino: "
        read *, id_fin

        print *, "Ingrese el DPI del tecnico que realizara el trabajo: "
        read *, dpi_tecnico

        call mapa%dijkstra_menos_distancia(sucursal_actual%valor, id_fin, resultado1)
        print *, "Datos usando el recorrido con menor distancia: "
        print *, "Costo por distancia (Q80/km): ", resultado1%distancia * 80
        print *, "Ganancia por impresoras (Q100/cu): ", resultado1%impresoras * 100
        print *, "Total: ", resultado1%impresoras * 100 - resultado1%distancia * 80 
        total1 = resultado1%impresoras * 100 - resultado1%distancia * 80
        call mapa%dijkstra_max_impresoras(sucursal_actual%valor, id_fin, resultado2)
        print *, "Datos usando el recorrido con mayor cantidad de impresoras: "
        print *, "Costo por distancia (Q80/km): ", resultado2%distancia * 80
        print *, "Ganancia por impresoras (Q100/cu): ", resultado2%impresoras * 100
        print *, "Total: ", resultado2%impresoras * 100 - resultado2%distancia * 80
        total2 = resultado2%impresoras * 100 - resultado2%distancia * 80

        if (total1 > total2) then
            print *, "El recorrido mas optimo es el que tiene menor distancia, se usara ese recorrido"
            resultadoFinal = resultado1
        else
            print *, "El recorrido mas optimo es el que tiene mayor cantidad de impresoras, se usara ese recorrido"
            resultadoFinal = resultado2
        end if

        call sucursal_actual%tabla_tecnicos%incrementar_trabajos_realizados(dpi_tecnico)

        sucursal_actual%costos = sucursal_actual%costos + resultadoFinal%distancia * 80
        sucursal_actual%ganancias = sucursal_actual%ganancias + resultadoFinal%impresoras * 100
        sucursal_actual%trabajos_realizados = sucursal_actual%trabajos_realizados + 1

    end subroutine generarRecorrido

    subroutine infoTecnico()

        integer (kind=8) :: dpi_tecnico

        print *, "Ingrese el DPI del tecnico: "
        read *, dpi_tecnico

        call sucursal_actual%tabla_tecnicos%search_graficar(dpi_tecnico)
    
    end subroutine infoTecnico

    subroutine listarTecnicos()

        call sucursal_actual%tabla_tecnicos%graficar()
    
    end subroutine listarTecnicos

    subroutine generarReporte()

        integer :: i
        integer :: io
        character(len=100) :: comando

        call sucursal_actual%tabla_tecnicos%generarReporteTecnicos()


        io = 1
        open(newunit=io, file="./info_sucursal.dot")
        comando = "dot -Tpng ./info_sucursal.dot -o ./info_sucursal.png"

        write(io, *) "digraph G {"
        write(io, *) '"Sucursal"[label="Sucursal: ', sucursal_actual%valor, '\nCosto: ', sucursal_actual%costos, '\nGanancia: ', &
        sucursal_actual%ganancias, '\nDiferencia: ', sucursal_actual%ganancias - sucursal_actual%costos, '"]'
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
            call system("start ./info_sucursal.png")
        end if
    
    end subroutine generarReporte

    subroutine menuReportes()

        integer :: opcion, id, io, i
        character(len=:), allocatable :: nombre_archivo
        type(nodo_avl), pointer :: nodo
        character(len=100) :: comando

        nombre_archivo = "GrafoSucursales"
        io = 1

        do
            print *, ""
            print *, "************MENU REPORTES************"
            print *, "0. Arbol AVL de sucursales"
            print *, "1. Grafo de sucursales"
            print *, "2. Tabla hash de tecnicos"
            print *, "3. Top 5 tecnicos con mas trabajos realizados"
            print *, "4. Gananacias y costos de la sucursal"
            print *, "5. Salir"

            print *, ""

            read *, opcion

            select case(opcion)
                case(0)
                    call sucursales%graficar_avl()
                    call system("start ./avl_imagenes.png")
                case(1)
                    call mapa%graficar_grafo(nombre_archivo)
                    call system("start ./GrafoSucursales.png")
                case(2)
                    print *, "Ingrese el ID de la sucursal de la que desea ver la tabla de tecnicos: "
                    read *, id
                    nodo => sucursales%buscar_imagen(id)

                    if (.not.associated(nodo)) then
                        print *, "No se encontro la sucursal"
                        return
                    end if

                    call nodo%tabla_tecnicos%graficar()
                case(3)
                    print *, "Ingrese el ID de la sucursal de la que desea ver el top 5 de tecnicos: "
                    read *, id
                    nodo => sucursales%buscar_imagen(id)

                    if (.not.associated(nodo)) then
                        print *, "No se encontro la sucursal"
                        return
                    end if

                    call nodo%tabla_tecnicos%generarReporteTecnicos()
                case(4)
                    print *, "Ingrese el ID de la sucursal de la que desea ver las ganancias y costos: "
                    read *, id
                    nodo => sucursales%buscar_imagen(id)

                    if (.not.associated(nodo)) then
                        print *, "No se encontro la sucursal"
                        return
                    end if


                    open(newunit=io, file="./info_sucursal.dot")
                    comando = "dot -Tpng ./info_sucursal.dot -o ./info_sucursal.png"

                    write(io, *) "digraph G {"
                    write(io, *) '"Sucursal"[label="Sucursal: ', nodo%valor, '\nCosto: ', nodo%costos, '\nGanancia: ', &
                    nodo%ganancias, '\nDiferencia: ', nodo%ganancias - nodo%costos, '"]'
                    write(io, *) "}"
                    close(io)

                    call execute_command_line(comando, exitstat=i)

                    if(i == 1) then
                        print *, "Error al momento de crear la imagen"
                    else
                        print *, "La imagen fue generada exitosamente"
                        call system("start ./info_sucursal.png")
                    end if

                case(5)
                    return
                case default
                    print *, "Opcion no valida"
            end select

        end do

    end subroutine menuReportes


    

end program main
