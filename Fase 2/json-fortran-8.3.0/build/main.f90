program main
    use datos_m
    use json_module
    implicit none

    integer :: opcion

    do

        call mostrarMenuInicial()

        ! Leer la opción seleccionada
        print *, ""
        print *, "Seleccione una opcion:"
        read(*, *) opcion
        print *, ""

        select case(opcion)
            case(1)
                call iniciarSesion()
            case(2)
                call registrarse()
            case(3)
                exit
            case default
                print *, "Opcion no valida"
        end select

    end do



    contains

    subroutine mostrarMenuInicial()
        print *, "--------Bienvenido a la aplicacion--------"
        print *, "1. Iniciar Sesion"
        print *, "2. Registrarse"
        print *, "3. Salir"
    end subroutine mostrarMenuInicial


    subroutine registrarse()
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: contrasena
        integer (kind=8) :: dpi
        character(len=100) :: input_line

        nombre = ""
        contrasena = ""

        print *, "Ingrese su nombre:"
        read(*,*) input_line
        nombre = trim(input_line)

        print *, "Ingrese su DPI:"
        read(*,*) dpi

        print *, "Ingrese su contrasena:"
        read(*,*) input_line
        contrasena = trim(input_line)

        print *, "Registrando usuario..."
        print *, "Nombre: ", nombre
        print *, "DPI: ", dpi
        print *, "Contrasena: ", contrasena


        call arbolClientes%insertar(dpi, nombre, contrasena)

    end subroutine registrarse

    subroutine iniciarSesion()
        integer (kind=8) :: dpi
        character(len=256) :: contrasena
        type(nodo_cliente), pointer :: cliente
        character(len=100) :: input_line
        character(len=100) :: usuario

        contrasena = ""

        print *, "Ingrese su DPI:"
        read(*,'(A100)') input_line
        usuario = trim(input_line)

        print *, "Ingrese su contrasena:"
        read(*,'(A256)') input_line
        contrasena = trim(input_line)

        !Verificamos si el usuario es admin
        if (usuario == "admin") then
            if (contrasena == "EDD2024") then
                call mostrarInicioAdmin()
            else
                print *, "Contrasena incorrecta"
            end if
            return
        else
            !convertimos el usuario a entero
            read(usuario, *) dpi
        end if
        

        cliente => arbolClientes%buscar(dpi)

        if (associated(cliente)) then
            if (trim(cliente%contrasena) == trim(contrasena)) then
                clienteActual => cliente
                call mostrarInicioCliente()
            else
                print *, "Contrasena incorrecta"
                print *, "contrasena ingresada: ", contrasena
                print *, "contrasena almacenada: ", cliente%contrasena
            end if
        else
            print *, "Usuario con el dpi ingresado no encontrado"
        end if

    end subroutine iniciarSesion

    subroutine mostrarInicioCliente()
        integer :: opcion
        do
            print *, ""
            print *, "--------Bienvenido ", clienteActual%nombre, "--------"
            print *, "1. Visualizar reportes y estructuras de datos"
            print *, "2. Navegacion y gestion de imagenes"
            print *, "3. Carga masiva"
            print *, "4. Salir"
            print *, "5. Eliminar cuenta"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call visualizarReportesUsuario()
                case(2)
                    call navegarGestionarImagenes()
                case(3)
                    call cargaMasivaUsuario()
                case(4)
                    clienteActual => null()
                    exit
                case(5)
                    call eliminarCuenta()
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine mostrarInicioCliente

    subroutine eliminarCuenta

        integer (kind = 8) :: dpi
        type(nodo_cliente), pointer :: cliente_eliminar
        print *, ""
        print *, "*** Eliminar cuenta ***"
        print *, "Ingrese su DPI para confirmar la eliminacion de su cuenta:"
        read(*,*) dpi

        cliente_eliminar => arbolClientes%buscar(dpi)

        if (associated(cliente_eliminar) .and. clienteActual%dpi == cliente_eliminar%dpi) then
                call arbolClientes%eliminar(dpi)
                print *, "Cuenta eliminada exitosamente"
                clienteActual => null()
        else
            print *, "Datos incorrectos, se cerrara la sesion pero no se eliminara la cuenta"
        end if

    end subroutine eliminarCuenta

    subroutine visualizarReportesUsuario()
        integer Opcion

        !Las opciones serán, reportes y visualiar estructuras

        do
            print *, ""
            print *, "--------Visualizar reportes y estructuras de datos--------"
            print *, "1. Reportes"
            print *, "2. Visualizar estructuras"
            print *, "3. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call reportes_usuario()
                case(2)
                    call visualizar_estructuras()
                case(3)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do

    end subroutine visualizarReportesUsuario

    subroutine reportes_usuario()
        integer :: io
        integer :: i
        character(len=100) :: comando, comando_abrir_imagen

        io = 1
        open(newunit=io, file="./reporteUsuario.dot")
        comando = "dot -Tpng ./reporteUsuario.dot -o ./reporteUsuario.png"
        comando_abrir_imagen = "start reporteUsuario.png"

        write(io, *) "digraph G {"

        !subgrafo top 5 imagenes con mas capas
        call clienteActual%arbolImagenes%topImagenes(io)
        call clienteActual%arbolCapas%obtenerNodoHoja(io)
        call clienteActual%arbolCapas%obtenerProfundidad(io)
        call clienteActual%arbolCapas%listarCapasOrdenes(io)

        write(io, *) "rankdir=LR"

        write(io,*) "}"

        close(io)

        call execute_command_line(comando, exitstat=i)


        if (i == 0) then
            print *, "Reporte generado exitosamente"
            call system(comando_abrir_imagen)
        else
            print *, "Error al generar el reporte"
        end if


    end subroutine reportes_usuario

    subroutine visualizar_estructuras()
        character(len=100) :: comando_abrir_imagen
        integer :: opcion
        integer :: id_capa, id_imagen
        type(nodo_avl), pointer :: nodoImagen
        type(nodo), pointer :: nodoCapa

        !Las opciones seran arbol avl imagenes, arbol abb capas, lista doble circular albumes
        !Matriz dispersa capa, imagen y arbol capas
        do
            print *, ""
            print *, "--------Visualizar estructuras--------"
            print *, "1. Arbol AVL de imagenes"
            print *, "2. Arbol ABB de capas"
            print *, "3. Lista doble circular de albumes"
            print *, "4. Matriz dispersa de capa"
            print *, "5. AVL de imagenes y ABB de capa de una imagen"
            print *, "6. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call clienteActual%arbolImagenes%graficar_avl()
                    comando_abrir_imagen = "start avl_imagenes.png"
                    call system(comando_abrir_imagen)
                    
                case(2)
                    call clienteActual%arbolCapas%graficarCapa()
                    comando_abrir_imagen = "start abb_capas.png"
                    call system(comando_abrir_imagen)
                case(3)
                    call clienteActual%listaAlbumes%graficarListaAlbumes(1)
                    comando_abrir_imagen = "start lista_albumes.png"
                    call system(comando_abrir_imagen)
                case(4)
                    print *, "Ingrese el id de la capa a visualizar:"
                    read(*, *) id_capa
                    nodoCapa => clienteActual%arbolCapas%buscarCapa(id_capa)
                    if (associated(nodoCapa)) then
                        call nodoCapa%capa%graficar()
                        comando_abrir_imagen = "start matrix_imagen.png"
                        call system(comando_abrir_imagen)
                    else
                        print *, "Capa no encontrada"
                    end if
                case(5)
                    print *, "Ingrese el id de la imagen a visualizar:"
                    read(*, *) id_imagen
                    nodoImagen => clienteActual%arbolImagenes%buscar_imagen(id_imagen)
                    if (associated(nodoImagen)) then
                        call clienteActual%arbolImagenes%graficar_avl_imagen(id_imagen)
                        comando_abrir_imagen = "start imagen_capa.png"
                        call system(comando_abrir_imagen)
                    else
                        print *, "Imagen no encontrada"
                    end if
                case(6)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine visualizar_estructuras

    subroutine navegarGestionarImagenes()
        integer :: opcion
        !Las opciones serán Generacion de imagenes, eliminacion de imagenes
        do
            print *, ""
            print *, "--------Navegacion y gestion de imagenes--------"
            print *, "1. Generar imagen"
            print *, "2. Eliminar imagen"
            print *, "3. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call generarImagenM()
                case(2)
                    call eliminarImagen()
                case(3)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine navegarGestionarImagenes

    subroutine generarImagenM()

        integer :: opcion
        integer :: cantidad_capas

        !Las opcioes para generar imagenes erán, por recorrido limitado, por arbol de imagenes, por capa
        do
            print *, ""
            print *, "--------Generar imagen--------"
            print *, "1. Por recorrido limitado"
            print *, "2. Por arbol de imagenes"
            print *, "3. Por capa"
            print *, "4. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    print *, "Ingrese la cantidad de capas a utilizar:"
                    read(*, *) cantidad_capas
                    call generarImagenRecorridoLimitado(cantidad_capas)
                case(2)
                    print *, "Ingrese el id de la imagen a generar:"
                    read(*, *) cantidad_capas
                    call generarImagenArbolImagenes(cantidad_capas)
                case(3)
                    print *, "Ingrese la cantidad de capas a utilizar:"
                    read(*, *) cantidad_capas
                    call generarImagenPorCapa(cantidad_capas)
                case(4)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do

    end subroutine generarImagenM

    subroutine generarImagenRecorridoLimitado(cantidad_capas)
        integer, intent(in) :: cantidad_capas
        integer :: opcion

        !Las opciones para el recorrido limitado serán, inorden, preorden, postorden
        do
            print *, ""
            print *, "--------Generar imagen por recorrido limitado--------"
            print *, "1. Preorden"
            print *, "2. Inodrden"
            print *, "3. Postorden"
            print *, "4. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call clienteActual%arbolCapas%obtenerRecorridoPreorden(cantidad_capas)
                case(2)
                    call clienteActual%arbolCapas%obtenerRecorridoInorden(cantidad_capas)
                case(3)
                    call clienteActual%arbolCapas%obtenerRecorridoPostorden(cantidad_capas)
                case(4)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do

    end subroutine generarImagenRecorridoLimitado

    subroutine generarImagenArbolImagenes(id_imagen)
        integer, intent(in) :: id_imagen
        type(nodo_avl), pointer :: nodoImagen

        nodoImagen => clienteActual%arbolImagenes%buscar_imagen(id_imagen)

        if (associated(nodoImagen)) then
            call nodoImagen%capas%obtenerRecorridoNiveles()
        else
            print *, "Imagen no encontrada"
        end if

    end subroutine generarImagenArbolImagenes

    subroutine generarImagenPorCapa(cantidad_capas)
        integer, intent(in) :: cantidad_capas
        integer :: contador, id_capa, i, j
        type(nodo), pointer :: nodoCapa
        type(matrix) :: matriz_general
        type(node_matriz_val) :: val
        character(:), allocatable :: nombre_archivo
        character(len=5) :: id_capa_str
        character(len=7) :: color_a
        character(len=256):: comando_abrir_imagen

        nombre_archivo = "Capas: "

        contador = 0

        do while (contador < cantidad_capas)
            print *, "Ingrese el id de la capa a utilizar:"
            read(*, *) id_capa

            nodoCapa => clienteActual%arbolCapas%buscarCapa(id_capa)

            if (associated(nodoCapa)) then

                write(id_capa_str, '(I5)') id_capa
                nombre_archivo = trim(adjustl(nombre_archivo)) // trim(adjustl(id_capa_str)) // ","

                do j = 0, nodoCapa%capa%height
                    do i = 0, nodoCapa%capa%width
                        val = nodoCapa%capa%obtenerValor(i, j)
                        if (val%exists) then
                            write(color_a, '(A7)') val%valor
                            call matriz_general%insert(i, j, color_a)
                        end if
                    end do
                end do
                
                contador = contador + 1
            else
                print *, "Capa no encontrada"
            end if
        end do

        call matriz_general%generarImagen(nombre_archivo)
        comando_abrir_imagen = "start ImagenGenerada.png"
        call system(comando_abrir_imagen)

    end subroutine generarImagenPorCapa


    subroutine eliminarImagen()
        integer :: id_imagen
        print *, "Ingrese el id de la imagen a eliminar: "
        read(*, *) id_imagen

        call clienteActual%arbolImagenes%delete(id_imagen)
        call clienteActual%listaAlbumes%eliminarNodoListaSimple(id_imagen)

        print *, "Imagen eliminada exitosamente"
        
    end subroutine eliminarImagen

    subroutine cargaMasivaUsuario()
        integer :: opcion
        !Las opciones seran, carga masiva de capas, carga masiva de imagenes y carga masiva de albumes
        do
            print *, ""
            print *, "--------Carga masiva--------"
            print *, "1. Carga masiva de capas"
            print *, "2. Carga masiva de imagenes"
            print *, "3. Carga masiva de albumes"
            print *, "4. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call cargaMasivaCapas()
                case(2)
                    call cargaMasivaImagenes()
                case(3)
                    call cargaMasivaAlbumes()
                case(4)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine cargaMasivaUsuario

    subroutine cargaMasivaCapas()
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: lista_capas_pointer, capa_pointer, atributos_pointer, atributos_pixel_pointer, pixeles_pointer
        character(len=100) :: file_path
        integer :: size_capas, size_pixeles
        logical :: found
        integer :: i, j, columna, fila, id
        character(len=:), allocatable :: color
        type(nodo), pointer :: nodoCapa

        print *, "Ingrese la ruta del archivo JSON con las capas:"
        read(*,*) file_path

        call json%initialize()
        call json%load(filename=file_path)
        call json%info('', n_children=size_capas)

        call json%get_core(jsonc)
        call json%get('', lista_capas_pointer, found)

        do i = 1, size_capas
            !Leemos cada uno de los elementos de la lista
            call jsonc%get_child(lista_capas_pointer, i, capa_pointer, found)
            
            call jsonc%get_child(capa_pointer, 'id_capa', atributos_pointer, found)
            call jsonc%get(atributos_pointer, id)

            !Creamos el nodo en el abb
            call clienteActual%arbolCapas%insertarCapa(id)

            nodoCapa => clienteActual%arbolCapas%buscarCapa(id)

            !obtenemos cuantos hijos tiene el atributo pixeles
            call jsonc%get_child(capa_pointer, 'pixeles', atributos_pointer, found)
            call jsonc%info(atributos_pointer, n_children=size_pixeles)

            !Leemos cada uno de los pixeles
            do j = 1, size_pixeles
                call jsonc%get_child(atributos_pointer, j, pixeles_pointer, found)
                
                call jsonc%get(pixeles_pointer, 'fila', atributos_pixel_pointer, found)
                call jsonc%get(atributos_pixel_pointer, fila)

                call jsonc%get(pixeles_pointer, 'columna', atributos_pixel_pointer, found)
                call jsonc%get(atributos_pixel_pointer, columna)

                call jsonc%get(pixeles_pointer, 'color', atributos_pixel_pointer, found)
                call jsonc%get(atributos_pixel_pointer, color)


                call nodoCapa%capa%insert(columna, fila, color)
            end do
        end do
        
    end subroutine cargaMasivaCapas

    subroutine cargaMasivaImagenes
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: lista_imagenes_pointer, imagen_pointer, atributos_pointer
        type(json_value), pointer :: atributos_capa_pointer, capa_pointer, datos_id_imagen
        character(len=100) :: file_path
        integer :: size_imagenes, size_capas
        logical :: found
        integer :: i, j, id_capa, id_imagen
        type(nodo), pointer :: nodoCapa, nodoCapaNuevo, imagenExistente
        type(nodo_avl), pointer :: nodoImagen

        print *, "Ingrese la ruta del archivo JSON con las imagenes:"
        read(*,*) file_path

        call json%initialize()
        call json%load(filename=file_path)
        call json%info('', n_children=size_imagenes)

        call json%get_core(jsonc)
        call json%get('', lista_imagenes_pointer, found)

        do i = 1, size_imagenes
            !Leemos cada uno de los elementos de la lista
            call jsonc%get_child(lista_imagenes_pointer, i, imagen_pointer, found)
            
            call jsonc%get_child(imagen_pointer, 'id', atributos_pointer, found)
            call jsonc%get(atributos_pointer, id_imagen)

            !Creamos el nodo en el avl
            
            call clienteActual%arbolImagenes%inserta_avl(id_imagen)

            nodoImagen => clienteActual%arbolImagenes%buscar_imagen(id_imagen)

            !obtenemos cuantos hijos tiene el atributo pixeles
            call jsonc%get_child(imagen_pointer, 'capas', capa_pointer, found)
            call jsonc%info(capa_pointer, n_children=size_capas)


            !Leemos cada uno de los pixeles
            do j = 1, size_capas
                call jsonc%get_child(capa_pointer, j, atributos_capa_pointer, found)
                
                call jsonc%get(atributos_capa_pointer, '', datos_id_imagen, found)
                call jsonc%get(datos_id_imagen, id_capa)

                nodoCapa => clienteActual%arbolCapas%buscarCapa(id_capa)
                if(associated(nodoCapa)) then
                    !Verificamos que la capa no exista ya en la imagen
                    imagenExistente => nodoImagen%capas%buscarCapa(id_capa)
                    if (.not. associated(imagenExistente)) then
                        call nodoImagen%capas%insertarCapa(id_capa)
                        nodoCapaNuevo => nodoImagen%capas%buscarCapa(id_capa)
                        nodoCapaNuevo%capa = nodoCapa%capa
                        nodoImagen%cantidad_capas = nodoImagen%cantidad_capas + 1
                    else
                        print *, "Capa con id ", id_capa, " ya existe en la imagen, se omitira"
                    end if
                else
                    print *, "Capa con id ", id_capa, " no encontrada, se omitira"
                end if
            end do
        end do
        
    end subroutine cargaMasivaImagenes

    subroutine cargaMasivaAlbumes

        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: lista_albumes_pointer, album_pointer, atributos_pointer
        type(json_value), pointer :: atributos_imgs_pointer, img_pointer, datos_id_imagen
        character(len=100) :: file_path
        integer :: size_albumes, size_imgs
        logical :: found
        integer :: i, j, id_imagen
        type(nodo_avl), pointer :: nodoImagen
        type(nodo_lista_doble_circular), pointer :: nodoAlbum
        type(nodo_lista_enlazada_simple), pointer :: imagenExistente, imagenNueva
        character(len=:), allocatable :: nombre_album

        print *, "Ingrese la ruta del archivo JSON con las imagenes:"
        read(*,*) file_path

        call json%initialize()
        call json%load(filename=file_path)
        call json%info('', n_children=size_albumes)

        call json%get_core(jsonc)
        call json%get('', lista_albumes_pointer, found)

        do i = 1, size_albumes
            !Leemos cada uno de los elementos de la lista
            call jsonc%get_child(lista_albumes_pointer, i, album_pointer, found)
            
            call jsonc%get_child(album_pointer, 'nombre_album', atributos_pointer, found)
            call jsonc%get(atributos_pointer, nombre_album)

            print *, "Cargando album ", nombre_album

            !verificamos que el album no exista
            nodoAlbum => clienteActual%listaAlbumes%buscar_nodo_por_id(nombre_album)


            if (.not. associated(nodoAlbum)) then
                call clienteActual%listaAlbumes%agregar_nodo_lista_doble_circular(nombre_album)
                nodoAlbum => clienteActual%listaAlbumes%buscar_nodo_por_id(nombre_album)
            end if


            !obtenemos cuantos hijos tiene el atributo pixeles
            call jsonc%get_child(album_pointer, 'imgs', img_pointer, found)
            call jsonc%info(img_pointer, n_children=size_imgs)

            print *, "Cargando imagenes al album ", nombre_album

            !Leemos cada uno de los pixeles
            do j = 1, size_imgs
                call jsonc%get_child(img_pointer, j, atributos_imgs_pointer, found)
                
                call jsonc%get(atributos_imgs_pointer, '', datos_id_imagen, found)
                call jsonc%get(datos_id_imagen, id_imagen)

                print *, "Cargando imagen ", id_imagen

                nodoImagen => clienteActual%arbolImagenes%buscar_imagen(id_imagen)
                if(associated(nodoImagen)) then
                    !Verificamos que la imagen no exista ya en el album
                    imagenExistente => clienteActual%listaAlbumes%buscar_nodo_imagen(nodoAlbum, id_imagen)
                    if (.not. associated(imagenExistente)) then
                        call clienteActual%listaAlbumes%agregar_nodo_lista_enlazada_simple(nodoAlbum, id_imagen)
                        imagenNueva => clienteActual%listaAlbumes%buscar_nodo_imagen(nodoAlbum, id_imagen)
                        imagenNueva%imagen_avl => nodoImagen
                        nodoAlbum%cantidad_imagenes = nodoAlbum%cantidad_imagenes + 1
                    else
                        print *, "Imagen con id ", id_imagen, " ya existe en el album, se omitira"
                    end if
                else
                    print *, "Imagen con id ", id_imagen, " no encontrada, se omitira"
                end if
            end do
        end do
        
    end subroutine cargaMasivaAlbumes


    subroutine mostrarInicioAdmin()
        integer :: opcion
        do
            print *, ""
            print *, "--------Bienvenido Admin--------"
            print *, "1. Ver reportes y estructuras de datos"
            print *, "2. Operaciones con usuarios"
            print *, "3. Carga masiva de usuarios"
            print *, "4. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call reportesAdmin()
                case(2)
                    call operacionesConUsuarios()
                case(3)
                    call cargaMasivaUsuarios()
                case(4)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine mostrarInicioAdmin

    subroutine reportesAdmin()
        !Opciones ver estructuras y reportes
        integer :: opcion
        do
            print *, ""
            print *, "--------Reportes y estructuras de datos--------"
            print *, "1. Visualizar estructuras"
            print *, "2. Reportes"
            print *, "3. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call visualizar_estructuras_administrador()
                case(2)
                    call reportes_admin()
                case(3)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine reportesAdmin

    subroutine visualizar_estructuras_administrador()

        character(len=100) :: comando_abrir_imagen
        comando_abrir_imagen = "start arbolB.jpg"
        call system(comando_abrir_imagen)

    end subroutine visualizar_estructuras_administrador

    subroutine reportes_admin()
        integer :: opcion
        !Las opciones serán generar informe de cliente y listar clientes
        do
            print *, ""
            print *, "--------Reportes--------"
            print *, "1. Generar informe de cliente"
            print *, "2. Listar clientes"
            print *, "3. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call generarInformeCliente()
                case(2)
                    call listarClientes()
                case(3)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do

    end subroutine reportes_admin

    subroutine generarInformeCliente()

        integer(kind=8) :: dpi
        type(nodo_cliente), pointer :: cliente
        character(len=100) :: comando_abrir_imagen
        integer :: io
        character(len=100) :: comando
        character(len=50) :: dpi_str
        integer :: cantidad_albumes, cantidad_imagenes, cantidad_capas
        character(len=12) :: cantidad_albumes_str, cantidad_imagenes_str, cantidad_capas_str

        integer :: i
        io = 1

        print *, "Ingrese el DPI del cliente a generar el informe:"
        read(*, *) dpi

        cliente => arbolClientes%buscar(dpi)

        if (associated(cliente)) then
            open(newunit=io, file="./informeCliente.dot")
            comando = "dot -Tpng ./informeCliente.dot -o ./informeCliente.png"
            write(io, *) "digraph G {"
            
            write(dpi_str, '(I50)') cliente%dpi
            write(io, *) '"dpi"'// '[label="DPI: '// trim(adjustl(dpi_str)) // &
            ' \n Nombre: '// cliente%nombre // &
            ' \n Contrasena: '// cliente%contrasena // '"]'

            cantidad_albumes = cliente%listaAlbumes%contar_albumes()
            cantidad_imagenes = cliente%arbolImagenes%contarNodos()
            cantidad_capas = cliente%arbolCapas%contarCapas()

            write(cantidad_albumes_str, '(I12)') cantidad_albumes
            write(cantidad_imagenes_str, '(I12)') cantidad_imagenes
            write(cantidad_capas_str, '(I12)') cantidad_capas

            write(io, *) '"cantidad_albumes"'// '[label="Cantidad de albumes: '//&
             trim(adjustl(cantidad_albumes_str)) // '"]'

            write(io, *) '"cantidad_imagenes"'// '[label="Cantidad de imagenes: '//&
                trim(adjustl(cantidad_imagenes_str)) // '"]'

            write(io, *) '"cantidad_capas"'// '[label="Cantidad de capas: '//&
                trim(adjustl(cantidad_capas_str)) // '"]'



            write(io, *) "}"
            close(io)

            call execute_command_line(comando, exitstat=i)

            if (i == 0) then
                print *, "Informe generado exitosamente"
                comando_abrir_imagen = "start informeCliente.png"
            call system(comando_abrir_imagen)
            else
                print *, "Error al generar el informe"
            end if

        else
            print *, "Cliente con el dpi ingresado no encontrado"
        end if

    end subroutine generarInformeCliente

    subroutine listarClientes()

        integer :: io
        type(nodo_cliente), pointer :: cliente
        character(len=100) :: comando
        character(len=100) :: comando_abrir_imagen
        integer :: i, cantidad_imagenes
        character(len=50) :: cantidad_imagenes_str, dpi_str
        io = 1

        open(newunit=io, file="./listaClientes.dot")
        comando = "dot -Tpng ./listaClientes.dot -o ./listaClientes.png"
        write(io, *) "digraph G {"
        write(io, *) "rankdir=LR"

        cliente => arbolClientes%head
        !Mostraremos nombre, dpi y cantidad de imagenes
        do while (associated(cliente))
            write(dpi_str, '(I50)') cliente%dpi
            write(cantidad_imagenes_str, '(I50)') cliente%arbolImagenes%contarNodos()
            write(io, *) '"dpi'// trim(adjustl(dpi_str)) // '" [label="DPI: '// trim(adjustl(dpi_str)) // &
                ' \n Nombre: '// cliente%nombre // &
                ' \n Cantidad de imagenes: '// trim(adjustl(cantidad_imagenes_str)) // '"]'
            cliente => cliente%next
        end do

        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if (i == 0) then
            print *, "Lista de clientes generada exitosamente"
            comando_abrir_imagen = "start listaClientes.png"
            call system(comando_abrir_imagen)
        else
            print *, "Error al generar la lista de clientes"
        end if

    end subroutine listarClientes

    subroutine operacionesConUsuarios()
        integer :: opcion
        do
            print *, ""
            print *, "--------Operaciones con usuarios--------"
            print *, "1. Crear usuario"
            print *, "2. Modificar usuario"
            print *, "3. Eliminar usuario"
            print *, "4. Salir"
            print *, ""
            print *, "Seleccione una opcion:"
            read(*, *) opcion
            print *, ""

            select case(opcion)
                case(1)
                    call crearUsuario()
                case(2)
                    call modificarUsuario()
                case(3)
                    call eliminarUsuario()
                case(4)
                    exit
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine operacionesConUsuarios

    subroutine crearUsuario()
        print *, ""
        print *, "*** Crear usuario ***"
        call registrarse()
    end subroutine crearUsuario

    subroutine modificarUsuario()
        integer (kind=8) :: dpi
        character(len=100) :: input_line
        type(nodo_cliente), pointer :: cliente_modificar
        print *, ""
        print *, "*** Modificar usuario ***"
        !El flujo de modificar usuario es similar a crear usuario
        !Primero buscar el usuario y luego modificar sus datos
        print *, "Ingrese el DPI del usuario a modificar:"
        read(*,*) dpi

        cliente_modificar => arbolClientes%buscar(dpi)

        if (associated(cliente_modificar)) then
            print *, "Ingrese el nuevo nombre:"
            read(*,'(A100)') input_line
            cliente_modificar%nombre = trim(input_line)

            print *, "Ingrese la nueva contrasena:"
            read(*,'(A100)') input_line
            cliente_modificar%contrasena = trim(input_line)

            print *, "Usuario modificado exitosamente"
        else
            print *, "Usuario con el dpi ingresado no encontrado"
        end if

    end subroutine modificarUsuario

    subroutine eliminarUsuario()
        integer (kind=8) :: dpi
        type(nodo_cliente), pointer :: cliente_eliminar
        print *, ""
        print *, "*** Eliminar usuario ***"
        print *, "Ingrese el DPI del usuario a eliminar:"
        read(*,*) dpi

        cliente_eliminar => arbolClientes%buscar(dpi)

        if (associated(cliente_eliminar)) then
            call arbolClientes%eliminar(dpi)
            print *, "Usuario eliminado exitosamente"
        else
            print *, "Usuario con el dpi ingresado no encontrado"
        end if

    end subroutine eliminarUsuario

    subroutine cargaMasivaUsuarios()
        use json_module ! Asumiendo que estás utilizando un módulo para manejar JSON
        implicit none
        
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: lista_usuarios_pointer, usuario_pointer, atributos_pointer
        character(len=256) :: file_path
        integer :: size_usuarios, i, error
        logical :: found
        character(len=:), allocatable :: dpi_str, nombre, contrasena
        integer (kind=8) :: dpi_int
        
        print *, "Ingrese la ruta del archivo JSON con los usuarios:"
        read(*,*) file_path
        
        call json%initialize()
        call json%load(filename=file_path)
        call json%info('', n_children=size_usuarios)
        
        call json%get_core(jsonc)
        call json%get('', lista_usuarios_pointer, found)
        
        print *, "Cargando usuarios..."
        
        do i = 1, size_usuarios
            dpi_str = ""
            nombre = ""
            contrasena = ""
            
            print *, "Cargando usuario ", i
            !leemos cada uno de los elementos de la lista
            call jsonc%get_child(lista_usuarios_pointer, i, usuario_pointer, found)
            if (.not. found) then
                print *, "Error: Usuario no encontrado"
                exit ! Salir del bucle si el usuario no se encontró
            end if
            
            call jsonc%get_child(usuario_pointer, 'dpi', atributos_pointer, found)
            if (found) then
                call jsonc%get(atributos_pointer, dpi_str)
                print *, "DPI: ", dpi_str
                read(dpi_str, *, iostat=error) dpi_int
                if (error /= 0) then
                    print *, "Error de conversión de DPI"
                    exit ! Salir del bucle si hay un error de conversión
                end if
            else
                print *, "Error: DPI no encontrado"
                exit ! Salir del bucle si DPI no se encontró
            end if
            
            call jsonc%get_child(usuario_pointer, 'nombre_cliente', atributos_pointer, found)
            if (found) then
                call jsonc%get(atributos_pointer, nombre)
                print *, "Nombre: ", nombre
            else
                print *, "Error: Nombre no encontrado"
                exit ! Salir del bucle si Nombre no se encontró
            end if
            
            call jsonc%get_child(usuario_pointer, 'password', atributos_pointer, found)
            if (found) then
                call jsonc%get(atributos_pointer, contrasena)
                print *, "Contrasena: ", contrasena
            else
                print *, "Error: Contraseña no encontrada"
                exit ! Salir del bucle si Contraseña no se encontró
            end if
            
            call arbolClientes%insertar(dpi_int, nombre, contrasena)
            
            print *, "Usuario ", nombre, " cargado exitosamente"
        end do
   
    end subroutine cargaMasivaUsuarios
    


end program main