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
            print *, "1. Visualizar reportes de estructuras de datos"
            print *, "2. Navegacion y gestion de imagenes"
            print *, "3. Carga masiva"
            print *, "4. Salir"
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
                case default
                    print *, "Opcion no valida"
            end select
        end do
    end subroutine mostrarInicioCliente

    subroutine visualizarReportesUsuario()
        character(len=100) :: comando_abrir_imagen
        !Generamos reportes, imagenes etc
        print *, "Visualizar reportes de estructuras de datos"
        call clienteActual%arbolCapas%graficarCapa()
        comando_abrir_imagen = "start abb_capas.png"
        call system(comando_abrir_imagen)
        call clienteActual%arbolImagenes%graficar()
        comando_abrir_imagen = "start avl_imagenes.png"
        call system(comando_abrir_imagen)
        call clienteActual%listaAlbumes%graficarListaAlbumes(1)
        comando_abrir_imagen = "start lista_albumes.png"
        call system(comando_abrir_imagen)
    
    end subroutine visualizarReportesUsuario

    subroutine navegarGestionarImagenes()
        !Navegar y gestionar imagenes
        print *, "Navegacion y gestion de imagenes"
    end subroutine navegarGestionarImagenes

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
                call nodoCapa%capa%graficar()
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
            
            call clienteActual%arbolImagenes%insert(id_imagen)

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
            print *, "1. Reportes de administrador"
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
        !Generamos reportes, imagenes etc
        print *, "Reportes de administrador"
    end subroutine reportesAdmin

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