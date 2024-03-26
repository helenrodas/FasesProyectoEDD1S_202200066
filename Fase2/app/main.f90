program main
  use:: json_module
  use BTree_module
  use abb_m
  use matrix_m
  use avl_module
  use listaAlbums_module
  use listaImg_module

  implicit none
  integer :: option,size, i,dpiAsInt,io,img_album
  character(:), allocatable :: dpi, nombreCliente, password
  integer, dimension(:), allocatable :: capas
  character(len=:), allocatable :: recorrido,nombre_alb
  integer :: id,numImg,canti, n, count, pos, id_capa_int,cantidad_capas,id_imagenAmplitud,numCapas
  character(len=100) :: filename,name
  character(len=:), allocatable :: amplitud_rec, amplitud_rec2
  integer, dimension(:), allocatable :: cadena_id, cadena_preorder, cadena_inorder, cadena_posorder,ordenCapas,amplitud_int
  ! integer ::  j, id_capa, n_pixeles
  ! integer :: fila, columna
  ! character(:), allocatable :: color
  
    !Declaracion de un objeto de tipo ususario para ejemplo
    type :: usuario
      integer*8 :: dpi
      ! character(:), allocatable :: nombre,password
      character(len=100) :: nombre,password
      type(abb) :: tree
      type(avl) :: avlTree
      type(listaAlbums) :: listaAlbums
    end type usuario
  

  type(BTree), pointer :: root => null()
  ! type(abb) :: tree
  type(abb) :: arbolTemp
  type(usuario) :: usuarioTemp
  type(matrix) :: m
  type(json_file) :: json
  type(json_core) :: jsonc
  type(json_value), pointer :: listPointer, animalPointer, attributePointer
  type(json_value), pointer :: filaPointer, columnaPointer, colorPointer
  type(json_value), pointer :: pixelPointer
  type(json_value), pointer :: listaPunteroCapa, punteroCapa, atributoPunteroCapa, punteroPixel, atributoPixel
    logical :: capa_encontrada
    integer :: size_capa, contador_capa, size_pixel, contador_pixel
    character(:), allocatable :: id_capa, fila, columna, color
    integer ::  fila_int, columna_int
    integer, dimension(:), allocatable :: ids_a_buscar

  logical :: found
  io = 1
  do 
    call menu_inicial()
    read(*, *) option
    select case(option)
    case(1)
      call inicio_sesion()
    case(2)
      call registro_usuarios()
    case(3)
      exit
    case default
      print *, "Error!. Por favor seleccione una opcion valida."
    end select

  end do

  contains

  subroutine menu_inicial()
    print *, " "
    print *, "...................................."
    print *, "  Bienvenido a Pixel Print Estudio  "
    print *, "...................................."
    print *, "1. Iniciar Sesion"
    print *, "2. Registro de Usuarios"
    print *, "3. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_inicial

  !Opcion inicio de sesion como usuario normal
  subroutine inicio_sesion()
    character(len=100) :: usuario, password,dpi
    
    integer*8 :: dpiAsInt
    print *, "--------------------"
    print *, "Ingrese su usuario: "
    read*, usuario
    
    print *, "Ingrese su password: "
    read*, password

    print *, "Ingrese su dpi: "
    read*, dpi

    usuario = trim(usuario)
    password = trim(password)
    
    if (usuario == "admin" .and. password == "EDD2024") then
        call op_menuAdmin()
    else
        read(dpi, *) dpiAsInt
        usuarioTemp%dpi = dpiAsInt
        usuarioTemp%nombre = usuario
        usuarioTemp%password = password

        call op_menuUsuario()

        ! print *, "----Pruebas Matriz y arbol ABB----"
        ! call pruebaMatriz()
        
    end if
  end subroutine inicio_sesion

  subroutine menu_usuario()
    print *, " "
    print *, "...................................."
    print *, "         Bienvenido Usuario         "
    print *, "...................................."
    print *, "1. Carga masiva de archivos"
    print *, "2. Generacion de imagenes"
    print *, "3. Ver estado de las estructuras"
    print *, "4. Agregar Modificaciones"
    print *, "5. Reportes"
    print *, "6. Cerrar Sesion" !Esta pendiente para cerrar sesion pero guardar los datos de usuario
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_usuario


  subroutine op_menuUsuario()
    integer :: option
    do
      call menu_usuario()
      read(*, *) option

      select case(option)
      case(1)
        call op_CargaArchivos()
      case(2)
        call op_GenImagen()
      case(3)
        print *, "Estado estructuras"
      case(4)
        print *, "Agregar modificaciones"
      case(5)
        print *, "Agregar modificaciones"
      case(6)
            exit
          case default
            print *, "Error!. Seleccione una opcion valida."
          end select
        end do
  end subroutine op_menuUsuario


  subroutine menu_cargaArchivos()
    print *, " "
    print *, "...................................."
    print *, "      Carga masiva de Archivos      "
    print *, "...................................."
    print *, "1. Carga de Capas"
    print *, "2. Carga de Imagenes"
    print *, "3. Carga de Albums"
    print *, "4. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_cargaArchivos



  subroutine op_CargaArchivos()
    integer :: option
    do
      call menu_cargaArchivos()
      read(*, *) option
      
      select case(option)
      case(1)
        call readCapas()
      case(2)
        call readImg
      case(3)
        call readAlbum()
      case(4)
            exit
          case default
            print *, "Error!. Seleccione una opcion valida."
          end select
        end do
  end subroutine op_CargaArchivos

  subroutine menu_GenImg()
    print *, " "
    print *, "...................................."
    print *, "       Generacion de Imagenes       "
    print *, "...................................."
    print *, "1. Por recorrido limitado"
    print *, "2. Por arbol de imagenes(Amplitud)"
    print *, "3. Por capa"
    print *, "4. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_GenImg

  subroutine op_GenImagen()
    integer :: option
    do
      call menu_GenImg()
      read(*, *) option
      
      select case(option)
      case(1)
        call op_recorridoLim()
      case(2)
        call recorridoAmplitud()
      case(3)
        call recorridoCapa()
      case(4)
            exit
          case default
            print *, "Error!. Seleccione una opcion valida."
          end select
        end do
  end subroutine op_GenImagen



  subroutine menu_recorridoLimitado()
    print *, " "
    print *, "...................................."
    print *, "      Selecciona un recorrido       "
    print *, "...................................."
    print *, "1. Preorder"
    print *, "2. Inorder"
    print *, "3. Postorder"
    print *, "4. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_recorridoLimitado


  subroutine op_recorridoLim()
    integer :: option
    do
      call menu_recorridoLimitado()
      read(*, *) option
      
      select case(option)
      case(1)
        print *, "Ingrese el numero de capas a recorrer: "
        read*, cantidad_capas
        print *, "---------------------"
        cadena_preorder = usuarioTemp%tree%preorder(cantidad_capas)
        print*, "Ordenamiento Preorder"
        print*, cadena_preorder
        call usuarioTemp%tree%buscarIdGraph(cadena_preorder)
      case(2)
        print *, "Ingrese el numero de capas a recorrer: "
        read*, cantidad_capas
        print *, "---------------------"
        cadena_inorder = usuarioTemp%tree%inorder(cantidad_capas)
        print*, "Ordenamiento Inorder"
        print*, cadena_inorder
        call usuarioTemp%tree%buscarIdGraph(cadena_inorder)
      case(3)
        print *, "Ingrese el numero de capas a recorrer: "
        read*, cantidad_capas
        print *, "---------------------"
        cadena_posorder = usuarioTemp%tree%posorder(cantidad_capas)
        print*, "Ordenamiento Posorder"
        print*, cadena_posorder
        call usuarioTemp%tree%buscarIdGraph(cadena_posorder)
      case(4)
            exit
          case default
            print *, "Error!. Seleccione una opcion valida."
          end select
        end do
  end subroutine op_recorridoLim

  subroutine recorridoAmplitud()
    print *, "Ingrese el id de la imagen a buscar: "
    read*, id_imagenAmplitud
    print *, "---------------------"
    arbolTemp = usuarioTemp%avlTree%getABB(id_imagenAmplitud)
    call arbolTemp%recorrido_amplitud(amplitud_rec) !Esto me devuelve el recorrido como arbol y una cadena de char
    print *, "Amplitud: " , amplitud_rec
    cadena_id = usuarioTemp%avlTree%getABBInt(id_imagenAmplitud)!Esto me devuelve el recorrido como cadena de int
    ! print * , "Amplitud con int cadena: " , cadena_id
    call usuarioTemp%tree%buscarIdGraph(cadena_id)
  end subroutine recorridoAmplitud

  subroutine recorridoCapa()
    if (allocated(ids_a_buscar)) deallocate(ids_a_buscar)
    print *, "Ingrese cuantas capas desea graficar: "
    read*, numCapas
    allocate(ids_a_buscar(numCapas))  ! Tamaño del arreglo
    ! Leer los IDs de capas ingresados por el usuario
    do i = 1, numCapas
      print *, "Ingrese el ID de la capa ", i, ": "
      read *, ids_a_buscar(i)
  end do

  ! Imprimir los IDs de capas ingresados
  print *, "Los IDs de capas a buscar son: "
  do i = 1, numCapas
      print *, ids_a_buscar(i)
  end do

  ! Llamamos a la función buscarIdGraph con el arreglo de IDs
  call usuarioTemp%tree%buscarIdGraph(ids_a_buscar)

  end subroutine recorridoCapa

  subroutine pruebaMatriz()
    
    type(Node_t), pointer :: nodo
    nodo => usuarioTemp%tree%root
     


!Esto ya lo puedo borrar al hacer la prueba de funcionamiento
      ! print*, "--------Recorrido limitado--------"
      ! cadena_preorder = usuarioTemp%tree%preorder(4)
      ! print*, "Orden preorder"
      ! print*, cadena_preorder


      ! call usuarioTemp%tree%buscarIdGraph(cadena_preorder)

      ! cadena_posorder = usuarioTemp%tree%posorder(4)
      ! print*, "Orden posorder"
      ! print*, cadena_posorder
      ! ! call usuarioTemp%tree%buscarIdGraph(cadena_posorder)

      ! cadena_inorder = usuarioTemp%tree%inorder(4)
      ! print*, "Orden inorder"
      ! print*, cadena_inorder
      ! ! call usuarioTemp%tree%buscarIdGraph(cadena_id)

  end subroutine pruebaMatriz

  subroutine menu_estadoEstructuras()
    print *, " "
    print *, "...................................."
    print *, "      Selecciona una opcion         "
    print *, "...................................."
    print *, "1. Ver arbol de Imagenes (AVL)"
    print *, "2. Ver arbol de Capas (ABB)"
    print *, "3. Ver listado de Albumes"
    print *, "4. Ver Capa"
    print *, "5. Ver Imagen y Arbol de Capas"
    print *, "6. Ver Arbol de Clientes"
    print *, "7. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_estadoEstructuras


  subroutine op_estadoEstructuras()
    integer :: option
    do
      call menu_estadoEstructuras()
      read(*, *) option
      
      select case(option)
      case(1)
        ! usuarioTemp%avlTree%avlGraph()   
        print *, "Grafica AVL generada exitosamente "
      case(2)
        ! call usuarioTemp%tree%graph("grafica_ABB")
        print *, "Grafica ABB generada exitosamente "
      case(3)
        print *, "pendiente graficar listado de albumes "
      case(4)
        print *, "pendiente graficar capas "
      case(5)
        print *, "pendiente graficar ver imagen y arbol de capas "
      case(6)
        print *, "pendiente graficar arbol de clientes "
      case(7)
            exit
          case default
            print *, "Error!. Seleccione una opcion valida."
          end select
        end do
  end subroutine op_estadoEstructuras

  subroutine menu_admin()
    print *, "...................................."
    print *, "          Menu Administrador        "
    print *, "...................................."
    print *, "1. Grafica Arbol B Usuarios"
    print *, "2. Operaciones sobre usuarios"
    print *, "3. Carga masiva de usuarios"
    print *, "4. Cerrar Sesion"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_admin

  subroutine op_menuAdmin()
    integer :: option
    do
      call menu_admin()
      read(*, *) option
      
      select case(option)
      case(1)
        print*, "Aqui se va a generar la grafica Arbol B"
      case(2)
        print *, "Operaciones con usuarios..."
      case(3)
        call readUsuarios()
      case(4)
            exit
          case default
            print *, "Error!. Seleccione una opcion valida."
          end select
        end do
  end subroutine

  subroutine registro_usuarios()
    
    character(len=100) :: nombre,dpi,contrasena
    print *, "----------------------=---------"
    print *, "Ingrese su nombre completo: "
    read*, nombre
    print *, "Ingrese su DPI: "
    read*, dpi
    print *, "Ingrese su password: "
    read*, contrasena
    print *, "Usuario registrado exitosamente!"
    print *, "--------------------------------"
  end subroutine registro_usuarios

  subroutine readUsuarios()
    integer*8 :: dpiAsInt
    print *, "---------------------------------------"
    print *, "----- Lista Usuarios -----"
    call json%initialize()
    call json%load(filename='usuarios.json')
    call json%info('',n_children=size)
    call json%get_core(jsonc)
    call json%get('', listPointer, found)

    do i = 1, size
        call jsonc%get_child(listPointer, i, animalPointer, found)

        call jsonc%get_child(animalPointer, 'dpi', attributePointer, found)
        call jsonc%get(attributePointer, dpi)

        call jsonc%get_child(animalPointer, 'nombre_cliente', attributePointer, found)
        call jsonc%get(attributePointer, nombreCliente)

        call jsonc%get_child(animalPointer, 'password', attributePointer, found) 
        call jsonc%get(attributePointer, password)

        read(dpi, *) dpiAsInt
        call insert(root,dpiAsInt,nombreCliente,password)
        
      ! print *, "DPI: ", dpi
      ! print *, "nombre_cliente: ", nombreCliente
      ! print *, "Contrasena: ", password
    end do
    call inorder(root)
    call json%destroy()
end subroutine readUsuarios

subroutine readCapas()
  call json%initialize()
  call json%load(filename='capa.json')
  call json%info('',n_children=size_capa)
  call json%get_core(jsonc)
  call json%get('', listaPunteroCapa, capa_encontrada)
  do contador_capa = 1, size_capa
      call jsonc%get_child(listaPunteroCapa, contador_capa, punteroCapa, capa_encontrada)
      call jsonc%get_child(punteroCapa, 'id_capa', atributoPunteroCapa, capa_encontrada)
      call jsonc%get(atributoPunteroCapa, id_capa)
      call jsonc%get_child(punteroCapa, 'pixeles', atributoPunteroCapa, capa_encontrada)
      call jsonc%info(atributoPunteroCapa,n_children=size_pixel)
      
      read(id_capa, *) id_capa_int
      call usuarioTemp%tree%insert(id_capa_int)

      do contador_pixel = 1, size_pixel
          call jsonc%get_child(atributoPunteroCapa, contador_pixel, punteroPixel, capa_encontrada)
          call jsonc%get_child(punteroPixel, 'fila', atributoPixel, capa_encontrada)
          call jsonc%get(atributoPixel, fila)
          call jsonc%get_child(punteroPixel, 'columna', atributoPixel, capa_encontrada)
          call jsonc%get(atributoPixel, columna)
          call jsonc%get_child(punteroPixel, 'color', atributoPixel, capa_encontrada)
          call jsonc%get(atributoPixel, color)
          
          read(fila, *) fila_int
          read(columna, *) columna_int
          call usuarioTemp%tree%buscarId(id_capa_int,columna_int,fila_int,color)
          print*," "
          print*, "-----------------------------------------------------"
          print *, "Capa: ", id_capa, " Pixel (Fila): ", fila, " Pixel (Columna): ", columna, " Color:" ,color
      end do
  end do
  call json%destroy()
  print*," "
  print*,"Archivo Capas leido exitosamente"
  
end subroutine readCapas



subroutine readImg()
  call json%initialize()
  call json%load(filename='imagenes.json')
  call json%info('',n_children=size_capa)
  call json%get_core(jsonc)
  call json%get('', listaPunteroCapa, capa_encontrada)
  print*, "-----------------------------------------------------"
  do contador_capa = 1, size_capa
      call jsonc%get_child(listaPunteroCapa, contador_capa, punteroCapa, capa_encontrada)
      call jsonc%get_child(punteroCapa, 'id', atributoPunteroCapa, capa_encontrada)
      call jsonc%get(atributoPunteroCapa, id)
      call jsonc%get_child(punteroCapa, 'capas', atributoPunteroCapa, capa_encontrada)
      call jsonc%info(atributoPunteroCapa,n_children=size_pixel)
      allocate(capas(size_pixel))

      call usuarioTemp%avlTree%insert(id)
      do contador_pixel = 1, size_pixel
          call jsonc%get_child(atributoPunteroCapa, contador_pixel, punteroPixel, capa_encontrada)
          call jsonc%get(punteroPixel, capas(contador_pixel))
      end do

      print*, "ID: ", id, "Capas: ", capas
      
      call usuarioTemp%avlTree%insertInABB(id,capas)
      
      deallocate(capas)
  end do

 
  print*," "
  print*,"Archivo Imagenes leido exitosamente"
  call json%destroy()
end subroutine readImg


subroutine readAlbum()
  type(listaImagenes),pointer::ImagenesList
  call json%initialize()
  call json%load(filename='album.json')
  call json%info('',n_children=size_capa)
  call json%get_core(jsonc)
  call json%get('', listaPunteroCapa, capa_encontrada)

  do contador_capa = 1, size_capa
      call jsonc%get_child(listaPunteroCapa, contador_capa, punteroCapa, capa_encontrada)
      call jsonc%get_child(punteroCapa, 'nombre_album', atributoPunteroCapa, capa_encontrada)
      call jsonc%get(atributoPunteroCapa, nombre_alb)
      call jsonc%get_child(punteroCapa, 'imgs', atributoPunteroCapa, capa_encontrada)
      call jsonc%info(atributoPunteroCapa,n_children=size_pixel)
      name = nombre_alb
      allocate(ImagenesList)
      do contador_pixel = 1, size_pixel
          call jsonc%get_child(atributoPunteroCapa, contador_pixel, punteroPixel, capa_encontrada)
          call jsonc%get(punteroPixel, img_album)
          call ImagenesList%push(img_album)
      end do
      call usuarioTemp%listaAlbums%add(name,ImagenesList)
      ! print*, "Nombre Album: ", nombre_alb, " img: ", img_album
      deallocate(ImagenesList)
      ! call usuarioTemp%listaAlbums%push(nombre_alb)
  end do
  call usuarioTemp%listaAlbums%print()
  call json%destroy()
end subroutine readAlbum


end program main


