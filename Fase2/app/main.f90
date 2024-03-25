program main
  use:: json_module
  use BTree_module
  use abb_m
  use matrix_m
  use avl_module

  implicit none
  integer :: option,size, i,dpiAsInt,io
  character(:), allocatable :: dpi, nombreCliente, password
  integer, dimension(:), allocatable :: capas
  character(len=:), allocatable :: recorrido,nombre_alb
  integer :: id,numImg,canti, n, count, pos, id_capa_int
  character(len=100) :: filename
  character(len=:), allocatable :: amplitud_rec, amplitud_rec2
  integer, dimension(:), allocatable :: cadena_id, cadena_preorder, cadena_inorder, cadena_posorder,ordenCapas,album,amplitud_int
  ! integer ::  j, id_capa, n_pixeles
  ! integer :: fila, columna
  ! character(:), allocatable :: color
  
    !Declaracion de un objeto de tipo ususario para ejemplo
    type :: usuario
      integer*8 :: dpi
      ! character(:), allocatable :: nombre,password
      character(len=100) :: nombre,password
      ! character(len=20) :: password
      type(abb) :: tree
      type(avl) :: avlTree
      ! type(double_linkedList) :: listaAlbums
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
        ! print *, "Cliente agregado!"
        call readCapas()

        print *, "----Pruebas Matriz y arbol ABB----"
        call pruebaMatriz()

        print *, "----Pruebas Arbol AVL----"
        call readImg()
        print*, "--------Recorrido por arbol de imagenes--------"
        arbolTemp = usuarioTemp%avlTree%getABB(3)
        call arbolTemp%recorrido_amplitud(amplitud_rec)
        
        print *, "Amplitud: " , amplitud_rec

        cadena_id = usuarioTemp%avlTree%getABBInt(3)
        ! print * , "Amplitud con int cadena: " , cadena_id
        call usuarioTemp%tree%buscarIdGraph(cadena_id)


        ! call usuarioTemp%tree%buscarIdGraph(cadena_id)

        ! print *, "----Pruebas Albums----"
        ! call readAlbum()
        
        
    end if
  end subroutine inicio_sesion


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





  subroutine pruebaMatriz()
    
    type(Node_t), pointer :: nodo
    nodo => usuarioTemp%tree%root
      ! Aqui aplica varias capas pero en este caso es una misma matriz solo para probar
      call usuarioTemp%tree%insert(1)  !esto es como que si estuviera insertando el indice de la capa, falta la matriz
      call usuarioTemp%tree%insert(3)
      call usuarioTemp%tree%insert(5)
      call usuarioTemp%tree%insert(2)
      call usuarioTemp%tree%insert(10)
      call usuarioTemp%tree%insert(10)
      print*, "Arbol tiene datos!"
      

      call usuarioTemp%tree%buscarId(1,3,4,"#9db9f0")
      call usuarioTemp%tree%buscarId(1,5,0,"#fdce80")
      call usuarioTemp%tree%buscarId(1,2,6,"#fdce80")
      call usuarioTemp%tree%buscarId(3,6,6,"#fdce80")
      call usuarioTemp%tree%buscarId(3,1,4,"#9db9f0")
      call usuarioTemp%tree%buscarId(10,3,7,"#fdce80")
      call usuarioTemp%tree%graph("grafica_ABB")
      ! allocate(ids_a_buscar(2))  ! Tamaño del arreglo
      ! ids_a_buscar = [1, 3]      ! Valores del arreglo
  
      ! Llamamos al método buscarIdGraph con el arreglo de IDs
      ! call usuarioTemp%tree%buscarIdGraph(ids_a_buscar)
      ! call usuarioTemp%tree%buscarIdGraph(3)
      ! call usuarioTemp%tree%buscarIdGraph(3)
      ! call usuarioTemp%tree%insert(2,m)
      ! print *, "Matriz agregada!"



      print*, "--------Recorrido limitado--------"
      cadena_preorder = usuarioTemp%tree%preorder(4)
      print*, "Orden preorder"
      print*, cadena_preorder
      ! call usuarioTemp%tree%buscarIdGraph(cadena_preorder)

      cadena_posorder = usuarioTemp%tree%posorder(4)
      print*, "Orden posorder"
      print*, cadena_posorder
      ! call usuarioTemp%tree%buscarIdGraph(cadena_posorder)

      cadena_inorder = usuarioTemp%tree%inorder(4)
      print*, "Orden inorder"
      print*, cadena_inorder
      ! call usuarioTemp%tree%buscarIdGraph(cadena_id)

      ! print*, "--------Recorrido por capas--------"
      ! print *, "Ingrese numero de capas a graficar: "
      ! read *, numImg

      ! allocate(ordenCapas(numImg))
      ! do canti =1 , numImg
      !   print *, "Ingrese id de la capa", canti, ": "
      !   read *, ordenCapas(canti)
      ! end do

      ! print *, "Los valores de las capas son: ", ordenCapas
      ! call usuarioTemp%tree%buscarIdGraph(ordenCapas)
      ! deallocate(ordenCapas)


  end subroutine pruebaMatriz

  ! subroutine pruebaAbb()
  !   ! call tree%insert(3)
  !   ! call tree%insert(43)
  !   ! call tree%insert(2)
  !   ! call tree%insert(9)
  !   ! call tree%insert(5)
  !   ! call tree%insert(3)
  !   ! call tree%insert(77)
  !   ! call tree%insert(4)

  !   ! if (associated(usuarioTemp%tree%root)) then
  !     ! Obtener un nodo del árbol (por ejemplo, el root)
  !     ! type(Node_t), pointer :: nodo
  !     ! nodo => usuarioTemp%tree%root
  
  !     ! Asignar la matriz al nodo
  !     nodo%matriz_temp = m
  !     ! Aqui aplica varias capas pero en este caso es una misma matriz solo para probar
  !     call usuarioTemp%tree%insert(3,m)  !esto es como que si estuviera insertando el indice de la capa, falta la matriz
  !     call usuarioTemp%tree%insert(2,m)
  !     ! call usuarioTemp%tree%insert(6,miMatriz)
  !     ! call usuarioTemp%tree%insert(1,miMatriz)
  !     print *, "Matriz agregada!"
  !     ! call usuarioTemp%tree%graph("inserted")

  !   ! write(*, '(A)') "Escribiendo en preorden: "
  !   ! call usuarioTemp%tree%preorder()

  !   ! write(*, '(A)') "Escribiendo en inorder: "
  !   ! call usuarioTemp%tree%inorder()

  !   ! print *, "Escribiendo en posorden: "
  !   ! call usuarioTemp%tree%posorder()
      
  !   ! else
  !     ! print *, "El árbol del usuario no está inicializado correctamente."
  !   ! end if



  ! end subroutine pruebaAbb


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

! subroutine readCapas()
  
!   filename = 'ImagenMario.json'

!   print *, "---------------------------------------"
!   print *, "----- Capas -----"

!   call json%initialize()
!   call json%load(filename=filename)
!   call json%info('',n_children=size)
!   call json%get_core(jsonc)
!   call json%get('', listPointer, found)

!   ! Variables para almacenar datos
  
!   call usuarioTemp%tree%insert(0)
!   do i = 1, size
!     call jsonc%get_child(listPointer, i, animalPointer, found)

!     ! Obtener el ID de la capa
!     call jsonc%get_child(animalPointer, 'id_capa', attributePointer, found)
!     call jsonc%get(attributePointer, id_capa)

!     ! Obtener los píxeles de la capa
!     call jsonc%get_child(animalPointer, 'pixeles', attributePointer, found)

!     ! Obtener el número de píxeles en la capa
!     call jsonc%info(attributePointer, n_children=n_pixeles)

!     do j = 1, n_pixeles
!         ! Obtener cada píxel individualmente
!         call jsonc%get_child(attributePointer, j, pixelPointer, found)

!         ! Obtener los atributos del píxel
!         call jsonc%get_child(pixelPointer, 'fila', filaPointer, found)
!         call jsonc%get(filaPointer, fila)

!         call jsonc%get_child(pixelPointer, 'columna', columnaPointer, found)
!         call jsonc%get(columnaPointer, columna)

!         call jsonc%get_child(pixelPointer, 'color', colorPointer, found)
!         call jsonc%get(colorPointer, color)

!         print *, "Capa:", id_capa, "Pixel (Fila, Columna):", fila, columna, "Color:" ,color
!         call usuarioTemp%tree%buscarId(0,fila,columna,color)
!         call usuarioTemp%tree%buscarIdGraph(0)
!         ! call m%insert(fila,columna,color)
!         ! call usuarioTemp%tree%insert(id_capa)
!         ! call usuarioTemp%tree%graph("arbolABB")
!     end do
! end do

!   call json%destroy()
! end subroutine readCapas


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
      do contador_pixel = 1, size_pixel
          call jsonc%get_child(atributoPunteroCapa, contador_pixel, punteroPixel, capa_encontrada)
          call jsonc%get_child(punteroPixel, 'fila', atributoPixel, capa_encontrada)
          call jsonc%get(atributoPixel, fila)
          call jsonc%get_child(punteroPixel, 'columna', atributoPixel, capa_encontrada)
          call jsonc%get(atributoPixel, columna)
          call jsonc%get_child(punteroPixel, 'color', atributoPixel, capa_encontrada)
          call jsonc%get(atributoPixel, color)
          read(id_capa, *) id_capa_int
          read(fila, *) fila_int
          read(columna, *) columna_int
          ! call usuarioTemp%tree%insert(id_capa_int)
          call m%insert( columna_int,fila_int, color)
      end do
  end do
  call json%destroy()
  print*,"Grafica Antes"
  ! call usuarioTemp%tree%graph("grafica_ABBJson")
  call m%graficar()
  print*,"Grafica DESPUES"
end subroutine readCapas



subroutine readImg()


  call json%initialize()
  call json%load(filename='imagenes.json')
  call json%info('',n_children=size_capa)
  call json%get_core(jsonc)
  call json%get('', listaPunteroCapa, capa_encontrada)

  do contador_capa = 1, size_capa
      call jsonc%get_child(listaPunteroCapa, contador_capa, punteroCapa, capa_encontrada)
      call jsonc%get_child(punteroCapa, 'id', atributoPunteroCapa, capa_encontrada)
      call jsonc%get(atributoPunteroCapa, id)
      call jsonc%get_child(punteroCapa, 'capas', atributoPunteroCapa, capa_encontrada)
      call jsonc%info(atributoPunteroCapa,n_children=size_pixel)
      allocate(capas(size_pixel))

      do contador_pixel = 1, size_pixel
          call jsonc%get_child(atributoPunteroCapa, contador_pixel, punteroPixel, capa_encontrada)
          call jsonc%get(punteroPixel, capas(contador_pixel))
      end do

      ! print*, "ID: ", id
      call usuarioTemp%avlTree%insert(id)
      call usuarioTemp%avlTree%insertInABB(id,capas)
      
      ! print*, "Capas: ", capas

      deallocate(capas)
  end do

  call json%destroy()
end subroutine readImg


subroutine readAlbum()


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
      allocate(album(size_pixel))

      do contador_pixel = 1, size_pixel
          call jsonc%get_child(atributoPunteroCapa, contador_pixel, punteroPixel, capa_encontrada)
          call jsonc%get(punteroPixel, album(contador_pixel))
      end do

      ! print*, "nombre_album: ", nombre_alb
      ! call usuarioTemp%listaAlbums%push(nombre_alb)
      
      ! print*, "imgs: ", album

      deallocate(album)
  end do
  ! call usuarioTemp%listaAlbums%print()
  ! call usuarioTemp%listaAlbums%print_dot("almbum prueba")

  call json%destroy()
end subroutine readAlbum


end program main


