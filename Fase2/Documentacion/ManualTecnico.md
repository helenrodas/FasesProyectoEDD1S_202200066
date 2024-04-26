# Manual Tecnico

## Helen Rodas - 202200066

### El proyecto de Pixel Print Studio consta una aplicacion en consola desarrollada en el lenguaje Fortran para permitir a los clientes de la empresa Pixel Print Studio registrar imágenes especiales construidas por capas. Para poder hacer uso de la aplicación el cliente debe registrarse.

## main
En este archivo se encuentra en la carpeta app pues es donde inicia el proyecto, imprimo menus de opciones para que puedan mostrarse en consola.
Menu inicio de Sesion y para usuario general
```fortran
do 
    call menu_inicial()
    read(*, *) option
    select case(option)
    case(1)
      call inicio_sesion()
    case(2)
      call nuevo_usuario()
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

  !Opcion inicio de sesion
  subroutine inicio_sesion()
    character(len=100) :: usuario, password,dpi
    
    integer*8 :: dpiAsInt
    print *, "--------------------"
    print *, "Ingrese su usuario: "
    read(*,'(A)') usuario
    
    print *, "Ingrese su password: "
    read*, password

    password = trim(password)
    
    if (usuario == "admin" .and. password == "EDD2024") then
        call op_menuAdmin()
    else
        print *, "Ingrese su dpi: "
        read*, dpi
        read(dpi, *) dpiAsInt

        call listaU%buscarUsuario(usuario,password,dpiAsInt,usuarioExiste,usuarioTemp)
        if(usuarioExiste) then
          call op_menuUsuario()
        else 
          print*, "Usuario no encontrado..."
        end if
        
    end if
  end subroutine inicio_sesion

  subroutine nuevo_usuario()
    character(len=100) :: usuario, password,dpi
    
    integer*8 :: dpiAsInt
    print *, "--------------------"
    print *, "Ingrese el nuevo usuario: "
    read(*,'(A)') usuario
    
    print *, "Ingrese su password: "
    read*, password

    print *, "Ingrese su dpi: "
    read*, dpi

    password = trim(password)
    
    read(dpi, *) dpiAsInt

    call listaU%existeUsuario(usuario,password,usuarioExiste)
    if(usuarioExiste) then
      print*, "El usuario ya existe!"
    else 
      ! call insert(root,dpiAsInt,usuario,password)  este metodo es para el arbol b
      call listaU%push(dpiAsInt,usuario,password)
      ! print*,"print desde arbol b"
      ! call inorder(root)
      ! print*,"print desde lista"
      ! call listaU%print()
      print*, "Usuario agregado!"
    end if
    ! call op_menuUsuario()
  end subroutine nuevo_usuario

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
    print *, "6. Cerrar Sesion" 
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_usuario

```

Menu para usuario Administrador
```fortran
  subroutine menu_admin()
    print *, "...................................."
    print *, "          Menu Administrador        "
    print *, "...................................."
    print *, "1. Grafica Arbol B Usuarios"
    print *, "2. Operaciones sobre usuarios"
    print *, "3. Carga masiva de usuarios"
    print *, "4. Reportes"
    print *, "5. Cerrar Sesion"
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
        call listaU%clientesGraph()
      case(2)
        call op_operacionesUsuarios()
      case(3)
        call readUsuarios()
      case(4)
        call op_reportesAdmin()
    case(5)
            exit
          case default
            print *, "Error!. Seleccione una opcion valida."
          end select
        end do
  end subroutine op_menuAdmin
```
Lectura de archivos .json
```fortran
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
        ! call insert(root,dpiAsInt,nombreCliente,password)
        call listaU%push(dpiAsInt,nombreCliente,password)

    end do
    ! call inorder(root)
    ! call listaU%print()
    call json%destroy()
    print*,"Archivo usuarios leido exitosamente"
end subroutine readUsuarios

subroutine readCapas()
  call json%initialize()
  call json%load(filename='ImagenMario.json')
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
          ! print*," "
          ! print*, "-----------------------------------------------------"
          ! print *, "Capa: ", id_capa, " Pixel (Fila): ", fila, " Pixel (Columna): ", columna, " Color:" ,color
      end do
  end do
  call json%destroy()
  call listaU%modificarCantidad(usuarioTemp%dpi,size_capa,0)
  print*,"----------------------------------- "
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
      ! print*, "ID: ", id, "Capas: ", capas
      
      call usuarioTemp%avlTree%agregarEnABB(id,capas)
      deallocate(capas)
  end do

  print*,"----------------------------------- "
  print*,"Archivo Imagenes leido exitosamente"
  call listaU%modificarCantidad(usuarioTemp%dpi,0,size_capa)
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
      deallocate(ImagenesList)
  end do
  ! call usuarioTemp%listaAlbums%print()
  call json%destroy()
  print*,"---------------------------------"
  print*,"Archivo albums leido exitosamente"
end subroutine readAlbum
```
## BTree_module
Modulo para arbol B
```fortran
type :: B_usuario
        integer*8 :: dpi
        character(:), allocatable :: name, password
        type(B_usuario), pointer :: left => null(), right => null()
    end type B_usuario

    contains

    recursive subroutine insert(root, dpi, name, password)
        type(B_usuario), pointer, intent(inout) :: root
        integer*8, intent(in) :: dpi
        character(len=*), intent(in) :: name, password

        if (.not. associated(root)) then
            allocate(root)
            root%dpi = dpi
            allocate(character(len=len(name)) :: root%name)
            allocate(character(len=len(password)) :: root%password)
            root%name = name
            root%password = password
        else
            if (dpi < root%dpi) then
                call insert(root%left, dpi, name, password)
            else if (dpi > root%dpi) then
                call insert(root%right, dpi, name, password)
            end if
        end if
    end subroutine insert
```
## AbbTree_module
Modulo para el arbol B
```fortran
 type, public :: Node_t
        integer :: value
        type(matrix),allocatable:: matriz_temp
        type(Node_t), pointer :: right => null()
        type(Node_t), pointer :: left => null()
    end type Node_t

    type, public :: abb
        type(Node_t), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: inorder
        procedure :: posorder
        procedure :: graph
        procedure :: buscarId
        procedure :: buscarIdGraph
        procedure :: GraphCapa
        procedure :: recorrido_amplitud
        procedure :: existeIDABB
        procedure :: print_profundidadCapas
        procedure :: print_capaHoja
        procedure :: contarNodosEnArbol
        procedure :: numero_nodos
        procedure :: getContenido
    end type abb

subroutine insert(self, val)
    class(abb), intent(inout) :: self
    integer, intent(in) :: val

    if (.not. associated(self%root)) then
        allocate(self%root)
        self%root%value = val
        allocate(self%root%matriz_temp)  ! Inicializa la matriz aquí
    else
        call insertRec(self%root, val)
    end if
end subroutine insert

recursive subroutine insertRec(root, val)
    type(Node_t), pointer, intent(inout) :: root
    integer, intent(in) :: val
    
    if (val < root%value) then
        if (.not. associated(root%left)) then
            allocate(root%left)
            root%left%value = val
            allocate(root%left%matriz_temp)  ! Inicializa la matriz aquí
        else
            call insertRec(root%left, val)
        end if
    else if (val > root%value) then
        if (.not. associated(root%right)) then
            allocate(root%right)
            root%right%value = val
            allocate(root%right%matriz_temp)  ! Inicializa la matriz aquí
        else
            call insertRec(root%right, val)
        end if
    end if
end subroutine insertRec
```
Funcion para eliminar un nodo del arbol B
```fortran
 subroutine delete(self, val)
        class(abb), intent(inout) :: self
        integer, intent(inout) :: val
    
        self%root => deleteRec(self%root, val)
    end subroutine delete

    recursive function deleteRec(root, value) result(res)
        type(Node_t), pointer :: root
        integer, intent(in) :: value
        type(Node_t), pointer :: res
        type(Node_t), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (value < root%value) then
            root%left => deleteRec(root%left, value)
        else if (value > root%value) then
            root%right => deleteRec(root%right, value)
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%left => deleteRec(root%left, temp%value)
            end if
        end if

        res => root
    end function deleteRec


    recursive subroutine getMajorOfMinors(root, major)
        type(Node_t), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors
```
Funciones para crear una matriz de una o varias capas
```fortran
subroutine buscarIdGraph(self, ids)
    class(abb), intent(inout) :: self
    integer, dimension(:), intent(in) :: ids
    integer :: num_ids, i
    type(Node_t), pointer :: node
    type(matrix), pointer :: combined_matrix

    num_ids = size(ids)

    print*, "num_ids: ", num_ids
    ! Inicializar matriz combinada
    allocate(combined_matrix)
    call combined_matrix%init()

    ! Recorrer cada ID y encontrar el nodo correspondiente
    do i = 1, num_ids
        node => existeNodoGraph(self%root, ids(i))
        if (associated(node)) then
            ! print *, "Nodo encontrado con ID:", ids(i)
            ! Agregar la matriz del nodo a la matriz combinada
            ! print*, "matriz del nodo"
            ! call node%matriz_temp%print()
            call combined_matrix%add_matrix(node%matriz_temp)
        else
            ! print *, "Nodo no encontrado con ID:", ids(i)
        end if
    end do

    ! Graficar la matriz combinada
    call combined_matrix%graficar()

    deallocate(combined_matrix)
contains

    recursive function existeNodoGraph(root, value) result(node)
        type(Node_t), pointer :: root
        integer, intent(in) :: value
        type(Node_t), pointer :: node

        if (associated(root)) then
            if (root%value == value) then
                node => root
            else if (value < root%value) then
                node => existeNodoGraph(root%left, value)
            else
                node => existeNodoGraph(root%right, value)
            end if
        else
            node => null()
        end if
    end function existeNodoGraph
end subroutine buscarIdGraph


!Esta es para cuando solo quiero graficar una capa


subroutine GraphCapa(self, val)
    class(abb), intent(inout) :: self
    integer, intent(in) :: val
    type(Node_t), pointer :: node

    node => buscarNodoCapa(self%root, val)

    if (associated(node)) then
    !   print *, "Nodo encontrado"
      call node%matriz_temp%graficar()
    else
    !   print *, "Nodo no encontrado"
    end if

    contains

    recursive function buscarNodoCapa(root, value) result(node)
        type(Node_t), pointer :: root
        integer, intent(in) :: value
        type(Node_t), pointer :: node

        if (associated(root)) then
            if (root%value == value) then
                node => root
            else if (value < root%value) then
                node => buscarNodoCapa(root%left, value)
            else
                node => buscarNodoCapa(root%right, value)
            end if
        else
            node => null()
        end if
    end function buscarNodoCapa
end subroutine GraphCapa

```
Funcion para una vez se haya insertado el id en un nodo buscarlo e insertar la matriz de ese id.

```fortran
subroutine buscarId(self, val,fila,columa,color)
    class(abb), intent(inout) :: self
    integer, intent(in) :: val,fila,columa
    type(Node_t), pointer :: node
    character(len=7) ::color

    node => existeNodo(self%root, val)
    
    if (associated(node)) then
    !   print *, "Nodo encontrado"
      call insertarEnMatriz(node,fila,columa,color)
    else
    !   print *, "Nodo no encontrado"
    end if

    contains

    recursive function existeNodo(root, value) result(node)
        type(Node_t), pointer :: root
        integer, intent(in) :: value
        type(Node_t), pointer :: node

        if (associated(root)) then
            if (root%value == value) then
                node => root
            else if (value < root%value) then
                node => existeNodo(root%left, value)
            else
                node => existeNodo(root%right, value)
            end if
        else
            node => null()
        end if
    end function existeNodo

    subroutine insertarEnMatriz(node, fila, columna, color)
        type(Node_t), pointer :: node
        integer, intent(in) :: fila, columna
        character(len=7), intent(in) :: color
    
        call node%matriz_temp%insert(fila, columna, color)
        ! call node%matriz_temp%print
    end subroutine insertarEnMatriz
    
end subroutine buscarId
```
Funcion para unir el arbol AVL con arbol ABB
```fortran
subroutine existeIDABB(self, val, encontrado)
    class(abb), intent(inout) :: self
    integer, intent(in) :: val
    logical, intent(out) :: encontrado
    type(Node_t), pointer :: node

    node => existeNodoID(self%root, val)
    
    if (associated(node)) then
      encontrado = .true.
    else
      encontrado = .false.
    end if

    contains

    recursive function existeNodoID(root, value) result(node)
        type(Node_t), pointer :: root
        integer, intent(in) :: value
        type(Node_t), pointer :: node

        if (associated(root)) then
            if (root%value == value) then
                node => root
            else if (value < root%value) then
                node => existeNodoID(root%left, value)
            else
                node => existeNodoID(root%right, value)
            end if
        else
            node => null()
        end if
    end function existeNodoID
end subroutine existeIDABB
```
Funcion para graficar el arbol ABB
```fortran
subroutine graph(self,filename)
        class(abb), intent(in) :: self
        character(len=*),intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        
        createNodes = ''
        linkNodes = ''
        ! filename = "graficaABB"
        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=Mcircle];" // new_line('a')

        if (associated(self%root)) then
            call RoamTree(self%root, createNodes, linkNodes)
        end if
        
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(filename, dotStructure)
        print *, "Archivo actualizado existosamente."
    end subroutine graph
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(Node_t), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_value

        if (associated(current)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
          address = get_address_memory(current)
          write(str_value, '(I0)') current%Value
          createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
          ! VIAJAMOS A LA SUBRAMA IZQ
          if (associated(current%Left)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Left)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "L"];' // new_line('a')
    
          end if
          ! VIAJAMOS A LA SUBRAMA DER
          if (associated(current%Right)) then
            address = get_address_memory(current)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Right)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "R"];' // new_line('a')
          end if
    
          call RoamTree(current%Left, createNodes, linkNodes)
          call RoamTree(current%Right, createNodes, linkNodes)
        end if
    end subroutine RoamTree


    subroutine write_dot(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        
        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        
        ! Abre el archivo DOT en la ruta proporcionada
        open(10, file=filename, status='replace', action='write')
    
        ! Escribe el código DOT en el archivo
        write(10, '(A)') trim(code)
        
        ! Cierra el archivo
        close(10)
    
        ! Genera la imagen PNG
        call system("dot -Tpng "// trim(filename) // " -o " // png_filename)
        call execute_command_line('start '// trim("./graficaABB.png"))
    end subroutine write_dot

    function get_address_memory(node) result(address)
        !class(matrix_t), intent(in) :: self
        type(Node_t), pointer :: node
        character(len=20) :: address
        ! integer 8
        integer*8 :: i
    
        i = loc(node) ! get the address of x
        ! convert the address to string
        write(address, 10) i 
        10 format(I0)
    
    end function get_address_memory
```
Funciones para recorridos del arbol ABB
```fortran
    function preorder(self, num_nodes)
        class(abb), intent(in) :: self
        integer, intent(in) :: num_nodes
        integer, dimension(:), allocatable :: preorder
        integer :: count
        count = 0
        allocate(preorder(num_nodes))
        call preorderRec(self%root, num_nodes, count, preorder)
        preorder = preorder  ! Asigna el resultado al nombre de la función
    end function preorder
    
    recursive subroutine preorderRec(root, num_nodes, count, node_values)
        type(Node_t), pointer, intent(in) :: root
        integer, intent(in) :: num_nodes
        integer, intent(inout) :: count
        integer, dimension(:), allocatable, intent(inout) :: node_values
    
        if(associated(root) .and. count < num_nodes) then
            ! RAIZ - IZQ - DER
            if (count < num_nodes) then
                node_values(count+1) = root%value
                count = count + 1
            end if
            call preorderRec(root%left, num_nodes, count, node_values)
            call preorderRec(root%right, num_nodes, count, node_values)
        end if
    end subroutine preorderRec
    
    
        function inorder(self, num_nodes)
            class(abb), intent(in) :: self
            integer, intent(in) :: num_nodes
            integer, dimension(:), allocatable :: inorder
            integer :: count
            count = 0
            allocate(inorder(num_nodes))
            call inordenRec(self%root, num_nodes, count, inorder)
            print *, ""
            inorder = inorder  ! Asigna el resultado al nombre de la función
        end function inorder
        
        recursive subroutine inordenRec(root, num_nodes, count, node_values)
            type(Node_t), pointer, intent(in) :: root
            integer, intent(in) :: num_nodes
            integer, intent(inout) :: count
            integer, dimension(:), allocatable, intent(inout) :: node_values
        
            if(associated(root) .and. count < num_nodes) then
                ! IZQ - RAIZ - DER
                call inordenRec(root%left, num_nodes, count, node_values)
                if (count < num_nodes) then
                    node_values(count+1) = root%value
                    count = count + 1
                end if
                call inordenRec(root%right, num_nodes, count, node_values)
            end if
        end subroutine inordenRec
        
    
        function posorder(self, num_nodes)
            class(abb), intent(in) :: self
            integer, intent(in) :: num_nodes
            integer, dimension(:), allocatable :: posorder
            integer :: count
            count = 0
            allocate(posorder(num_nodes))
            call posordenRec(self%root, num_nodes, count, posorder)
            print *, ""
            posorder = posorder  ! Asigna el resultado al nombre de la función
        end function posorder
        
        recursive subroutine posordenRec(root, num_nodes, count, node_values)
            type(Node_t), pointer, intent(in) :: root
            integer, intent(in) :: num_nodes
            integer, intent(inout) :: count
            integer, dimension(:), allocatable, intent(inout) :: node_values
        
            if(associated(root) .and. count < num_nodes) then
                ! IZQ - DER - RAIZ
                call posordenRec(root%left, num_nodes, count, node_values)
                call posordenRec(root%right, num_nodes, count, node_values)
                if (count < num_nodes) then
                    node_values(count+1) = root%value
                    count = count + 1
                end if
            end if
        end subroutine posordenRec
```
Funciones para generar reportes de usuario sobre arbol
```fortran 
    recursive subroutine agregarNivel(root, nivel, cadena)
        type(Node_t), pointer, intent(in) :: root
        integer, intent(in) :: nivel
        character(len=:), allocatable, intent(inout) :: cadena
        character(len=20) :: valor_str
    
        if (.not. associated(root)) then
            return
        else if (nivel == 1) then
            write(valor_str, '(I0)') root%value
            cadena = trim(cadena) // trim(valor_str) // " - "
        else if (nivel > 1) then
            call agregarNivel(root%left, nivel-1, cadena)
            call agregarNivel(root%right, nivel-1, cadena)
        end if
    end subroutine agregarNivel
    
    recursive function altura(root) result(h)
    type(Node_t), pointer, intent(in) :: root
    integer :: h, h1, h2

    if (.not. associated(root)) then
        h = 0
    else
        h1 = altura(root%left)
        h2 = altura(root%right)
        if (h1 > h2) then
            h = h1 + 1
        else
            h = h2 + 1
        end if
    end if
end function altura

subroutine print_profundidadCapas(self)
    class(abb), intent(inout) :: self
    
    integer :: profundidad

    profundidad = depth(self%root)
    print *, "-----------------------------------------"
    print *, 'La profundidad de arbol de capas es: ', profundidad
end subroutine print_profundidadCapas



recursive function depth(root) result(d)
type(Node_t), pointer, intent(in) :: root
integer :: d
integer :: left_depth, right_depth

if (.not. associated(root)) then
    d = 0
else
    left_depth = depth(root%left)
    right_depth = depth(root%right)
    d = max(left_depth, right_depth) + 1
end if
end function depth


subroutine capas_hojas(self)
    class(abb), intent(inout) :: self
    call capas_hojasRec(self%root)

end subroutine capas_hojas

recursive subroutine capas_hojasRec(root)
    type(Node_t), pointer, intent(in) :: root
    if(associated(root)) then
        if(.not. associated(root%left) .and. .not. associated(root%right)) then
            print *, "La hoja es: ", root%value
        end if
        call capas_hojasRec(root%left)
        call capas_hojasRec(root%right)
    end if
end subroutine capas_hojasRec

subroutine print_capaHoja(self)
    class(abb), intent(in) :: self
    print *, "--------------------"
    print *, "Capas que son hojas:"
    call imprimir_hoja_recursivo(self%root)
end subroutine print_capaHoja

recursive subroutine imprimir_hoja_recursivo(root)
    type(Node_t), pointer, intent(in) :: root
    if (associated(root)) then
        if (.not. associated(root%left) .and. .not. associated(root%right)) then
            print *, root%value
        else
            call imprimir_hoja_recursivo(root%left)
            call imprimir_hoja_recursivo(root%right)
        end if
    end if
end subroutine imprimir_hoja_recursivo

recursive function contarNodos(root) result(n)
    type(Node_t), pointer :: root
    integer :: n

    if (.not. associated(root)) then
        n = 0
    else
        n = 1 + contarNodos(root%left) + contarNodos(root%right)
    end if
end function contarNodos

subroutine contarNodosEnArbol(self, totalNodos)
    class(abb), intent(inout) :: self
    integer, intent(out) :: totalNodos

    totalNodos = contarNodos(self%root)
end subroutine contarNodosEnArbol



function numero_nodos(self) result(num_nodos)
    class(abb), intent(in) :: self
    integer :: num_nodos
    num_nodos = contar_nodos(self%root)
end function numero_nodos

recursive function contar_nodos(root) result(num_nodos)
    type(Node_t), pointer, intent(in) :: root
    integer :: num_nodos
    if (.not. associated(root)) then
        num_nodos = 0
    else
        num_nodos = 1 + contar_nodos(root%left) + contar_nodos(root%right)
    end if
end function contar_nodos

subroutine getContenido(self, contenido, nodoCapa)
    class(abb), intent(in) :: self
    character(len=:), allocatable :: contenido
    character(len=20) :: nodoCapa
    character(len=36) :: nombre

        !Graficar
    if(associated(self%root)) then
        nombre = generate_uuid()
        contenido = contenido//'"'//trim(nodocapa)//'"'//'->"Nodo'//nombre//'"'
        !contenido = contenido//'subgraph cluster{label="Capas"'
        call imprimirRec2(self%root, nombre, contenido)
        !contenido = contenido//"}"
    end if
end subroutine getContenido


recursive subroutine imprimirRec2(raiz, nombre, contenido)
    type(Node_t), pointer, intent(in) :: raiz
    character(len=36), intent(in) :: nombre
    character(len=:), allocatable :: contenido

    character(len=36) :: derecha
    character(len=36) :: izquierda
    character(len=36) :: idcapa

    derecha = generate_uuid()
    izquierda = generate_uuid()

    if(associated(raiz)) then
        write(idcapa, '(I0)') raiz%value
        contenido = contenido//'"Nodo'//nombre//'"[label= "'//trim(idcapa)//'", shape ="box3d"]'

        if(associated(raiz%left)) then
            contenido = contenido//'"Nodo'//nombre//'"->"Nodo'//izquierda//'"'
        end if

        if(associated(raiz%right)) then
            contenido = contenido//'"Nodo'//nombre//'"->"Nodo'//derecha//'"'
        end if
        call imprimirRec2(raiz%left, izquierda, contenido)
        call imprimirRec2(raiz%right, derecha, contenido)
    end if
end subroutine imprimirRec2
```
## AVLTree_module
Modulo del arbol AVL
```fortran
 type, public ::  Node_AVL
        integer :: Value
        integer :: altura = 1
        type(Node_AVL), pointer :: Left => null()
        type(Node_AVL), pointer :: Right => null()
        type(abb),allocatable :: arbol
        integer, dimension(:), allocatable :: capaimg
        
    end type Node_AVL

    type,public :: avl
        type(Node_AVL), pointer :: root => null()
        contains
        ! procedure :: newTree
        procedure :: insert
        procedure :: avlGraph
        procedure :: agregarEnABB
        procedure :: returnABB
        procedure :: returnABBInt
        procedure :: existeId
        procedure :: top5_imagenes
        procedure :: delete
        procedure :: ABBAVLGraph
    end type avl

    contains

    subroutine insert(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        call insertRec(self%root, val)
    end subroutine insert

    recursive subroutine insertRec(root, val)
        type(Node_AVL), pointer, intent(inout) :: root
        integer, intent(in) :: val

        if(.not. associated(root)) then
            allocate(root)
            root = Node_AVL(Value=val)
            allocate(root%arbol)
            allocate(root%capaimg(0))
        else if(val < root%Value) then 
            call insertRec(root%left, val)

        else if(val > root%Value) then
            call insertRec(root%right, val)
        end if

        root%altura = maximo(obtenerAltura(root%left), obtenerAltura(root%right)) + 1

        if(obtenerBalance(root) > 1) then
            if(obtenerBalance(root%right) < 0) then
                root%right => rotacionDerecha(root%right)
                root => rotacionIzquierda(root)
            else
                root => rotacionIzquierda(root)
            end if
        end if

        if(obtenerBalance(root) < -1) then
            if(obtenerBalance(root%left) > 0) then
                root%left => rotacionIzquierda(root%left)
                root => rotacionDerecha(root)

            else
                root => rotacionDerecha(root)
            end if
        end if
    end subroutine insertRec
```
Funcion para eliminar un nodo en el arbol AVL
```fortran
subroutine delete(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        self%root => deleteRec(self%root, val)
    end subroutine delete

recursive function deleteRec(root, val) result(res)
        type(Node_AVL), pointer :: root
        integer, intent(in) :: val

        type(Node_AVL), pointer :: temp
        type(Node_AVL), pointer :: res 
        
        if(.not. associated(root)) then
            res => root
            return
        end if

        if(val < root%Value) then
            root%left => deleteRec(root%left, val)
        
        else if(val > root%Value) then
            root%right => deleteRec(root%right, val)

        else
            if(.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp

            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
            
            else
                call obtenerMayorDeMenores(root%left, temp)
                root%Value = temp%Value
                root%left => deleteRec(root%left, temp%Value)
            end if
        end if

        res => root
        if(.not. associated(root)) return

        root%altura = maximo(obtenerAltura(root%left), obtenerAltura(root%right))

        if(obtenerBalance(root) > 1) then
            if(obtenerBalance(root%right) < 0) then
                root%right => rotacionDerecha(root%right)
                root => rotacionIzquierda(root)
            else
                root => rotacionIzquierda(root)
            end if
        end if

        if(obtenerBalance(root) < -1) then
            if(obtenerBalance(root%left) > 0) then
                root%left => rotacionIzquierda(root%left)
                root => rotacionDerecha(root)

            else
                root => rotacionDerecha(root)
            end if
        end if

        res => root
    end function deleteRec
```
Funcion para graficar el arbol AVL
```fortran
subroutine avlGraph(this)
    class(avl), intent(inout) :: this
    character(len=:), allocatable :: dotStructure
    character(len=:), allocatable :: createNodes
    character(len=:), allocatable :: linkNodes
    createNodes = ''
    linkNodes = ''


    dotStructure = "digraph G{" // new_line('a')
    ! dotStructure = dotStructure // "Node_AVL [shape=Mcircle];" // new_line('a')

    if (associated(this%root)) then
        call RoamTree(this%root, createNodes, linkNodes)
    end if

    dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
    call write_dot(dotStructure)
    print *, "Archivo actualizado existosamente."
end subroutine avlGraph

recursive subroutine RoamTree(actual, createNodes, linkNodes)
    type(Node_AVL), pointer :: actual
    character(len=:), allocatable, intent(inout) :: createNodes
    character(len=:), allocatable, intent(inout) :: linkNodes
    character(len=20) :: address
    character(len=20) :: str_value

    if (associated(actual)) then
        ! SE OBTIENE INFORMACION DEL NODO ACTUAL
        address = get_address_memory(actual)
        write(str_value, '(I0)') actual%Value
        createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
        ! VIAJAMOS A LA SUBRAMA IZQ
        if (associated(actual%Left)) then
        linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
        address = get_address_memory(actual%Left)
        linkNodes = linkNodes // '"' // trim(address) // '" ' &
                    // '[label = "L"];' // new_line('a')

        end if
        ! VIAJAMOS A LA SUBRAMA DER
        if (associated(actual%Right)) then
        address = get_address_memory(actual)
        linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
        address = get_address_memory(actual%Right)
        linkNodes = linkNodes // '"' // trim(address) // '" ' &
                    // '[label = "R"];' // new_line('a')
        end if

        call RoamTree(actual%Left, createNodes, linkNodes)
        call RoamTree(actual%Right, createNodes, linkNodes)
    end if
end subroutine RoamTree

    function get_address_memory(node) result(address)
    !class(matrix_t), intent(in) :: self
    type(Node_AVL), pointer :: node
    character(len=20) :: address
    ! integer 8
    integer*8 :: i

    i = loc(node) ! get the address of x
    ! convert the address to string
    write(address, 10) i 
    10 format(I0)

    end function get_address_memory

    subroutine write_dot(code)
    character(len=*), intent(in) :: code
    open(10, file='grafica_AVL.dot', status='replace', action='write')
    write(10, '(A)') trim(code)
    close(10)
    ! Genera la imagen PNG
    call system("dot -Tpng grafica_AVL.dot -o grafica_AVL.png")
    call execute_command_line('start '// trim("./grafica_AVL.png"))
    end subroutine write_dot
```
Funciones para retornar el arbol ABB del nodo del AVL
```fortran
    function returnABB(self, id) result(result)
        class(avl), intent(inout) :: self
        integer, intent(in) :: id
        type(Node_AVL), pointer :: node
        type(abb), pointer :: result  ! Cambia el tipo de 'result' a 'abb'
    
        node => search1(self%root, id)
        if (associated(node)) then
            result => node%arbol
        else
            nullify(result)
        end if
    end function returnABB
    


    function returnABBInt(self, id) result(result)
        class(avl), intent(inout) :: self
        integer, intent(in) :: id
        type(Node_AVL), pointer :: node
        integer, dimension(:), pointer :: result  ! Especifica el tipo de 'result' como un arreglo de enteros
    
        node => search1(self%root, id)
        if (associated(node)) then
            result => node%capaimg
        else
            nullify(result)
        end if
    end function returnABBInt
```
Funcion para verificar si existe un nodo por su id en el arbol
```fortran
function existeId(self, id) result(existe)
        class(avl), intent(inout) :: self
        integer, intent(in) :: id
        type(Node_AVL), pointer :: node
        logical :: existe  
    
        node => search1(self%root, id)
        if (associated(node)) then
            existe = .TRUE.
        else
            existe = .FALSE.
        end if
    end function existeId
```
Funcion reporte de to 5
```fortran
 subroutine top5_imagenes(self)
        class(avl), intent(inout) :: self
        integer :: max1, max2, max3, max4, max5
        integer :: id1, id2, id3, id4, id5
        max1 = 0
        max2 = 0
        max3 = 0
        max4 = 0
        max5 = 0
        id1 = 0
        id2 = 0
        id3 = 0
        id4 = 0
        id5 = 0
        call buscar_top_5(self%root, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        print*, "---------------------------------------------"
        print *, "Top 5 de imagenes con mayor numero de capas:"
        print *, "ID imagen:", id1, "Numero de capas:", max1
        print *, "ID imagen:", id2, "Numero de capas:", max2
        print *, "ID imagen:", id3, "Numero de capas:", max3
        print *, "ID imagen:", id4, "Numero de capas:", max4
        print *, "ID imagen:", id5, "Numero de capas:", max5
    end subroutine top5_imagenes
    
    recursive subroutine buscar_top_5(root, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        type(Node_AVL), pointer, intent(in) :: root
        integer, intent(inout) :: max1, max2, max3, max4, max5
        integer, intent(inout) :: id1, id2, id3, id4, id5
        integer :: num_nodos
        if (.not. associated(root)) return
        num_nodos = root%arbol%numero_nodos()
        if (num_nodos > max1) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = max2
            id3 = id2
            max2 = max1
            id2 = id1
            max1 = num_nodos
            id1 = root%Value
        else if (num_nodos > max2) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = max2
            id3 = id2
            max2 = num_nodos
            id2 = root%Value
        else if (num_nodos > max3) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = num_nodos
            id3 = root%Value
        else if (num_nodos > max4) then
            max5 = max4
            id5 = id4
            max4 = num_nodos
            id4 = root%Value
        else if (num_nodos > max5) then
            max5 = num_nodos
            id5 = root%Value
        end if
        call buscar_top_5(root%left, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        call buscar_top_5(root%right, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
    end subroutine buscar_top_5
```
Funcion para graficar el AVL con el ABB segun el nodo
```fortran
subroutine ABBAVLGraph(this, idimg)
        class(avl), intent(inout) :: this
        integer, intent(in) :: idimg
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        character(len=:), allocatable :: contenido
        character(len=30) :: name
        name = "AVL_CAPAS"
        createNodes = ''
        linkNodes = ''
        contenido = ''
    
    
        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=Mcircle];" // new_line('a')
    
        if (associated(this%root)) then
            call RoamTreeIMG(this%root, createNodes, linkNodes, idimg, contenido)
        end if
    
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // contenido // "}" // new_line('a')
        call write_dotAVL(dotStructure)
        print *, "Grafica generada exitosamente!."
    end subroutine ABBAVLGraph

    recursive subroutine RoamTreeIMG(actual, createNodes, linkNodes, idimg, contenido)
        type(Node_AVL), pointer :: actual
        integer, intent(in) :: idimg
        character(len=:), allocatable, intent(inout) :: createNodes
        character(len=:), allocatable, intent(inout) :: linkNodes
        character(len=:), allocatable:: contenido
        character(len=20) :: address
        character(len=20) :: str_value

        if (associated(actual)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
            address = get_address_memory(actual)
        
            if(idimg == actual%Value) then
                call actual%arbol%getContenido(contenido, address)
            end if
            write(str_value, '(I0)') actual%Value
            createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
            ! VIAJAMOS A LA SUBRAMA IZQ
            if (associated(actual%left)) then
                linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
                address = get_address_memory(actual%left)
                linkNodes = linkNodes // '"' // trim(address) // '" ' &
                        // '[label = "L"];' // new_line('a')

            end if
            ! VIAJAMOS A LA SUBRAMA DER
            if (associated(actual%right)) then
                address = get_address_memory(actual)
                linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
                address = get_address_memory(actual%right)
                linkNodes = linkNodes // '"' // trim(address) // '" ' &
                        // '[label = "R"];' // new_line('a')
            end if

            call RoamTreeIMG(actual%left, createNodes, linkNodes, idimg, contenido)
            call RoamTreeIMG(actual%right, createNodes, linkNodes, idimg, contenido)
        end if
    end subroutine RoamTreeIMG

    subroutine write_dotAVL(code)
        character(len=*), intent(in) :: code
        open(10, file='AVLSegundo.dot', status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)
        ! Genera la imagen PNG
        call system("dot -Tpng AVLSegundo.dot -o AVLSegundo.png")
        call execute_command_line('start '// trim("./AVLSegundo.png"))
        end subroutine write_dotAVL
```
## linkedlist_module
Modulo de la lista de usuarios
```fortran
 type,public :: listaUser
        type(nodeUser), pointer :: head => null() ! head of the list
    
        contains
            procedure :: push
            procedure :: print
            procedure :: delete_by_position
            procedure :: buscarUsuario
            procedure :: existeUsuario
            procedure :: actualizarUsuario
            procedure :: eliminarUsuario
            procedure :: modificarCantidad
            procedure :: clienteABuscar
            procedure :: clientesGraph
            ! procedure :: grafica_listaImg
        end type listaUser
    
        type,public :: nodeUser
        integer*8 :: dpi
        character(len=100) :: nombre,password
        type(abb) :: tree
        type(avl) :: avlTree
        type(listaAlbums) :: listaAlbums
        type(nodeUser), pointer :: next
        integer :: numImagenes = 0
        integer :: numCapas = 0
        end type nodeUser
    
        contains
    
        subroutine push(self, dpi,nombre,password)
        class(listaUser), intent(inout) :: self
        integer*8, intent(in) :: dpi
        character(len=*), intent(in)::nombre
        character(len=*), intent(in)::password
    
        type(nodeUser), pointer :: newNode
        allocate(newNode)
    
        newNode%dpi = dpi
        newNode%nombre = nombre
        newNode%password = password
        newNode%next => null()
    
        if (.not. associated(self%head)) then
            self%head => newNode
        else
            newNode%next => self%head
            self%head => newNode
        end if
    
        !print *, 'pushed:: ', value
        end subroutine push
```
Funcion para buscar un usuario
```fortran
subroutine buscarUsuario(self, nombreUsuario, passwordUsuario,dpiInt ,encontrado, user_node)
            class(listaUser), intent(inout) :: self
            character(len=*), intent(in) :: nombreUsuario, passwordUsuario
            integer*8::dpiInt
            logical, intent(out) :: encontrado
            type(nodeUser), pointer, intent(out) :: user_node
        
            type(nodeUser), pointer :: current
        
            current => self%head
            encontrado = .false.
        
            do while (associated(current))
                if (trim(current%nombre) == trim(nombreUsuario) .and. trim(current%password) == trim(passwordUsuario) &
                    .and. (current%dpi) == dpiInt ) then
                    encontrado = .true.
                    user_node => current
                    exit
                else
                    current => current%next
                end if
            end do
        end subroutine buscarUsuario
```
Funcion para verificar que existe un usuario
```fortran
subroutine existeUsuario(self, nombreUsuario, passwordUsuario, encontrado)
            class(listaUser), intent(inout) :: self
            character(len=*), intent(in) :: nombreUsuario, passwordUsuario
            logical, intent(out) :: encontrado
        
            type(nodeUser), pointer :: current
        
            current => self%head
            encontrado = .false.
        
            do while (associated(current))
                if (trim(current%nombre) == trim(nombreUsuario) .and. trim(current%password) == trim(passwordUsuario)) then
                    encontrado = .true.
                    exit
                else
                    current => current%next
                end if
            end do
        end subroutine existeUsuario
```
Funcion para actualizar un usuario
```fortran
subroutine actualizarUsuario(self, dpi, nuevoNombre, nuevaPassword)
            class(listaUser), intent(inout) :: self
            integer*8, intent(in) :: dpi
            character(len=*), intent(in) :: nuevoNombre, nuevaPassword
            type(nodeUser), pointer :: current
        
            current => self%head
        
            do while (associated(current))
                if (current%dpi == dpi) then
                    current%nombre = nuevoNombre
                    current%password = nuevaPassword
                    print *, "Usuario actualizado exitosamente."
                    return
                end if
                current => current%next
            end do
        
            print *, "Usuario no encontrado."
        end subroutine actualizarUsuario
```
Funcion para eliminar un usuario
```fortran
subroutine eliminarUsuario(self, dpi)
            class(listaUser), intent(inout) :: self
            integer*8, intent(in) :: dpi
            type(nodeUser), pointer :: current, previous
            logical :: encontrado
        
            current => self%head
            previous => null()
            encontrado = .false.
        
            do while (associated(current) .and. .not. encontrado)
                if (current%dpi == dpi) then
                    encontrado = .true.
                    if (associated(previous)) then
                        previous%next => current%next
                    else
                        self%head => current%next
                    end if
                    deallocate(current)
                    print *, 'Usuario eliminado!'
                else
                    previous => current
                    current => current%next
                end if
            end do
        
            if (.not. encontrado) then
                print *, 'Usuario no encontrado.'
            end if
        end subroutine eliminarUsuario
```
Funcion para reportes de cantidad de capas e imagenes
```fortran
subroutine modificarCantidad(self,dpi,numCapas,numImagenes)
            class(listaUser), intent(inout) :: self
            integer ,intent(in) :: numCapas,numImagenes
            integer*8, intent(in) :: dpi
            type(nodeUser), pointer :: current

            current => self%head

            do while (associated(current))
                if (current%dpi == dpi) then
                    current%numCapas = numCapas + current%numCapas
                    current%numImagenes = numImagenes + current%numImagenes
                    
                    return
                end if
                current => current%next
            end do
        
            print *, "Usuario no encontrado."    
        end subroutine modificarCantidad


        subroutine clienteABuscar(self, dpi)
            class(listaUser), intent(inout) :: self
            integer*8, intent(in) :: dpi
            
            type(nodeUser), pointer :: current
        
            current => self%head
        
            do while (associated(current))
                if (current%dpi == dpi) then
                    print *, "DPI: " , current%dpi
                    print *, "Nombre: ", current%nombre
                    print *, "Paasword: ", current%password
                    call current%listaAlbums%printReporte()
                    print *, "Cantidad de capas totales: ", current%numCapas
                    print * , "Cantidad de imagenes totales: " , current%numImagenes
                    
                    return
                end if
                current => current%next
            end do
        
            print *, "Usuario no encontrado."
        end subroutine clienteABuscar
```
Funcion para graficar los clientes
```fortran
 subroutine clientesGraph(self)

            class(listaUser), intent(inout) :: self
            type(nodeUser), pointer :: current
            character(len=:), allocatable :: filepath
            integer :: unit, counter
    
            filepath = trim("clientes") 
            open(unit, file=filepath, status='replace')
            write(unit, *) 'digraph cola {node [fontname="Arial"]'
            write(unit, *) '    node [shape=ellipse]; rankdir = LR'
            current => self%head
            counter = 0
    
            do while (associated(current))
                counter = counter + 1
                write(unit, *) '    "Node', counter, '" [label="', &
                                    "DPI: ", current%dpi, "\n", &
                                    "Nombre: ", current%nombre, "\n", &
                                    '"];'
                if (associated(current%next)) then
                    write(unit, *) '    "Node', counter, '" -> "Node', counter+1, '";'
                end if
                current => current%next
            end do 
            write(unit, *) '}'
            close(unit)
            call system('dot -Tpng ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.png')
            print *, 'Grafica clientes generada!: ', trim(adjustl(filepath)) // '.png'
            call execute_command_line('start '//trim('clientes.png'))
        end subroutine clientesGraph
```
## listaAlbums
Modulo para la lista de albums
```fortran
type,public ::  listaAlbums
            integer :: size = 0
            type(nodeLD), pointer :: head => null()
            type(nodeLD), pointer :: tail => null()
            
        
            contains
                procedure :: add
                procedure :: print
                procedure :: almbumGraphic
                procedure :: remove
                procedure :: buscarAlbum
                procedure :: printReporte
            end type listaAlbums
        
            type,public :: nodeLD
            character(len=100) :: nombre_album
            type(nodeLD), pointer :: next => null()
            type(nodeLD), pointer :: prev => null()
            type(listaImagenes) :: myLista_img
            
            end type nodeLD
        
            contains
        
            subroutine add(self, nombre_album,lista_img)
            class(listaAlbums), intent(inout) :: self
            character(len=100),intent(in) :: nombre_album
            type(listaImagenes),intent(in) :: lista_img
        
            type(nodeLD), pointer :: newNode 
            allocate(newNode)
        
            newNode%nombre_album = nombre_album
            newNode%myLista_img = lista_img
        
            if(.not. associated(self%head)) then
                newNode%next => newNode
                newNode%prev => newNode
                self%head => newNode
                self%tail => newNode
            else
                self%tail%next => newNode
                self%head%prev => newNode
                newNode%next => self%head
                newNode%prev => self%tail
                self%tail => newNode
            end if
        
            self%size = self%size + 1
            
            end subroutine add
```
Funcion para buscar un album
```fortran
subroutine buscarAlbum(self,idImagen)
                class(listaAlbums), intent(in) :: self
                type(nodeLD), pointer :: actual
                integer, intent(in) :: idImagen
                logical :: imagenEncontrada
            
                if (.not. associated(self%head)) then
                    print *, "No hay albumes"
                else
                    actual => self%head
                    do
                        call actual%myLista_img%idEncontrado(idImagen,imagenEncontrada)
                        if(imagenEncontrada)then
                            call actual%myLista_img%delete_by_id(idImagen)
                            print*, "Imagen eliminada de album!"
                        end if
                        actual => actual%next
                        if(associated(actual, self%head)) exit
                    end do
                end if
            end subroutine buscarAlbum
```
Funcion para eliminar album
```fortran

            subroutine remove(self, nombre_album)
            class(listaAlbums), intent(inout) :: self
            character, intent(in) :: nombre_album
            type(nodeLD), pointer :: current
            type(nodeLD), pointer :: next
            type(nodeLD), pointer :: prev
        
            current => self%head
        
            if(.not. associated(self%head)) then
                print*, "Empty list"
            else
                if(self%head%nombre_album == nombre_album) then
                self%head => current%next
                next => current%next
                prev => current%prev
                next%prev => prev
                prev%next => next
        
                deallocate(current)
                self%size = self%size - 1
        
                ! If the list is empty, set the head and tail to null
                if(self%size == 0) then
                    self%head => null()
                    self%tail => null()
                end if
        
                else if (self%tail%nombre_album == nombre_album) then
                current => self%tail
                self%tail => current%prev
        
                next => current%next
                prev => current%prev
                next%prev => prev
                prev%next => next
        
                deallocate(current)
                self%size = self%size - 1
        
                else
                do
                    if(current%nombre_album == nombre_album) then
                    next => current%next
                    prev => current%prev
                    next%prev => prev
                    prev%next => next
                    deallocate(current)
                    self%size = self%size - 1
                    exit
                    end if
                    current => current%next
                    
                end do
                end if
            end if
            end subroutine remove
```
Funcion para grafica de albums
```fortran
 subroutine almbumGraphic(self)
            
                class(listaAlbums), intent(inout) :: self ! referencia a la listaAlbums
            integer :: io,i
            character(len=100) :: nombre
            character(len=100), allocatable :: command
            character(:), allocatable :: contenido
            character(len=30) :: nodoA
            character(len=100) :: filename
            
            type(nodeLD), pointer :: current ! puntero al nodo actual
            
            command = "dot -Tpng ./lista_album.dot -o ./lista_album.png"
            ! puntero al nodo actual
            current => self%head
            io = 1
            contenido = ""
            nodoA = ""


            filename = "./lista_album.png"
            open(newunit=io, file='./lista_album.dot')
            write(io, *) "digraph G {"
            write(io, *) "  node [shape=doubleoctagon];"
            write(io, *) "  rankdir=LR"
        
            if ( .not. associated(current))  then ! si la listaAlbums está vacía
                write(io, *) "  EmptyList;" ! escribir un nodo que diga que la listaAlbums está vacía
            else ! si la listaAlbums no está vacía
                
                
                do ! recorrer la listaAlbums
                    nombre = current%nombre_album
                    nodoA = '"nodeLD'//trim(nombre)//'"'
                
                ! crear el nodo
                    write(io, *) '  "nodeLD'// trim(current%nombre_album)// &
                    '"[label="'// trim(current%nombre_album)// '"];'
                ! escribir las aristas
                    

                    call current%myLista_img%grafica_listaImg(contenido,nodoA,nombre)
                    write(io, *) contenido

                    write(io, *) '  "nodeLD'// trim(current%prev%nombre_album)// &
                    '"-> "nodeLD'// trim(current%nombre_album)// '";' ! arista del nodo anterior al nodo actual
                    write(io, *) '  "nodeLD'// trim(current%next%nombre_album)// &
                    '"-> "nodeLD'// trim(current%nombre_album)// '";' ! arista del nodo siguiente al nodo actual
        
                ! avanzar al siguiente nodo
                current => current%next
                ! si ya se recorrió toda la listaAlbums, salir del ciclo
                if (nombre == self%tail%nombre_album) exit
                end do
            end if
            ! escribir el pie del archivo

            write(io, *) "}"
            ! cerrar el archivo
            close(io)

            call execute_command_line(command, exitstat=i)

            if(i == 1) then
                print *, "Ocurrió un error"
            else
                print *, "Imagen generada satisfactoriamente"
                call execute_command_line('start '// trim(filename))
            end if
            end subroutine almbumGraphic
```
Funcion para reporte de album
```fortran
subroutine printReporte(self)
                class(listaAlbums), intent(in) :: self
                type(nodeLD), pointer :: actual
                ! aux => self%head
        
                print *, ""
                
                if (.not. associated(self%head)) then
                    print *, "No hay albumes"
                else
                    actual => self%head
                    do
                        print *, "-----------------------------"
                        print *, "Nombre: ",actual%nombre_album
                        print *, "cantidad imagenes: " , actual%myLista_img%size
                        print *, "-----------------------------------------"
                        actual => actual%next
                        if(associated(actual, self%head)) exit
                    end do
                end if
            end subroutine printReporte
```
## listaImg
Modulo para la lista de imagenes de cada album
```fortran
 type,public :: listaImagenes
        type(nodeLS), pointer :: head => null() ! head of the list
        integer :: size = 0
        contains
            procedure :: push
            procedure :: print
            procedure :: delete_by_position
            procedure :: grafica_listaImg
            procedure :: delete_by_id
            procedure :: idEncontrado
        end type listaImagenes
    
        type,public :: nodeLS
        integer :: value
        type(nodeLS), pointer :: next
        end type nodeLS
    
        contains
    
        subroutine push(self, value)
        class(listaImagenes), intent(inout) :: self
        integer, intent(in) :: value
    
        type(nodeLS), pointer :: newNode
        allocate(newNode)
    
        newNode%value = value
        newNode%next => null()
    
        if (.not. associated(self%head)) then
            self%head => newNode
        else
            newNode%next => self%head
            self%head => newNode
        end if
    
        self%size = self%size + 1
        !print *, 'pushed:: ', value
        end subroutine push
```
Funcion para eliminar imagen segun su id
```fortran
subroutine delete_by_id(self, id)
            class(listaImagenes), intent(inout) :: self
            integer, intent(in) :: id
            type(nodeLS), pointer :: current, previous
        
            current => self%head
            previous => null()
        
            if (associated(current) .and. current%value == id) then
                self%head => current%next
                deallocate(current)
                return
            end if
        
            do while (associated(current))
                if (current%value == id) then
                    previous%next => current%next
                    deallocate(current)
                    return
                end if
                previous => current
                current => current%next
            end do
        
            ! print *, 'ID no encontrado'
        end subroutine delete_by_id
```
Funcion para encontrar una imagen por id
```fortran
subroutine idEncontrado(self, id, encontrado)
            class(listaImagenes), intent(inout) :: self
            integer, intent(in) :: id
            logical, intent(out) :: encontrado
            type(nodeLS), pointer :: current
        
            current => self%head
            encontrado = .false.
        
            do while (associated(current))
                if (current%value == id) then
                    encontrado = .true.
                    return
                end if
                current => current%next
            end do
        end subroutine idEncontrado
```
Funcion para graficar lista de imagenes
```fortran
subroutine grafica_listaImg(self, contenido, albumes, name_)
            class(listaImagenes), intent(in) :: self
            character(:), allocatable :: contenido, unir, nodo
            character(len=30), intent(in) :: albumes, name_
            integer :: index
            
            type(nodeLS), pointer :: actual

            character(len=20) :: indice, imagen 

            actual => self%head

            contenido = ""
            unir = ""
            index = 1
            

            do while(associated(actual))
                write(indice, '(I5)') index
                write(imagen , '(I5)') actual%value
                nodo = '"nodo'//trim(name_)//trim(imagen )//trim(indice)//'"'
                if(index == 1) then
                    contenido = contenido//trim(albumes)//"->"//nodo
                end if
                contenido = contenido//trim(nodo)//'[label="'//trim(imagen )//'", fillcolor=yellow,  style=filled]'

                if (associated(actual%next)) then
                    unir = unir//nodo//"->"
                else
                    unir = unir//nodo
                end if

                actual => actual%next
                index = index + 1

            end do

            contenido = contenido//unir
            
        end subroutine grafica_listaImg
```
## matrix
Modulo para generar matrices o capas
```fortran
type :: node_val
        private
        logical :: exists = .false.
        character(len=:), allocatable :: valor
    end type node_val

    type,public :: node
        private
        integer :: i,j
        
        character(len=:), allocatable :: valor
        type(node), pointer :: arriba => null()
        type(node), pointer :: abajo => null()
        type(node), pointer :: derecha => null()
        type(node), pointer :: izquierda => null()

    end type node

    type, public :: matrix
        ! private
        type(node), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
        integer :: node_count = 0
    contains
        procedure :: insert
        procedure :: insertarCabeceraFila
        procedure :: insertarCabeceraColumna
        procedure :: buscarFila
        procedure :: buscarColumna
        procedure :: existeNodo
        procedure :: print
        procedure :: imprimirEncabezadoColumnas
        procedure :: obtenerValor
        procedure :: graficar
        procedure :: add_matrix
        procedure :: init
    end type matrix
    
contains

subroutine add_matrix(self, other_matrix)
    class(matrix), intent(inout) :: self
    class(matrix), intent(in) :: other_matrix
    type(node), pointer :: current_node
    type(node), pointer :: next_node
    type(node), pointer :: temp_node

    ! Verificar si other_matrix%root es null
    if (.not. associated(other_matrix%root)) then
        print *, "Error: other_matrix%root es null"
        return
    end if

    ! Inicializar current_node al nodo raíz de other_matrix
    current_node => other_matrix%root

    ! Recorrer la matriz 'other_matrix' e insertar cada valor en 'self'
    do while (associated(current_node))
        ! Imprimir i y j en todos los nodos
        ! print *, "i:", current_node%i
        ! print *, "j:", current_node%j

        ! Determinar el próximo nodo a visitar antes de procesar el nodo actual
        if (associated(current_node%derecha)) then
            next_node => current_node%derecha
            temp_node => current_node%derecha
            do while (associated(temp_node%abajo))
                temp_node => temp_node%abajo
                ! print *, "i:", temp_node%i
                ! print *, "j:", temp_node%j
                if (temp_node%i /= -1 .and. temp_node%j /= -1) then
                    ! print *, "valor:", temp_node%valor
                    call self%insert(temp_node%i, temp_node%j, temp_node%valor)
                    ! print *, "se pudo agregar un nodo :D"
                end if
            end do
        else
            next_node => null()
        end if

        ! Verificar si i y j son diferentes de -1 para considerar el valor del nodo
        if (current_node%i /= -1 .and. current_node%j /= -1) then
            ! Imprimir el valor del nodo
            ! print *, "valor:", current_node%valor
            ! Insertar el nodo actual en la matriz 'self'
            call self%insert(current_node%i, current_node%j, current_node%valor)
            ! print *, "se pudo agregar un nodo :D"
        end if

        ! Mover al siguiente nodo
        current_node => next_node
    end do
end subroutine add_matrix


subroutine init(self)
    class(matrix), intent(inout) :: self
    
    ! Inicializa la matriz combinada
    self%width = 0
    self%height = 0
    self%root => null()
end subroutine init



subroutine insert(self, i, j, valor)
    class(matrix), intent(inout) :: self
    integer, intent(in) :: i
    integer, intent(in) :: j
    character(len=*), intent(in) :: valor  

    type(node), pointer :: nuevo
    type(node), pointer :: fila
    type(node), pointer :: columna

    allocate(nuevo)
    nuevo = node(i=i, j=j, valor=valor)

    if(.not. associated(self%root)) then
        allocate(self%root)
        self%root = node(i=-1, j=-1)
    end if

    fila => self%buscarFila(j)
    columna => self%buscarColumna(i)

    if(i > self%width) self%width = i
    if(j > self%height) self%height = j

    if(.not. self%existeNodo(nuevo)) then
        if(.not. associated(columna)) then
            columna => self%insertarCabeceraColumna(i)
        end if

        if(.not. associated(fila)) then
            fila => self%insertarCabeceraFila(j)
        end if
        call insertarEnColumna(nuevo, fila)
        call insertarEnFila(nuevo, columna)
    end if
end subroutine insert
```
Funcion para retornar un nodo de la matriz
```fortran
function obtenerValor(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j

        type(node), pointer :: cabeceraFila
        type(node), pointer :: columna
        type(node_val) :: val
        cabeceraFila => self%root

        do while(associated(cabeceraFila))
            if(cabeceraFila%j == j) then
                columna => cabeceraFila
                do while(associated(columna)) 
                    if(columna%i == i) then
                        val%valor = columna%valor
                        val%exists = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            cabeceraFila => cabeceraFila%abajo
        end do
        return
    end function obtenerValor
```
Funcion para graficar la matriz
```fortran
subroutine graficar(self)
        class(matrix), intent(in) :: self
        
        integer :: io
        integer :: i
        character(len=10) :: str_i
        character(len=10) :: str_j
        character(len=10) :: str_i_aux
        character(len=10) :: str_j_aux
        character(len=150) :: node_dec
        character(len=20) :: nombre

        character(len=100) :: comando
        character(len=50) :: contenido
        character(:), allocatable :: rank
        character(:), allocatable :: conexion
        character(:), allocatable :: conexionRev
        type(node), pointer :: fila_aux
        type(node), pointer :: columna_aux
        io = 1
        fila_aux => self%root
        comando = "dot -Tpng ./matrix.dot -o ./matrix.png"

        open(newunit=io, file="./matrix.dot")

        write(io, *) "digraph Matrix {"
        write(io, *) 'node[shape = "box"]'

        do while (associated(fila_aux))
            rank = "{rank=same"
            columna_aux => fila_aux
            do while(associated(columna_aux)) 
                write(str_i, '(I10)') columna_aux%i + 1
                write(str_j, '(I10)') columna_aux%j + 1
                nombre = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'
                ! contenidoValor= columna_aux%valor
                ! contenidoValor = "#FFCC33"
                if (columna_aux%i == -1 .and. columna_aux%j == -1) then
                    node_dec = trim(adjustl(nombre))//'[label = "root", group="'//trim(adjustl(str_i))//'"]'

                else if(columna_aux%i == -1) then
                    write(str_j_aux, '(I10)') columna_aux%j
                    ! contenidoValor = "#FFCC27"
                    contenido = trim(adjustl(str_j_aux))
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                    
                else if(columna_aux%j == -1) then
                    ! contenidoValor = "#FFCC27"
                    write(str_i_aux, '(I10)') columna_aux%i
                    contenido = trim(adjustl(str_i_aux))
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                        
                else
                    if(columna_aux%valor .NE. "" ) then
                        contenido = columna_aux%valor
                    else
                        contenido = 'F'
                    end if 
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'",  style = filled, fillcolor = "'//&
                    & trim(contenido)//'" group="'// &
                    & trim(adjustl(str_i))//'"]'
                end if
                write(io, *) node_dec

                if(associated(columna_aux%derecha)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'

                    write(str_i_aux, '(I10)') columna_aux%derecha%i + 1
                    write(str_j_aux, '(I10)') columna_aux%derecha%j + 1

                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if

                if(associated(columna_aux%abajo)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'

                    write(str_i_aux, '(I10)') columna_aux%abajo%i + 1
                    write(str_j_aux, '(I10)') columna_aux%abajo%j + 1

                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if

                rank = rank//';"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'
                columna_aux => columna_aux%derecha
            end do
            rank = rank//'}'
            write(io, *) rank

            fila_aux => fila_aux%abajo
        end do
        write(io, *) "}"
        close(io)
        
        call execute_command_line(comando, exitstat=i)

        if ( i == 1 ) then
            print *,""
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *,""
            print *, "La imagen fue generada exitosamente"
            call execute_command_line('start '// trim("./matrix.png"))
        end if
    end subroutine graficar
```
## uuid_module
Modulo para asignar un id a cada nodo
```fortran
 module uuid_module

        implicit none
    
        private
    
        integer, parameter :: INT64 = selected_int_kind(18)
        integer, parameter :: INT32 = selected_int_kind(9)
    
        integer(INT32), parameter :: mtprng_N = 624_INT32
        integer(INT32), parameter :: mtprng_M = 397_INT32
    
        character(len=1),dimension(0:15),parameter :: hexdigits = &
            ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
    
        type mtprng_state
            integer(INT32) :: mti = -1_INT32
            integer(INT64), dimension(0:mtprng_N-1) :: mt = 0_INT64
        end type
    
        type(mtprng_state) :: rng_state
        logical :: initialized = .false.
        integer :: values_save = 0
        integer(kind=INT32) :: hires_count = 0
    
        integer, save :: clock_seq = 0  !! clock-seq holds a random number
                                        !! constant for the lifetime of the program
                                        !! using this module. That's the best we
                                        !! can do per S 4.1.5
    
        public :: generate_uuid
    
        contains
    !*****************************************************************************************
    
        function generate_uuid(version) result(uuid)
    
            integer, intent(in), optional :: version    !! identifies the version of UUID to be
                                                        !! used (see section 4.1.3 of the RFC).
                                                        !! Only versions 0, 1, and 4 are supported.
                                                        !! Version 0 generates a nil UUID; version 1 a
                                                        !! time-based UUID, and version 4 a
                                                        !! pseudo-randomly-generated UUID.
                                                        !!
                                                        !! Version 1 is the default, and is recommended.
    
            character(len=36) :: uuid
    
            integer(kind=INT64) :: timestamp, node
            integer(kind=INT32) :: clock_sequence
            integer(kind=INT32) :: time_low, time_mid, time_hi_and_version
            integer(kind=INT32) :: clk_seq_hi_res, clk_seq_low
            integer,dimension(8) :: values !! must be default for `date_and_time`
            integer(kind=INT32) :: variant, v
    
            if (.not.initialized) then
                ! Use the current date and time to init mtprng
                ! but this gives limited varaibility, so mix
                ! the result up.  Can we do better? In any
                ! case, this gets passed through a quick
                ! generator inside mtprng_init.
                call date_and_time(values=values)
                values(7) = values(7)*1000+values(5)*100+values(3)*10+values(1)
                values(8) = values(2)*1000+values(4)*100+values(6)*10+values(8)
                call mtprng_init(int(values(7)*10000+values(8), INT32), rng_state)
                clock_seq = int(mtprng_rand64(rng_state), INT32)
                initialized = .true.
            endif
    
            variant = 1
    
            if (present(version)) then
                v = version
            else
                v = 4
            endif
    
            select case (v)
            case (0)
                ! Nil UUID  - S 4.1.7
                uuid = repeat('0',8)//'-'//repeat('0',4)//'-'//repeat('0',4)// &
                        '-'//repeat('0',4)//'-'//repeat('0',12)
                return
            case(1)
                call date_and_time(values=values)
                ! In case of too-frequent requests, we will replace time_low
                ! with the count below ...
                if (all(values==values_save)) then
                    hires_count = hires_count + 1
                else
                    hires_count = 0
                endif
            case(2:3)
                !Unimplemented
                uuid = ''
                return
            case(4)
                continue
            case(5)
                !Unimplemented
                uuid = ''
                return
            case default
                !Unspecified
                uuid = ''
                return
            end select
    
            !4.1.4 Timestamp
            select case(v)
            case(1)
                timestamp = get_utc_since_1582(values)
            case(4)
                timestamp = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 28))
            end select
    
            !4.1.5 Clock Sequence
            ! 14 bits
            select case(v)
            case(1)
                clock_sequence = clock_seq
            case(4)
                clock_sequence = int(mtprng_rand64(rng_state), INT32)
            end select
    
            !4.1.6 Node
            ! 48 bits
            select case(v)
            case(1)
                node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))
                ! No MAC address accessible - see section 4.5 !FIXME
            case(4)
                node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))
            end select
    
            time_low = ibits(timestamp, 0, 32)
            time_mid = ibits(timestamp, 32, 16)
            if (hires_count==0) then
                time_hi_and_version = ior(int(ibits(timestamp, 48, 12), INT32), ishft(v, 12))
            else
                time_hi_and_version = ior(hires_count, ishft(v, 12))
            endif
    
            clk_seq_low = ibits(clock_sequence, 0, 8)
            clk_seq_hi_res = ior(ibits(clock_sequence, 8, 6), ishft(variant, 6))
    
            uuid = int32ToHexOctets(time_low, 4)//"-"// &
                    int32ToHexOctets(time_mid, 2)//"-"// &
                    int32ToHexOctets(time_hi_and_version, 2)//"-"// &
                    int32ToHexOctets(clk_seq_hi_res, 1)// &
                    int32ToHexOctets(clk_seq_low, 1)//"-"// &
                    int64ToHexOctets(node, 6)
```