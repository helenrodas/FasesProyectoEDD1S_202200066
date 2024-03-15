program main
  use:: json_module
  use BTree_module

  implicit none
  integer :: option,size, i,dpiAsInt
  character(:), allocatable :: dpi, nombreCliente, password
  

  type(BTree), pointer :: root => null()
  type(json_file) :: json
  type(json_core) :: jsonc
  type(json_value), pointer :: listPointer, animalPointer, attributePointer
  logical :: found
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
    print *, "...................................."
    print *, "  Bienvenido a Pixel Print Estudio  "
    print *, "...................................."
    print *, "1. Iniciar Sesion"
    print *, "2. Registro de Usuarios"
    print *, "3. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_inicial


  subroutine inicio_sesion()
    character(len=100) :: usuario, password
    
    print *, "--------------------"
    print *, "Ingrese su usuario: "
    read*, usuario
    
    print *, "Ingrese su password: "
    read*, password

    usuario = trim(usuario)
    password = trim(password)
    
    ! print *, "Usuario ingresado: ", usuario
    ! print *, "Contrasena ingresada: ", password
    
    if (usuario == "admin" .and. password == "EDD2024") then
        call op_menuAdmin()
    else
        print *, "El usuario no existe por el momento..."
    end if
  end subroutine inicio_sesion


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

end program main


