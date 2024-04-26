program main
  implicit none
  ! use:: json_module
integer :: option

  do 
    call menu_inicial()
    read(*, *) option
    select case(option)
    case(1)
      call inicio_sesion()
    case(2)
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
    print *, "2. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_inicial


  subroutine inicio_sesion()
    character(len=100) :: usuario, password
    
    print *, "--------------------"
    print *, "Ingrese su usuario: "
    read(*,'(A)') usuario
    
    print *, "Ingrese su password: "
    read*, password

    password = trim(password)
    
    if (usuario == "EDD1S2024" .and. password == "ProyectoFase3") then
        call op_menuAdmin()
    else
          print*, "Usuario no encontrado.."
    end if
        
  end subroutine inicio_sesion


  subroutine op_menuAdmin()
    integer :: option
    do
      call menu_admin()
      read(*, *) option
      
      select case(option)
      case(1)
        call op_CargaArchivos()
      case(2)
        print *, "menu sucursales"
      case(3)
        print *, "menu reportes"
      case(4)
        print *, "menu salida"
        exit
      case default
        print *, "Error!. Seleccione una opcion valida."
      end select
        end do
  end subroutine op_menuAdmin


  subroutine menu_admin()
    print *, "...................................."
    print *, "          Menu Administrador        "
    print *, "...................................."
    print *, "1. Carga de Archivos"
    print *, "2. Sucursales"
    print *, "3. Reportes"
    print *, "4. Cerrar Sesion"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_admin


  subroutine op_CargaArchivos()
    integer :: option
    do
      call menu_cargaArchivos()
      read(*, *) option
      
      select case(option)
      case(1)
        print *, "carga sucursales"
      case(2)
        print *, "carga rutas"
      case(3)
        print *, "menu salida"
        exit
      case default
        print *, "Error!. Seleccione una opcion valida."
      end select
        end do
  end subroutine op_CargaArchivos



  subroutine menu_cargaArchivos()
    print *, "...................................."
    print *, "          Carga de Archivos         "
    print *, "...................................."
    print *, "1. Sucursales"
    print *, "2. Rutas"
    print *, "3. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_cargaArchivos

end program main