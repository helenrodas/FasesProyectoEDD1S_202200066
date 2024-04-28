program main
  use:: json_module
  use:: sucursales_module
  implicit none
  
  integer :: option,size
  character(:), allocatable :: id, departamento,direccion ,password
  integer :: i, s1, s2, distancia, imp_mantenimiento
  type(json_file) :: json
  type(json_core) :: jsonc
  type(json_value), pointer :: listPointer, animalPointer, attributePointer,grafo_value
  ! type(json_array), pointer :: grafo2
  ! type(json_object), pointer :: item
  logical :: found


  type(sucursalABB) :: arbolSucursales

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
        call readSucursales("sucursales")
      case(2)
        ! call readRutas("rutas")
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


  subroutine readSucursales(nombreArchivo)
    integer :: i,idAsInt
    character(len=*), intent(in)::nombreArchivo
    character(len=100) :: filename 


    print *, "------------Sucursales---------------"
    call json%initialize()
    filename = trim(nombreArchivo) // '.json'
    call json%load(filename=filename)
    call json%info('',n_children=size)
    call json%get_core(jsonc)
    call json%get('', listPointer, found)

    do i = 1, size
        call jsonc%get_child(listPointer, i, animalPointer, found)

        call jsonc%get_child(animalPointer, 'id', attributePointer, found)
        call jsonc%get(attributePointer, id)

        call jsonc%get_child(animalPointer, 'departamento', attributePointer, found)
        call jsonc%get(attributePointer, departamento)

        call jsonc%get_child(animalPointer, 'direccion', attributePointer, found)
        call jsonc%get(attributePointer, direccion)

        call jsonc%get_child(animalPointer, 'password', attributePointer, found) 
        call jsonc%get(attributePointer, password)

        ! print *, "ID:",id
        ! print *, "Departamento:",departamento
        ! print *, "Direccion:",direccion
        ! print *, "Password:",password
        print *, "----------------------------"
        read(id, *) idAsInt
        call arbolSucursales%insert(idAsInt,departamento,direccion,password)

    end do
    call arbolSucursales%printABB()
    call json%destroy()
    print*,"Archivo sucursales leido exitosamente"
end subroutine readSucursales



! subroutine readRutas(nombreArchivo)
!   character(len=*), intent(in) :: nombreArchivo
!   character(len=100) :: filename
!   integer :: i, size, found, num_edges

!   ! Variables para almacenar los valores de cada borde
!   integer :: s1, s2, distancia, imp_mantenimiento

!   print *, "------------Grafo---------------"

!   ! Inicializar la biblioteca json_module
!   call json%initialize()

!   ! Cargar el archivo JSON
!   filename = trim(nombreArchivo) // '.json'
!   call json%load(filename=filename)

!   call json%get('grafo', grafo_value)
!     call grafo_value%get_array(grafo2)

!     do i = 1, grafo2%size()
!         call grafo2%get(i, item)
!         call item%get('s1', s1)
!         call item%get('s2', s2)
!         call item%get('distancia', distancia)
!         call item%get('imp_mantenimiento', imp_mantenimiento)
!     print *, "Edge ", i
!       print *, "s1:", s1
!       print *, "s2:", s2
!       print *, "distancia:", distancia
!       print *, "imp_mantenimiento:", imp_mantenimiento
!       print *, "----------------------------"
! end do


!   ! Liberar recursos de json_module
!   call json%destroy()

!   print *, "Archivo grafo leído exitosamente"
! end subroutine readRutas





end program main