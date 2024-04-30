module tecnicos_module
    implicit none
    private

    integer :: size_tabla = 7
    integer, parameter :: porcentaje_maximo = 70
    
    type nodoTecnico
        integer(8) :: dpiAsInt, telefonoAsInt
        character(:), allocatable::nombre, apellido, genero, direccion
    end type nodoTecnico
    
    
    type, public :: nodoTabla
        integer :: elemento = 0
        type(nodoTecnico), allocatable :: arreglo(:)

        contains
        procedure :: insertar, buscarTecnico, imprimirTecnicos
        procedure, private :: solve_colision
    end type nodoTabla

contains
    subroutine insertar(self, dpiAsInt, nombre, apellido, direccion, telefonoAsInt, genero)
        class(nodoTabla), intent(inout) :: self
        type(nodoTabla) :: tablaNueva
        integer(8), intent(in) :: dpiAsInt, telefonoAsInt
        character(:), allocatable::nombre, apellido, direccion, genero
        type(nodoTecnico), allocatable :: arreglo_anterior(:)
        real :: porcentaje_utilizado
        integer(8) :: posicion
        
        if(.not. allocated(self%arreglo)) then
            allocate(self%arreglo(0:size_tabla-1))
            self%arreglo(:)%dpiAsInt = -1
        end if
        posicion = get_posicion(dpiAsInt)
        if(self%arreglo(posicion)%dpiAsInt /= -1 .and. self%arreglo(posicion)%dpiAsInt /= dpiAsInt) then
            call self%solve_colision(posicion)
        end if
        self%arreglo(posicion)%dpiAsInt=dpiAsInt
        self%arreglo(posicion)%nombre=nombre
        self%arreglo(posicion)%apellido= apellido
        self%arreglo(posicion)%direccion=direccion
        self%arreglo(posicion)%telefonoAsInt=telefonoAsInt
        self%arreglo(posicion)%genero=genero
        self%elemento = self%elemento + 1
        porcentaje_utilizado = (self%elemento * 1.0/size_tabla) * 100
        if(porcentaje_utilizado > porcentaje_maximo) then
            arreglo_anterior = self%arreglo
            deallocate(self%arreglo)
            tablaNueva = rehashing(arreglo_anterior)
            self%arreglo = tablaNueva%arreglo
            self%elemento = tablaNueva%elemento
        end if
    end subroutine insertar

    function rehashing(arreglo_anterior) result(tablaNueva)
        type(nodoTecnico), intent(in) :: arreglo_anterior(:)
        integer :: i
        type(nodoTabla) :: tablaNueva
        size_tabla = size_tabla*2
        allocate(tablaNueva%arreglo(0:size_tabla-1))
        tablaNueva%arreglo(:)%dpiAsInt = -1
        do i = 1, size(arreglo_anterior)
            if(arreglo_anterior(i)%dpiAsInt /= -1) then
            call tablaNueva%insertar(arreglo_anterior(i)%dpiAsInt,arreglo_anterior(i)%nombre,&
            arreglo_anterior(i)%apellido, &
            arreglo_anterior(i)%direccion,&
            arreglo_anterior(i)%telefonoAsInt, arreglo_anterior(i)%genero)
            end if
        end do
    end function rehashing

    subroutine solve_colision(self, posicion)
        class(nodoTabla), intent(inout) :: self
        integer(8), intent(inout) :: posicion
        do while(self%arreglo(posicion)%dpiAsInt /= -1)
            posicion = posicion + 1
            posicion = mod(posicion, size_tabla)
        end do
    end subroutine solve_colision

    function get_posicion(dpiAsInt) result(posicion)
        integer(8), intent(in) :: dpiAsInt
        integer(8) :: posicion
        posicion = mod(dpiAsInt,size_tabla)
    end function get_posicion

    ! subroutine buscarTecnico(self, dpiAsInt)
    !     class(nodoTabla), intent(inout) :: self
    !     integer(8), intent(in) :: dpiAsInt
    !     integer(8) :: posicion
    !     posicion = get_posicion(dpiAsInt)
    !     if (self%arreglo(posicion)%dpiAsInt == dpiAsInt) then
        
    !         print*, 'DPI: ', self%arreglo(posicion)%dpiAsInt
    !         print*, 'Nombre: ', trim(self%arreglo(posicion)%nombre)
    !         print*, 'Apellido: ', trim(self%arreglo(posicion)%apellido)
    !         print*, 'Genero: ', trim(self%arreglo(posicion)%genero)
    !         print*, 'Direccion: ', trim(self%arreglo(posicion)%direccion)
    !         print*, 'Telefono: ', self%arreglo(posicion)%telefonoAsInt
            
    !     else
    !         print*, 'Tecnico', dpiAsInt ,'no encontrado en tabla: '
    !     end if
    ! end subroutine buscarTecnico

    subroutine buscarTecnico(self, dpiAsInt)
        class(nodoTabla), intent(inout) :: self
        integer(8), intent(in) :: dpiAsInt
        integer(8) :: posicion
        if (.not. allocated(self%arreglo)) then
            print*, 'No Existen Tecnicos Registrados.'
            return
        end if
        posicion = get_posicion(dpiAsInt)
        do while (self%arreglo(posicion)%dpiAsInt /= dpiAsInt .and. self%arreglo(posicion)%dpiAsInt /= -1)
            posicion = posicion + 1
            posicion = mod(posicion, size_tabla)
        end do
        if (self%arreglo(posicion)%dpiAsInt == dpiAsInt) then
            print*, 'DPI: ', self%arreglo(posicion)%dpiAsInt
            print*, 'Nombre: ', trim(self%arreglo(posicion)%nombre)
            print*, 'Apellido: ', trim(self%arreglo(posicion)%apellido)
            print*, 'Direccion: ', trim(self%arreglo(posicion)%direccion)
            print*, 'Telefono: ', self%arreglo(posicion)%telefonoAsInt
            print*, 'Genero: ', trim(self%arreglo(posicion)%genero)
        else
            print*, 'No Existe Un Tecnico Con DPI: ',dpiAsInt
        end if
    end subroutine buscarTecnico





    subroutine imprimirTecnicos(self)
        class(nodoTabla), intent(inout) :: self
        integer :: i
        do i = 0, size(self%arreglo)-1
            if (self%arreglo(i)%dpiAsInt /= -1) then
                print*, 'Posicion: ', i
                print*, 'DPI: ', self%arreglo(i)%dpiAsInt
                print*, 'Nombre: ', trim(self%arreglo(i)%nombre)
                print*, 'Apellido: ', trim(self%arreglo(i)%apellido)
                print*, 'Genero: ', trim(self%arreglo(i)%genero)
                print*, 'Direccion: ', trim(self%arreglo(i)%direccion)
                print*, 'Telefono: ', self%arreglo(i)%telefonoAsInt
                
                print*, '------------------------'
            end if
        end do
    end subroutine imprimirTecnicos
    
end module tecnicos_module