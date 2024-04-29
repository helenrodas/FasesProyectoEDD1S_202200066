module sucursales_module
    implicit none
    private

    type, public :: nodeSucursales
        integer :: id
        character(len=:), allocatable :: departamento,direccion,password
        type(nodeSucursales), pointer :: right => null()
        type(nodeSucursales), pointer :: left => null()
    end type nodeSucursales

    type, public :: sucursalABB
        type(nodeSucursales), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: printABB
        procedure :: searchSucursal
        ! procedure :: existeIDABB
    end type sucursalABB

contains   


! subroutine existeIDABB(self, id, encontrado)
!     class(sucursalABB), intent(inout) :: self
!     integer, intent(in) :: id
!     logical, intent(out) :: encontrado
!     type(nodeSucursales), pointer :: node

!     node => existeNodoID(self%root, id)
    
!     if (associated(node)) then
!       encontrado = .true.
!     else
!       encontrado = .false.
!     end if

!     contains

!     recursive function existeNodoID(root, id) result(node)
!         type(nodeSucursales), pointer :: root
!         integer, intent(in) :: id
!         type(nodeSucursales), pointer :: node

!         if (associated(root)) then
!             if (root%id == id) then
!                 node => root
!             else if (id < root%id) then
!                 node => existeNodoID(root%left, id)
!             else
!                 node => existeNodoID(root%right, id)
!             end if
!         else
!             node => null()
!         end if
!     end function existeNodoID
! end subroutine existeIDABB

subroutine searchSucursal(self, id,password,encontrado)
    class(sucursalABB), intent(inout) :: self
    integer, intent(in) :: id
    character(len=*),intent(in)::password
    
    type(nodeSucursales), pointer :: node
    logical, intent(inout) :: encontrado
    encontrado = .false.


    node => existeNodo(self%root, id,password)
    
    if (associated(node)) then
        encontrado = .true.
    else
        encontrado = .false.
    end if

    contains

    recursive function existeNodo(root, id,password) result(node)
        type(nodeSucursales), pointer :: root
        integer, intent(in) :: id
        character(len=*),intent(in)::password
        type(nodeSucursales), pointer :: node

        if (associated(root)) then
            if (root%id == id .and. root%password == password) then
                node => root
            else if (id < root%id) then
                node => existeNodo(root%left, id,password)
            else
                node => existeNodo(root%right, id,password)
            end if
        else
            node => null()
        end if
    end function existeNodo

    
    
 end subroutine searchSucursal



recursive subroutine printTree(root)
    type(nodeSucursales), pointer, intent(inout) :: root

    if (associated(root)) then
        call printTree(root%left)

        print*, "ID: ", root%id
        print*, "Departamento: ", root%departamento
        print*, "Direccion: ", root%direccion
        print*, "Password: ", root%password
        print*, "-------------------------"

        call printTree(root%right)
    end if
end subroutine printTree

subroutine printABB(self)
    class(sucursalABB), intent(inout) :: self

    if (associated(self%root)) then
        call printTree(self%root)
    else
        print*, "El árbol está vacío."
    end if
end subroutine printABB



  
subroutine insert(self, id,departamento,direccion,password)
    class(sucursalABB), intent(inout) :: self
    integer, intent(in) :: id
    character(len=*),intent(in)::departamento,direccion,password

    if (.not. associated(self%root)) then
        allocate(self%root)
        self%root%id = id
        self%root%departamento = departamento
        self%root%direccion = direccion
        self%root%password = password
    else
        call insertRec(self%root, id,departamento,direccion,password)
    end if
end subroutine insert

recursive subroutine insertRec(root, id,departamento,direccion,password)
    type(nodeSucursales), pointer, intent(inout) :: root
    integer, intent(in) :: id
    character(len=*),intent(in)::departamento,direccion,password
    
    if (id < root%id) then
        if (.not. associated(root%left)) then
            allocate(root%left)
            root%left%id = id
            root%left%departamento = departamento
            root%left%direccion = direccion
            root%left%password = password
        else
            call insertRec(root%left, id,departamento,direccion,password)
        end if
    else if (id > root%id) then
        if (.not. associated(root%right)) then
            allocate(root%right)
            root%right%id = id
            root%right%departamento = departamento
            root%right%direccion = direccion
            root%right%password = password
        else
            call insertRec(root%right,id,departamento,direccion,password)
        end if
    end if
end subroutine insertRec

    subroutine delete(self, id)
        class(sucursalABB), intent(inout) :: self
        integer, intent(inout) :: id
    
        self%root => deleteRec(self%root, id)
    end subroutine delete

    recursive function deleteRec(root, id) result(res)
        type(nodeSucursales), pointer :: root
        integer, intent(in) :: id
        type(nodeSucursales), pointer :: res
        type(nodeSucursales), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (id < root%id) then
            root%left => deleteRec(root%left, id)
        else if (id > root%id) then
            root%right => deleteRec(root%right, id)
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
                root%id = temp%id
                root%left => deleteRec(root%left, temp%id)
            end if
        end if

        res => root
    end function deleteRec

    recursive subroutine getMajorOfMinors(root, major)
        type(nodeSucursales), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors




end module sucursales_module