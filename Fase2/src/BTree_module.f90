module BTree_module
  implicit none
  private
  public :: BTree, insert, inorder

  type :: BTree
      integer*8 :: dpi
      character(:), allocatable :: name, password
      type(BTree), pointer :: left => null(), right => null()
  end type BTree

contains

  recursive subroutine insert(root, dpi, name, password)
      type(BTree), pointer, intent(inout) :: root
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

  recursive subroutine inorder(root)
      type(BTree), pointer, intent(in) :: root

      if (associated(root)) then
          call inorder(root%left)
          print*, "DPI: ",root%dpi
          print*, "Nombre: ",root%name
          print*, "Password: " ,root%password
          call inorder(root%right)
      end if
  end subroutine inorder

end module BTree_module
