module module_btree
    implicit none

    type nodeptr
        type (B_usuario), pointer :: ptr => null()
    end type nodeptr
    
    type B_usuario
        integer :: MAXI = 4
        integer :: MINI = ceiling((dble(4)-1)/2)
        integer*8 :: val(0:5)  ! Cambiado a integer*8
        integer :: num = 0
        type(nodeptr) :: link(0:5)
        type(B_usuario), pointer :: root => null()
        contains
        procedure :: insert
        procedure :: returnRoot
        procedure :: traversal
        procedure :: traversal2
        procedure :: graphBTree
        procedure :: setValue
        procedure :: splitNode
        procedure :: createNode
        procedure :: deleteTree
    end type B_usuario

    contains

    subroutine insert(this,val)
        class(B_usuario), intent(inout) :: this
        integer*8, intent(in) :: val  ! Cambiado a integer*8
        integer*8 :: i  ! Cambiado a integer*8
        type(B_usuario), pointer :: child
        allocate(child)
        if (this%setValue(val, i, this%root, child)) then
            this%root => this%createNode(i, child,this%root)
            print*, "Nodo agregado"
        end if
    end subroutine insert

    function returnRoot(this) result(myRoot)
        class(B_usuario) :: this
        type(B_usuario), pointer :: myRoot
        myRoot => this%root
    end function returnRoot

    recursive function setValue(this,val, pval, node, child) result(res)
        class(B_usuario), intent(inout) :: this
        integer*8, intent(in) :: val  ! Cambiado a integer*8
        integer*8, intent(inout) :: pval  ! Cambiado a integer*8
        type(B_usuario), pointer, intent(inout) :: node
        type(B_usuario), pointer, intent(inout) :: child
        type(B_usuario), pointer :: newnode        
        integer :: pos
        logical :: res
        allocate(newnode)
        if (.not. associated(node)) then            
                pval = val
                child => null()
                res = .true.
                return
        end if
        if (val < node%val(1)) then
            pos = 0
        else
            pos = node%num
            do while (val < node%val(pos) .and. pos > 1) 
            pos = pos - 1
            end do
            if (val == node%val(pos)) then
                print *, "Usuario Duplicado no agregado!"
                res = .false.
                return
            end if
        end if
        if (this%setValue(val, pval, node%link(pos)%ptr, child)) then
            if (node%num < this%MAXI) then
                call insertNode(pval, pos, node, child)
            else
                call this%splitNode(pval, pval, pos, node, child, newnode)
                child => newnode
                res = .true.
                return
            end if
        end if  
        res = .false.
    end function setValue

    subroutine insertNode(val, pos, node, child)
        integer*8, intent(in) :: val  ! Cambiado a integer*8
        integer, intent(in) :: pos
        type(B_usuario), pointer, intent(inout) :: node
        type(B_usuario), pointer, intent(in) :: child
        integer :: j
        j = node%num
        do while (j > pos)
                node%val(j + 1) = node%val(j)
                node%link(j + 1)%ptr => node%link(j)%ptr
                j = j - 1
        end do
        node%val(j + 1) = val
        node%link(j + 1)%ptr => child
        node%num = node%num + 1
    end subroutine insertNode

    subroutine splitNode(this,val, pval, pos, node, child, newnode)
        class(B_usuario), intent(in) :: this
        integer*8, intent(in) :: val  ! Cambiado a integer*8
        integer,intent(in):: pos
        integer*8, intent(inout) :: pval  ! Cambiado a integer*8
        type(B_usuario), pointer, intent(inout) :: node,  newnode
        type(B_usuario), pointer, intent(in) ::  child
        integer :: median, i, j
        if (pos > this%MINI) then
                median = this%MINI + 1
        else
                median = this%MINI
        end if
        if (.not. associated(newnode)) then
            allocate(newnode)
        do i = 0, this%MAXI
                    newnode%link(i)%ptr => null()
            enddo
        end if
        j = median + 1
        do while (j <= this%MAXI)
                newnode%val(j - median) = node%val(j)
                newnode%link(j - median)%ptr => node%link(j)%ptr
                j = j + 1
        end do
        node%num = median
        newnode%num = this%MAXI - median
        if (pos <= this%MINI) then
                call insertNode(val, pos, node, child)
        else
                call insertNode(val, pos - median, newnode, child)
        end if        
        pval = node%val(node%num)        
        newnode%link(0)%ptr => node%link(node%num)%ptr
        node%num = node%num - 1
    end subroutine splitNode

    function createNode(this,val, child,root) result(newNode)
        class(B_usuario), intent(in) :: this
        integer*8, intent(in) :: val  ! Cambiado a integer*8
        type(B_usuario), pointer, intent(in) :: child
        type(B_usuario), pointer, intent(in) :: root    
        type(B_usuario), pointer :: newNode
        integer :: i
        allocate(newNode)
        newNode%val(1) = val
        newNode%num = 1
        newNode%link(0)%ptr => root
        newNode%link(1)%ptr => child
        do i = 2, this%MAXI
            newNode%link(i)%ptr => null()
        end do
    end function createNode

    recursive subroutine traversal(this,myNode)
        class(B_usuario), intent(in) :: this
        type(B_usuario), pointer, intent(in) :: myNode
        integer :: i
        if (associated(myNode)) then
                write (*, '(A)', advance='no') ' ['
                do i = 1, myNode%num
                    write (*,'(I17)', advance='no') myNode%val(i)
                end do
                do i = 0, myNode%num
                    call this%traversal(myNode%link(i)%ptr)
                end do
                write (*, '(A)', advance='no') ' ] '
        end if
    end subroutine traversal

    recursive subroutine traversal2(this,myNode)
        class(B_usuario), intent(in) :: this
        type(B_usuario), pointer, intent(in) :: myNode
        integer :: i
        if (associated(myNode)) then
            write (*, '(A)', advance='no') ' ['
            do i = 1, myNode%num
                write (*,'(I17)', advance='no') myNode%val(i)
            end do
            do i = 0, myNode%num
                call this%traversal(myNode%link(i)%ptr)
            end do
            write (*, '(A)', advance='no') ' ] '
        end if
    end subroutine traversal2

    recursive subroutine graphBTree(this,myNode)
    class(B_usuario), intent(in) :: this
    type(B_usuario), pointer, intent(in) :: myNode
    type(B_usuario), pointer :: current
    character(:),allocatable :: path
    integer*8, pointer :: dpi
    integer :: i,file
    character(200) :: node1,casteo
    path= "graficaBUsuarios.dot"
    open(file, file=path, status="replace")
    write(file, *) 'digraph G {'
    
    if (.not. associated(myNode)) then
        write(file, *) '"empty" [label="Empty papers", shape=component];'
    else
        write (node1,'(I13)') myNode%val(1)
        do i = 1, myNode%num-1
            dpi => myNode%val(i+1)
            if (associated(dpi)) then
                casteo=""
                write (casteo,'(I13)') myNode%val(i+1)
                print *, "casteo", trim(adjustl(casteo))
                node1=trim(adjustl(node1))//","//trim(adjustl(casteo))
            end if
        end do
        write(file, *) " ",'"Node', trim(adjustl(node1)), '" [label="', trim(adjustl(node1)),'"];'
        
        do i = 0, myNode%num
            if (associated(myNode%link(i)%ptr)) then
                call graphrec(myNode%link(i)%ptr,node1,file)
            else
                write (file, *) " "
            end if 
        end do
    end if
    write(file, *) '}'
    close(file)
    call execute_command_line(trim("dot -Tpng graficaBUsuarios.dot -o graficaBUsuarios.png"))
    call system('Start graficaBUsuarios.png')
end subroutine graphBTree

recursive subroutine graphrec(myNode,node1,file)
    type(B_usuario), pointer, intent(in) :: myNode
    type(B_usuario), pointer :: current
    character(200), intent(inout) :: node1
    character(200) :: node2
    integer, intent(inout) :: file
    character(15) :: casteo
    integer :: i
    if (associated(myNode)) then
        write (node2,'(I13)') myNode%val(1)
        do i = 1, myNode%num-1
            casteo=""
            write (casteo,'(I13)') myNode%val(i+1)
            node2=trim(adjustl(node2))//","//trim(adjustl(casteo))
        end do
        
        write(file, *) " ",'"Node', trim(adjustl(node2)), '" [label="', trim(adjustl(node2)),'"];'
        
        write(file, *) " ",'"Node', trim(adjustl(node1)), '" -> "Node', trim(adjustl(node2)), '";'
        do i = 0, myNode%num
            call graphrec(myNode%link(i)%ptr,node2,file)
        end do
    end if
end subroutine graphrec

subroutine deleteTree(this)
    class(B_usuario), intent(inout) :: this
    if (associated(this%root)) then
        deallocate(this%root)
        this%root => null()
        print*, "Árbol eliminado"
    end if
end subroutine deleteTree

end module module_btree