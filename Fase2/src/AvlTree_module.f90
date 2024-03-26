module avl_module
    use abb_m
    
    implicit none
    private 
    ! Cons
    integer, parameter :: LEFT_HEAVY = -1
    integer, parameter :: BALANCED = 0
    integer, parameter :: RIGHT_HEAVY = +1

    type, public ::  Node_AVL
        integer :: Value
        integer :: Factor
        type(Node_AVL), pointer :: Left => null()
        type(Node_AVL), pointer :: Right => null()
        type(abb),allocatable :: arbol
        integer, dimension(:), allocatable :: capaimg
        
    end type Node_AVL

    type,public :: avl
        type(Node_AVL), pointer :: root => null()
        contains
        procedure :: newTree
        procedure :: insert
        procedure :: avlGraph
        procedure :: insertInABB
        procedure :: getABB
        procedure :: getABBInt
        
    end type avl

    contains

    function NewNode(value) result(nodePtr)
    type(Node_AVL), pointer :: nodePtr
    integer, intent(in) :: value
    allocate(nodePtr)
    nodePtr%Value = value
    nodePtr%Factor = 0
    nodePtr%Left => null()
    nodePtr%Right => null()
    allocate(nodePtr%arbol)
    allocate(nodePtr%capaimg(0))
    
    end function NewNode

    subroutine newTree(self)
    class(avl), intent(inout) :: self
    self%root => null()
    end subroutine newTree

    function rotationII(n, n1) result(result_node)
        type(Node_AVL), pointer :: n, n1, result_node
        
        n%Left => n1%Right
        n1%Right => n
        if (n1%Factor == -1) then
            n%Factor = 0
            n1%Factor = 0
        else
            n%Factor = -1
            n1%Factor = 1
        end if
        result_node => n1
    end function rotationII

    function rotationDD(n, n1) result(result_node)
        type(Node_AVL), pointer :: n, n1, result_node

        n%Right => n1%Left
        n1%Left => n
        if (n1%Factor == 1) then
            n%Factor = 0
            n1%Factor = 0
        else
            n%Factor = 1
            n1%Factor = -1
        end if
        result_node => n1
    end function rotationDD

    function rotationDI(n, n1) result(result_node)
    type(Node_AVL), pointer :: n, n1, result_node, n2

    n2 => n1%Left
    n%Right => n2%Left
    n2%Left => n
    n1%Left => n2%Right
    n2%Right => n1
    if (n2%Factor == 1) then
        n%Factor = -1
    else
        n%Factor = 0
    end if
    if (n2%Factor == -1) then
        n1%Factor = 1
    else
        n1%Factor = 0
    end if
    n2%Factor = 0
    result_node => n2
    end function rotationDI

    function rotationID(n, n1) result(result_node)
        type(Node_AVL), pointer :: n, n1, result_node, n2
        n2 => n1%Right
        n%Left => n2%Right
        n2%Right => n
        n1%Right => n2%Left
        n2%Left => n1
        if (n2%Factor == 1) then
            n1%Factor = -1
        else
            n1%Factor = 0
        end if
        if (n2%Factor == -1) then
            n%Factor = 1
        else
            n%Factor = 0
        end if
        n2%Factor = 0
        result_node => n2
    end function rotationID

    recursive function insert2(root, value, increase) result(result_node)
        type(Node_AVL), pointer :: root, result_node, n1
        logical, intent(out) :: increase
        integer, intent(in) :: value

        if (.not. associated(root)) then
            allocate(result_node)
            root => NewNode(value)
            increase = .true.
        else if (value < root%Value) then
            root%Left => insert2(root%Left, value, increase)
            if (increase) then
            select case (root%Factor)
                case (RIGHT_HEAVY)
                    root%Factor = 0
                    increase = .false.
                case (BALANCED)
                    root%Factor = -1
                case (LEFT_HEAVY)
                    n1 => root%Left
                    if (n1%Factor == -1) then
                        root => rotationII(root, n1)
                    else
                        root => rotationID(root, n1)
                    end if
                    increase = .false.
            end select
            end if
        else if (value > root%Value) then
            root%Right => insert2(root%Right, value, increase)
            if (increase) then
                select case (root%Factor)
                case (RIGHT_HEAVY)
                    n1 => root%Right
                    if (n1%Factor == 1) then
                        root => rotationDD(root, n1)
                    else
                        root => rotationDI(root, n1)
                    end if
                    increase = .false.
                case (BALANCED)
                    root%Factor = 1
                case (LEFT_HEAVY)
                    root%Factor = 0
                    increase = .false.
                end select
            end if
        end if

        result_node => root
    end function insert2

    subroutine insert(tree, value)
    class(avl), intent(inout) :: tree
    integer, intent(in) :: value
    logical :: increase

    increase = .false.
    tree%root => insert2(tree%root, value, increase)
    end subroutine insert



    subroutine avlGraph(this)
    class(avl), intent(inout) :: this
    character(len=:), allocatable :: dotStructure
    character(len=:), allocatable :: createNodes
    character(len=:), allocatable :: linkNodes
    createNodes = ''
    linkNodes = ''


    dotStructure = "digraph G{" // new_line('a')
    dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

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



    subroutine insertInABB(self, id, data)
        class(avl), intent(inout) :: self
        integer, intent(in) :: id
        integer, dimension(:), intent(in) :: data  ! Cambia esto para aceptar un arreglo
        type(Node_AVL), pointer :: node
        integer :: i
    
        node => search(self%root, id)
        if (associated(node)) then
            if (allocated(node%capaimg)) then
                deallocate(node%capaimg)
            end if
            allocate(node%capaimg(size(data)))
            node%capaimg = data
            do i = 1, size(data)
                call node%arbol%insert(data(i))
            end do
        end if
    end subroutine insertInABB
    
    
    recursive function search(root, id) result(node)
        type(Node_AVL), pointer :: root, node
        integer, intent(in) :: id
    
        if (.not. associated(root)) then
            node => null()
        else if (id == root%Value) then
            node => root
        else if (id < root%Value) then
            node => search(root%Left, id)
        else
            node => search(root%Right, id)
        end if
    end function search
    

    function getABB(self, id) result(result)
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
    end function getABB
    


    function getABBInt(self, id) result(result)
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
    end function getABBInt
    
    
    
    recursive function search1(root, id) result(node)
        type(Node_AVL), pointer :: root, node
        integer, intent(in) :: id
    
        if (.not. associated(root)) then
            node => null()
        else if (id == root%Value) then
            node => root
        else if (id < root%Value) then
            node => search(root%Left, id)
        else
            node => search(root%Right, id)
        end if
    end function search1


end module avl_module