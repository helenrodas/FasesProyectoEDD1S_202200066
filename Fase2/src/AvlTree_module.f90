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
        procedure :: insertInABB
        procedure :: getABB
        procedure :: getABBInt
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

    subroutine delete(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        self%root => deleteRec(self%root, val)
    end subroutine delete

    subroutine preorden(self)
        class(avl), intent(in) :: self
        
        call preordenRec(self%root)
    end subroutine preorden

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

    function rotacionIzquierda(root) result(rootDerecha)
        type(Node_AVL), pointer, intent(in) :: root
        type(Node_AVL), pointer :: rootDerecha
        type(Node_AVL), pointer :: temp

        rootDerecha => root%right
        temp => rootDerecha%left

        rootDerecha%left => root
        root%right => temp

        root%altura = maximo(obtenerAltura(root%left), obtenerAltura(root%right)) + 1
        rootDerecha%altura = maximo(obtenerAltura(rootDerecha%left), obtenerAltura(rootDerecha%right)) + 1
    end function rotacionIzquierda

    function rotacionDerecha(root) result(rootIzquierda)
        type(Node_AVL), pointer, intent(in) :: root
        type(Node_AVL), pointer :: rootIzquierda
        type(Node_AVL), pointer :: temp

        rootIzquierda => root%left
        temp => rootIzquierda%right

        rootIzquierda%right => root
        root%left => temp

        root%altura = maximo(obtenerAltura(root%left), obtenerAltura(root%right)) + 1
        rootIzquierda%altura = maximo(obtenerAltura(rootIzquierda%left), obtenerAltura(rootIzquierda%right)) + 1
    end function rotacionDerecha

    recursive subroutine obtenerMayorDeMenores(root, mayor)
        type(Node_AVL), pointer :: root, mayor
        if(associated(root%right)) then
            call obtenerMayorDeMenores(root%right, mayor)
        else
            mayor => root
        end if
    end subroutine obtenerMayorDeMenores

    recursive subroutine preordenRec(root)
        type(Node_AVL), pointer, intent(in) :: root

        if(associated(root)) then
            print *, root%Value
            call preordenRec(root%left)
            call preordenRec(root%right)
        end if
    end subroutine preordenRec

    function maximo(left, right) result(res)
        integer, intent(in) :: left
        integer, intent(in) :: right

        integer :: res
        res = right

        if(left >= right) then
            res = left
            return
        end if
    end function maximo

    function obtenerBalance(root) result(res)
        type(Node_AVL), pointer, intent(in) :: root
        integer :: res
        
        res = obtenerAltura(root%right) - obtenerAltura(root%left)
    end function

    function obtenerAltura(n) result(res)
        type(Node_AVL), pointer :: n
        integer :: res
        res = 0

        if(.not. associated(n)) return
        res = n%altura
    end function obtenerAltura



    subroutine avlGraph(this)
    class(avl), intent(inout) :: this
    character(len=:), allocatable :: dotStructure
    character(len=:), allocatable :: createNodes
    character(len=:), allocatable :: linkNodes
    createNodes = ''
    linkNodes = ''


    dotStructure = "digraph G{" // new_line('a')
    dotStructure = dotStructure // "Node_AVL [shape=Mcircle];" // new_line('a')

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

end module avl_module