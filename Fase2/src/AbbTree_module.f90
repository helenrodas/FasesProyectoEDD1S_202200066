module abb_m
    use matrix_m
    implicit none
    private

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
        ! procedure :: insertarEnMatriz
    end type abb

contains   

subroutine buscarId(self, val,fila,columa,color)
    class(abb), intent(inout) :: self
    integer, intent(in) :: val,fila,columa
    type(Node_t), pointer :: node
    character(len=7) ::color

    node => existeNodo(self%root, val)
    
    if (associated(node)) then
      print *, "Nodo encontrado"
      call insertarEnMatriz(node,fila,columa,color)
    else
      print *, "Nodo no encontrado"
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
        call node%matriz_temp%print
    end subroutine insertarEnMatriz
    
end subroutine buscarId


!Esta funcion es para cuando le mando un array de capas
subroutine buscarIdGraph(self, ids)
    class(abb), intent(inout) :: self
    integer, dimension(:), intent(in) :: ids
    integer :: num_ids, i
    type(Node_t), pointer :: node
    type(matrix), pointer :: combined_matrix

    num_ids = size(ids)

    ! Inicializar matriz combinada
    allocate(combined_matrix)
    call combined_matrix%init()

    ! Recorrer cada ID y encontrar el nodo correspondiente
    do i = 1, num_ids
        node => existeNodoGraph(self%root, ids(i))
        if (associated(node)) then
            print *, "Nodo encontrado con ID:", ids(i)
            ! Agregar la matriz del nodo a la matriz combinada
            print*, "matriz del nodo"
            call node%matriz_temp%print()
            call combined_matrix%add_matrix(node%matriz_temp)
        else
            print *, "Nodo no encontrado con ID:", ids(i)
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
      print *, "Nodo encontrado"
      call node%matriz_temp%graficar()
    else
      print *, "Nodo no encontrado"
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


 !Subrutinas del tipo abb
    ! subroutine insert(self, val)
    !     class(abb), intent(inout) :: self
    !     integer, intent(in) :: val

    !     if (.not. associated(self%root)) then
    !         allocate(self%root)
    !         self%root%value = val
    !         allocate(self%root%matriz_temp)  ! Inicializa la matriz aquí
    !     else
    !         call insertRec(self%root, val)
    !     end if
    ! end subroutine insert
    ! recursive subroutine insertRec(root, val)
    !     type(Node_t), pointer, intent(inout) :: root
    !     integer, intent(in) :: val
        
    !     if (val < root%value) then
    !         if (.not. associated(root%left)) then
    !             allocate(root%left)
    !             root%left%value = val
    !             allocate(root%left%matriz_temp)
                
    !         else
    !             call insertRec(root%left, val)
    !         end if
    !     else if (val > root%value) then
    !         if (.not. associated(root%right)) then
    !             allocate(root%right)
    !             root%right%value = val
    !             allocate(root%right%matriz_temp)
    !         else
    !             call insertRec(root%right, val)
    !         end if
    !     end if
    ! end subroutine insertRec



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

    subroutine graph(self, filename)
        class(abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        
        createNodes = ''
        linkNodes = ''

        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

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

    subroutine recorrido_amplitud(self, cadena)
        class(abb), intent(in) :: self
        character(len=:), allocatable, intent(out) :: cadena
        integer :: h, i
    
        cadena = ""
        h = altura(self%root)
        do i = 1, h
            call agregarNivel(self%root, i, cadena)
        end do
    end subroutine recorrido_amplitud
    
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





end module abb_m