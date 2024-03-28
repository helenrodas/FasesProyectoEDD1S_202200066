module linkedList_module
    use abb_m
    use avl_module
    use listaAlbums_module
    implicit none
        private
        type,public :: listaUser
        type(nodeUser), pointer :: head => null() ! head of the list
    
        contains
            procedure :: push
            procedure :: print
            procedure :: delete_by_position
            procedure :: buscarUsuario
            procedure :: existeUsuario
            procedure :: actualizarUsuario
            procedure :: eliminarUsuario
            procedure :: modificarCantidad
            procedure :: clienteABuscar
            procedure :: clientesGraph
            ! procedure :: grafica_listaImg
        end type listaUser
    
        type,public :: nodeUser
        integer*8 :: dpi
        character(len=100) :: nombre,password
        type(abb) :: tree
        type(avl) :: avlTree
        type(listaAlbums) :: listaAlbums
        type(nodeUser), pointer :: next
        integer :: numImagenes = 0
        integer :: numCapas = 0
        end type nodeUser
    
        contains
    
        subroutine push(self, dpi,nombre,password)
        class(listaUser), intent(inout) :: self
        integer*8, intent(in) :: dpi
        character(len=*), intent(in)::nombre
        character(len=*), intent(in)::password
    
        type(nodeUser), pointer :: newNode
        allocate(newNode)
    
        newNode%dpi = dpi
        newNode%nombre = nombre
        newNode%password = password
        newNode%next => null()
    
        if (.not. associated(self%head)) then
            self%head => newNode
        else
            newNode%next => self%head
            self%head => newNode
        end if
    
        !print *, 'pushed:: ', value
        end subroutine push
    
        subroutine delete_by_position(self, position)
        class(listaUser), intent(inout) :: self
        integer, intent(in) :: position
        type(nodeUser), pointer :: current, previous
        integer :: counter
    
        current => self%head
        previous => null()
    
        if(position == 1) then
            self%head => current%next
            deallocate(current)
            return
        end if
    
        counter = 1
        do while (associated(current) .and. counter < position)
            previous => current
            current => current%next
            counter = counter + 1
        end do
    
        if (.not. associated(current)) then
            print *, 'Position not found'
            return
        end if
    
        previous%next => current%next
        deallocate(current)
        end subroutine delete_by_position
    

        subroutine print(self)
        class(listaUser), intent(in) :: self
    
        type(nodeUser), pointer :: current
    
        current => self%head
    
        do while (associated(current))
            print*, "----------------------------"
            print *,"DPI: ", current%dpi
            print *, "Nombre: ", current%nombre
            print *, "Password: ", current%password
            current => current%next
        end do
        end subroutine print


        subroutine buscarUsuario(self, nombreUsuario, passwordUsuario, encontrado, user_node)
            class(listaUser), intent(inout) :: self
            character(len=*), intent(in) :: nombreUsuario, passwordUsuario
            logical, intent(out) :: encontrado
            type(nodeUser), pointer, intent(out) :: user_node
        
            type(nodeUser), pointer :: current
        
            current => self%head
            encontrado = .false.
        
            do while (associated(current))
                if (trim(current%nombre) == trim(nombreUsuario) .and. trim(current%password) == trim(passwordUsuario)) then
                    encontrado = .true.
                    user_node => current
                    exit
                else
                    current => current%next
                end if
            end do
        end subroutine buscarUsuario

        subroutine existeUsuario(self, nombreUsuario, passwordUsuario, encontrado)
            class(listaUser), intent(inout) :: self
            character(len=*), intent(in) :: nombreUsuario, passwordUsuario
            logical, intent(out) :: encontrado
        
            type(nodeUser), pointer :: current
        
            current => self%head
            encontrado = .false.
        
            do while (associated(current))
                if (trim(current%nombre) == trim(nombreUsuario) .and. trim(current%password) == trim(passwordUsuario)) then
                    encontrado = .true.
                    exit
                else
                    current => current%next
                end if
            end do
        end subroutine existeUsuario
        

        subroutine actualizarUsuario(self, dpi, nuevoNombre, nuevaPassword)
            class(listaUser), intent(inout) :: self
            integer*8, intent(in) :: dpi
            character(len=*), intent(in) :: nuevoNombre, nuevaPassword
            type(nodeUser), pointer :: current
        
            current => self%head
        
            do while (associated(current))
                if (current%dpi == dpi) then
                    current%nombre = nuevoNombre
                    current%password = nuevaPassword
                    print *, "Usuario actualizado exitosamente."
                    return
                end if
                current => current%next
            end do
        
            print *, "Usuario no encontrado."
        end subroutine actualizarUsuario
        
        subroutine eliminarUsuario(self, dpi)
            class(listaUser), intent(inout) :: self
            integer*8, intent(in) :: dpi
            type(nodeUser), pointer :: current, previous
            logical :: encontrado
        
            current => self%head
            previous => null()
            encontrado = .false.
        
            do while (associated(current) .and. .not. encontrado)
                if (current%dpi == dpi) then
                    encontrado = .true.
                    if (associated(previous)) then
                        previous%next => current%next
                    else
                        self%head => current%next
                    end if
                    deallocate(current)
                    print *, 'Usuario eliminado!'
                else
                    previous => current
                    current => current%next
                end if
            end do
        
            if (.not. encontrado) then
                print *, 'Usuario no encontrado.'
            end if
        end subroutine eliminarUsuario

        subroutine modificarCantidad(self,dpi,numCapas,numImagenes)
            class(listaUser), intent(inout) :: self
            integer ,intent(in) :: numCapas,numImagenes
            integer*8, intent(in) :: dpi
            type(nodeUser), pointer :: current

            current => self%head

            do while (associated(current))
                if (current%dpi == dpi) then
                    current%numCapas = numCapas + current%numCapas
                    current%numImagenes = numImagenes + current%numImagenes
                    
                    return
                end if
                current => current%next
            end do
        
            print *, "Usuario no encontrado."    
        end subroutine modificarCantidad


        subroutine clienteABuscar(self, dpi)
            class(listaUser), intent(inout) :: self
            integer*8, intent(in) :: dpi
            
            type(nodeUser), pointer :: current
        
            current => self%head
        
            do while (associated(current))
                if (current%dpi == dpi) then
                    print *, "DPI: " , current%dpi
                    print *, "Nombre: ", current%nombre
                    print *, "Paasword: ", current%password
                    call current%listaAlbums%printReporte()
                    print *, "Cantidad de capas totales: ", current%numCapas
                    print * , "Cantidad de imagenes totales: " , current%numImagenes
                    
                    return
                end if
                current => current%next
            end do
        
            print *, "Usuario no encontrado."
        end subroutine clienteABuscar


        subroutine clientesGraph(self)

            class(listaUser), intent(inout) :: self
            type(nodeUser), pointer :: current
            character(len=:), allocatable :: filepath
            integer :: unit, counter
    
            filepath = trim("clientes") 
            open(unit, file=filepath, status='replace')
            write(unit, *) 'digraph cola {node [fontname="Arial"]'
            write(unit, *) '    node [shape=ellipse]; rankdir = LR'
            current => self%head
            counter = 0
    
            do while (associated(current))
                counter = counter + 1
                write(unit, *) '    "Node', counter, '" [label="', &
                                    "DPI: ", current%dpi, "\n", &
                                    "Nombre: ", current%nombre, "\n", &
                                    '"];'
                if (associated(current%next)) then
                    write(unit, *) '    "Node', counter, '" -> "Node', counter+1, '";'
                end if
                current => current%next
            end do 
            write(unit, *) '}'
            close(unit)
            call system('dot -Tpng ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.png')
            print *, 'Grafica clientes generada!: ', trim(adjustl(filepath)) // '.png'
            call execute_command_line('start '//trim('clientes.png'))
        end subroutine clientesGraph


    end module linkedList_module