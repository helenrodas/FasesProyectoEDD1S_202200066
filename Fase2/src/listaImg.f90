module listaImg_module
    implicit none
        private
        type,public :: listaImagenes
        type(nodeLS), pointer :: head => null() ! head of the list
    
        contains
            procedure :: push
            procedure :: print
            procedure :: delete_by_position
            procedure :: grafica_listita
        end type listaImagenes
    
        type,public :: nodeLS
        integer :: value
        type(nodeLS), pointer :: next
        end type nodeLS
    
        contains
    
        subroutine push(self, value)
        class(listaImagenes), intent(inout) :: self
        integer, intent(in) :: value
    
        type(nodeLS), pointer :: newNode
        allocate(newNode)
    
        newNode%value = value
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
        class(listaImagenes), intent(inout) :: self
        integer, intent(in) :: position
        type(nodeLS), pointer :: current, previous
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
        class(listaImagenes), intent(in) :: self
    
        type(nodeLS), pointer :: current
    
        current => self%head
    
        do while (associated(current))
            print *, current%value
            current => current%next
        end do
        end subroutine print


        subroutine grafica_listita(self, contenido, albumes, name_)
            class(listaImagenes), intent(in) :: self
            character(:), allocatable :: contenido, unir, nodo
            character(len=30), intent(in) :: albumes, name_
            integer :: index
            
            type(nodeLS), pointer :: actual

            character(len=20) :: indice, imagen 

            actual => self%head

            contenido = ""
            unir = ""
            index = 1
            

            do while(associated(actual))
                write(indice, '(I5)') index
                write(imagen , '(I5)') actual%value
                nodo = '"nodo'//trim(name_)//trim(imagen )//trim(indice)//'"'
                if(index == 1) then
                    contenido = contenido//trim(albumes)//"->"//nodo
                end if
                contenido = contenido//trim(nodo)//'[label="'//trim(imagen )//'", fillcolor=yellow,  style=filled]'

                if (associated(actual%next)) then
                    unir = unir//nodo//"->"
                else
                    unir = unir//nodo
                end if

                actual => actual%next
                index = index + 1

            end do

            contenido = contenido//unir
            
        end subroutine grafica_listita

    end module listaImg_module