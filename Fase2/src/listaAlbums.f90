module listaAlbums_module
    use listaImg_module
    implicit none
        private 
        type,public ::  listaAlbums
            integer :: size = 0
            type(nodeLD), pointer :: head => null()
            type(nodeLD), pointer :: tail => null()
            
        
            contains
                procedure :: add
                procedure :: print
                procedure :: almbumGraphic
                procedure :: remove
                procedure :: buscarAlbum
                procedure :: printReporte
            end type listaAlbums
        
            type,public :: nodeLD
            character(len=100) :: nombre_album
            type(nodeLD), pointer :: next => null()
            type(nodeLD), pointer :: prev => null()
            type(listaImagenes) :: myLista_img
            
            end type nodeLD
        
            contains
        
            subroutine add(self, nombre_album,lista_img)
            class(listaAlbums), intent(inout) :: self
            character(len=100),intent(in) :: nombre_album
            type(listaImagenes),intent(in) :: lista_img
        
            type(nodeLD), pointer :: newNode 
            allocate(newNode)
        
            newNode%nombre_album = nombre_album
            newNode%myLista_img = lista_img
        
            if(.not. associated(self%head)) then
                newNode%next => newNode
                newNode%prev => newNode
                self%head => newNode
                self%tail => newNode
            else
                self%tail%next => newNode
                self%head%prev => newNode
                newNode%next => self%head
                newNode%prev => self%tail
                self%tail => newNode
            end if
        
            self%size = self%size + 1
            
            end subroutine add

            subroutine print(self)
                class(listaAlbums), intent(in) :: self
                type(nodeLD), pointer :: actual
                ! aux => self%head
        
                
                if (.not. associated(self%head)) then
                    print *, "No hay albumes"
                else
                    actual => self%head
                    do
                        print *, "Nombre Album: ",actual%nombre_album
                        print *, "Imagenes: "
                        call actual%myLista_img%print()
                        actual => actual%next
                        if(associated(actual, self%head)) exit
                    end do
                end if
            end subroutine print

            subroutine buscarAlbum(self,idImagen)
                class(listaAlbums), intent(in) :: self
                type(nodeLD), pointer :: actual
                integer, intent(in) :: idImagen
                logical :: imagenEncontrada
            
                if (.not. associated(self%head)) then
                    print *, "No hay albumes"
                else
                    actual => self%head
                    do
                        call actual%myLista_img%idEncontrado(idImagen,imagenEncontrada)
                        if(imagenEncontrada)then
                            call actual%myLista_img%delete_by_id(idImagen)
                            print*, "Imagen eliminada de album!"
                        end if
                        actual => actual%next
                        if(associated(actual, self%head)) exit
                    end do
                end if
            end subroutine buscarAlbum
            
        
            subroutine remove(self, nombre_album)
            class(listaAlbums), intent(inout) :: self
            character, intent(in) :: nombre_album
            type(nodeLD), pointer :: current
            type(nodeLD), pointer :: next
            type(nodeLD), pointer :: prev
        
            current => self%head
        
            if(.not. associated(self%head)) then
                print*, "Empty list"
            else
                if(self%head%nombre_album == nombre_album) then
                self%head => current%next
                next => current%next
                prev => current%prev
                next%prev => prev
                prev%next => next
        
                deallocate(current)
                self%size = self%size - 1
        
                ! If the list is empty, set the head and tail to null
                if(self%size == 0) then
                    self%head => null()
                    self%tail => null()
                end if
        
                else if (self%tail%nombre_album == nombre_album) then
                current => self%tail
                self%tail => current%prev
        
                next => current%next
                prev => current%prev
                next%prev => prev
                prev%next => next
        
                deallocate(current)
                self%size = self%size - 1
        
                else
                do
                    if(current%nombre_album == nombre_album) then
                    next => current%next
                    prev => current%prev
                    next%prev => prev
                    prev%next => next
                    deallocate(current)
                    self%size = self%size - 1
                    exit
                    end if
                    current => current%next
                    
                end do
                end if
            end if
            end subroutine remove

        
            subroutine almbumGraphic(self)
            
                class(listaAlbums), intent(inout) :: self ! referencia a la listaAlbums
            integer :: io,i
            character(len=100) :: nombre
            character(len=100), allocatable :: command
            character(:), allocatable :: contenido
            character(len=30) :: nodoA
            character(len=100) :: filename
            
            type(nodeLD), pointer :: current ! puntero al nodo actual
            
            command = "dot -Tpng ./lista_album.dot -o ./lista_album.png"
            ! puntero al nodo actual
            current => self%head
            io = 1
            contenido = ""
            nodoA = ""


            filename = "./lista_album.png"
            open(newunit=io, file='./lista_album.dot')
            write(io, *) "digraph G {"
            write(io, *) "  node [shape=doubleoctagon];"
            write(io, *) "  rankdir=LR"
        
            if ( .not. associated(current))  then ! si la listaAlbums está vacía
                write(io, *) "  EmptyList;" ! escribir un nodo que diga que la listaAlbums está vacía
            else ! si la listaAlbums no está vacía
                
                
                do ! recorrer la listaAlbums
                    nombre = current%nombre_album
                    nodoA = '"nodeLD'//trim(nombre)//'"'
                
                ! crear el nodo
                    write(io, *) '  "nodeLD'// trim(current%nombre_album)// &
                    '"[label="'// trim(current%nombre_album)// '"];'
                ! escribir las aristas
                    

                    call current%myLista_img%grafica_listaImg(contenido,nodoA,nombre)
                    write(io, *) contenido

                    write(io, *) '  "nodeLD'// trim(current%prev%nombre_album)// &
                    '"-> "nodeLD'// trim(current%nombre_album)// '";' ! arista del nodo anterior al nodo actual
                    write(io, *) '  "nodeLD'// trim(current%next%nombre_album)// &
                    '"-> "nodeLD'// trim(current%nombre_album)// '";' ! arista del nodo siguiente al nodo actual
        
                ! avanzar al siguiente nodo
                current => current%next
                ! si ya se recorrió toda la listaAlbums, salir del ciclo
                if (nombre == self%tail%nombre_album) exit
                end do
            end if
            ! escribir el pie del archivo

            write(io, *) "}"
            ! cerrar el archivo
            close(io)

            call execute_command_line(command, exitstat=i)

            if(i == 1) then
                print *, "Ocurrió un error"
            else
                print *, "Imagen generada satisfactoriamente"
                call execute_command_line('start '// trim(filename))
            end if
            end subroutine almbumGraphic
            

            subroutine printReporte(self)
                class(listaAlbums), intent(in) :: self
                type(nodeLD), pointer :: actual
                ! aux => self%head
        
                print *, ""
                
                if (.not. associated(self%head)) then
                    print *, "No hay albumes"
                else
                    actual => self%head
                    do
                        print *, "-----------------------------"
                        print *, "Nombre: ",actual%nombre_album
                        print *, "cantidad imagenes: " , actual%myLista_img%size
                        print *, "-----------------------------------------"
                        actual => actual%next
                        if(associated(actual, self%head)) exit
                    end do
                end if
            end subroutine printReporte
end module listaAlbums_module