# Manual Tecnico

## Helen Rodas - 202200066

### El proyecto de Pixel Print Studio consta una aplicacion en consola desarrollada en el lenguaje Fortran para permitir a los clientes de la empresa Pixel Print Studio registrar imágenes especiales construidas por capas. Para poder hacer uso de la aplicación el cliente debe registrarse.

## Lectura de json
Se uso la libreria json_module para la lectura de archivos json, la cual se importo json_file,json_core,json_value


## main
En este archivo se encuentra en la carpeta app pues es donde inicia el proyecto, imprimo menus de opciones para que puedan mostrarse en consola.

Se inicia con el menu para iniciar sesion, solo puede iniciar sesion el usuario administrador.
Sus credenciales son: Usuario: EDD1S2024  Contrase;a: ProyectoFase3
```fortran
  subroutine menu_inicial()
    print *, " "
    print *, "...................................."
    print *, "  Bienvenido a Pixel Print Estudio  "
    print *, "...................................."
    print *, "1. Iniciar Sesion"
    print *, "2. Salir"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_inicial
```

Esta es la funcion para validar que las credenciales del administrador sean correctas.
```fortran
subroutine inicio_sesion()
    character(len=100) :: usuario, password
    
    print *, "--------------------"
    print *, "Ingrese su usuario: "
    read(*,'(A)') usuario
    
    print *, "Ingrese su password: "
    read*, password

    password = trim(password)
    
    if (usuario == "EDD1S2024" .and. password == "ProyectoFase3") then
        call op_menuAdmin()
    else
          print*, "Usuario no encontrado.."
    end if
        
  end subroutine inicio_sesion
```



Una vez iniciada la sesion como administrador puede escoger diferentes opciones del menu administrador.
```fortran
subroutine menu_admin()
    print *, "...................................."
    print *, "          Menu Administrador        "
    print *, "...................................."
    print *, "1. Carga de Archivos"
    print *, "2. Sucursales"
    print *, "3. Reportes"
    print *, "4. Cerrar Sesion"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_admin
```

## 1. Carga de archivos
La opcion carga de archivos va a ser capaz de leer los archivos json de tipo sucursales y rutas.

Funcion para leer el archivo .json de sucursales.
```fortran
subroutine readSucursales(nombreArchivo)
    integer :: i
    character(len=*), intent(in)::nombreArchivo
    character(len=100) :: filename 


    print *, "------------Sucursales---------------"
    call json%initialize()
    filename = trim(nombreArchivo) // '.json'
    call json%load(filename=filename)
    call json%info('',n_children=size)
    call json%get_core(jsonc)
    call json%get('', listPointer, found)

    do i = 1, size
        call jsonc%get_child(listPointer, i, animalPointer, found)

        call jsonc%get_child(animalPointer, 'id', attributePointer, found)
        call jsonc%get(attributePointer, id)

        call jsonc%get_child(animalPointer, 'departamento', attributePointer, found)
        call jsonc%get(attributePointer, departamento)

        call jsonc%get_child(animalPointer, 'direccion', attributePointer, found)
        call jsonc%get(attributePointer, direccion)

        call jsonc%get_child(animalPointer, 'password', attributePointer, found) 
        call jsonc%get(attributePointer, password)

        read(id, *) idAsInt
        call arbolSucursales%insert(idAsInt,departamento,direccion,password)

    end do
    call arbolSucursales%printABB()
    call json%destroy()
    print*,"Archivo sucursales leido exitosamente"
end subroutine readSucursales
```
Estos datos se van a agregar a un arbol ABB.
## sucursalesABB
Esta es la funcion para insertar los datos como id, departamento, direccion y password de cada sucursal.
```fortran
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
```
## 2. Sucursales
Esta opcion del menu administrador permite iniciar sesion en una sucursal, se debe ingresar el id de la sucursal y su password.
```fortran
subroutine sesion_sucursal()
    character(len=100) ::  password
    
    logical :: existeSucursal
    print *, "--------------------"
    print *, "Ingrese el ID de la sucursal: "
    read(*,*) idAsInt
    
    print *, "Ingrese el password: "
    read*, password

    password = trim(password)
    
    call arbolSucursales%searchSucursal(idAsInt,password,existeSucursal)
    if(existeSucursal) then
      call op_sucursales()
    else
      print*, "sucursal no encontrado..."
    end if
  end subroutine sesion_sucursal
```
Dicha funcion va a recorrer el arbol ABB hasta encontrar la sucursal que se ingresaron sus datos, caso contrario mostrara un mensaje que la sucursal no fue encontrada. Va a usar de apoyo a una funcion recursiva para que encuentre el nodo, cuando retorna el nodo que se estaba buscando regresara un true porque encontro el nodo, caso contrario retorna un false.

```fortran
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
```

Una vez encontrada la sucursal va a ingresar al menu de sucursales el cual incluye nuevas opciones para ejecutar.
```fortran
  subroutine menu_sucursales()
    print *, "...................................."
    print *, "          Menu Sucursales        "
    print *, "...................................."
    print *, "1. Carga de Tecnicos"
    print *, "2. Generar recorrido mas optimo"
    print *, "3. Informacion Tecnico"
    print *, "4. Listar Tecnicos"
    print *, "5. Reporte"
    print *, "6. Salida"
    print *, "...................................."
    print *, "Ingrese el numero de la opcion deseada:"
  end subroutine menu_sucursales
```
## 1. Carga de Tecnicos
Esta opcion es para cargar el archivo .json de los tecnicos.
```fortran
subroutine readTecnicos(nombreArchivo)

  integer :: i
  character(len=*), intent(in)::nombreArchivo
  character(len=100) :: filename 
  integer*8 :: dpiAsInt,telefonoAsInt

  type(nodoTabla),pointer :: tablaTecnicos
  allocate(tablaTecnicos)

  print *, "------------Tecnicos---------------"
  call json%initialize()
  filename = trim(nombreArchivo) // '.json'
  call json%load(filename=filename)
  call json%info('',n_children=size)
  call json%get_core(jsonc)
  call json%get('', listPointer, found)

  do i = 1, size
      call jsonc%get_child(listPointer, i, animalPointer, found)

      call jsonc%get_child(animalPointer, 'dpi', attributePointer, found)
      call jsonc%get(attributePointer, dpi)

      call jsonc%get_child(animalPointer, 'nombre', attributePointer, found)
      call jsonc%get(attributePointer, nombre)

      call jsonc%get_child(animalPointer, 'apellido', attributePointer, found)
      call jsonc%get(attributePointer, apellido)

      call jsonc%get_child(animalPointer, 'genero', attributePointer, found) 
      call jsonc%get(attributePointer, genero)

      call jsonc%get_child(animalPointer, 'direccion', attributePointer, found) 
      call jsonc%get(attributePointer, direccion)

      call jsonc%get_child(animalPointer, 'telefono', attributePointer, found) 
      call jsonc%get(attributePointer, telefono)

      read(dpi, *) dpiAsInt
      read(telefono, *) telefonoAsInt

      call tablaTecnicos%insertar(dpiAsInt,nombre,apellido,direccion,telefonoAsInt,genero)
  
    end do
  call json%destroy()

  call arbolSucursales%insert_tabla(idAsInt,tablaTecnicos)
  deallocate(tablaTecnicos)

  print*,"Archivo Tecnicos leido exitosamente"
end subroutine readTecnicos
```
Dentro de dicha fucnion se utilizaron otras fucniones pues estos datos se van a incluir dentro de una tabla hash por lo tanto primero se insertan los datos dentro de una tabla hash.

```fortran
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
```
Una vez creada la tabla hash se buscara el nodo en el que se quiere agregar la tabla ya que no todos los nodos tendran la misma tabla o tendran una tabla de tecnicos.

```fortran
subroutine insert_tabla(self, id, tablaTecnicos)

        class(sucursalABB), intent(inout) :: self
        integer, intent(in) :: id
        type(nodoTabla), intent(in) :: tablaTecnicos
    
        print *, "Tabla hash por insertar"
    
        if(associated(self%root)) then
            call agregarEnTabla(self%root, id, tablaTecnicos)
        else
            print*, "Tabla no insertada, sucursal no encontrada"
        end if
    
    end subroutine insert_tabla
    

    recursive subroutine agregarEnTabla(root, id, tablaTecnicos)
        type(nodeSucursales), pointer, intent(inout) :: root
        integer, intent(in) :: id
        type(nodoTabla), intent(in) :: tablaTecnicos
    
        if (associated(root)) then
            if (id == root%id) then
                root%tablaTecnicos = tablaTecnicos
                print *, "Tabla hash insertada correctamente."
                print *, ""
            else if (id < root%id) then
                call agregarEnTabla(root%left, id, tablaTecnicos)
            else
                call agregarEnTabla(root%right, id, tablaTecnicos)
            end if
        end if
    
    end subroutine agregarEnTabla
```
## 3. Informacion Tecnico
Se debe de solicitar el id del técnico y se debe de mostrar toda la información del mismo.
```fortran
  subroutine buscarTecnico()
    character(len=100) ::  password
    integer*8 :: dpiAsInt
    logical :: existeSucursal
    print *, "--------------------"
    print *, "Ingrese el DPI del tecnico: "
    read(*,*) dpiAsInt

    call arbolSucursales%getTablaSucursal(idAsInt,dpiAsInt)

  end subroutine buscarTecnico
```
Esta funcion llama a otra funcion la cual va a ir al nodo en el que se entro sesion y va a mandar el dpi del tecnico al cual se quiere buscar su informacion. Esta es la funcion recursiva que busca en el arbol abb el nodo de la sucursal.
```fortran
recursive subroutine buscarEnTablaTecnico(root, idSucursal,dpiTecnico)

        type(nodeSucursales), pointer, intent(inout) :: root
        integer, intent(in) :: idSucursal
        integer*8,intent(in) :: dpiTecnico
        type(nodoTabla), pointer :: tablaTecnicos
    
        if (associated(root)) then
            if (idSucursal == root%id) then
                tablaTecnicos => root%tablaTecnicos
                call tablaTecnicos%buscarTecnico(dpiTecnico)
            else if (idSucursal < root%id) then
                call buscarEnTablaTecnico(root%left, idSucursal,dpiTecnico)
            else
                call buscarEnTablaTecnico(root%right, idSucursal,dpiTecnico)
            end if
        end if
    
    end subroutine buscarEnTablaTecnico
```

Una vez encontrado el nodo ingresa a la tabla para buscar al tecnico en especifico.
```fortran
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
```

## 4.Listar Tecnicos
Muestra un listado de todos los tecnicos cargados
```fortran
subroutine imprimirTecnicos(self)
        class(nodoTabla), intent(inout) :: self
        integer :: i
    
        print '(1x, A, 5x, A, 5x, A, 5x, A, 5x, A, 5x, A, 5x, A)', &
            "Posicion", "DPI", "Nombre", "Apellido", "Genero", "Direccion", "Telefono"
    
        print '(1x, A)', repeat("-", 80)
    
        do i = 0, size(self%arreglo)-1
            if (self%arreglo(i)%dpiAsInt /= -1) then
                print '(1x, I0, 5x, I0, 5x, A, 5x, A, 5x, A, 5x, A, 5x, I0)', &
                    i, self%arreglo(i)%dpiAsInt, trim(self%arreglo(i)%nombre), &
                    trim(self%arreglo(i)%apellido), trim(self%arreglo(i)%genero), &
                    trim(self%arreglo(i)%direccion), self%arreglo(i)%telefonoAsInt
            end if
        end do
    end subroutine imprimirTecnicos
```

## 6. Salida
Funcion para cerrar sesion.
