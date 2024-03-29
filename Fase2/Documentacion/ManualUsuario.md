# Manual de usuario
## Helen Rodas - 202200066

### El proyecto Pixel Print Studio es para la empresa "Pixel Print Studio" se dedica a la impresión de figuras de pixel art en distintos tamaños y tipos de papel. La principal funcionalidad de la aplicación consiste en un generador de imágenes por capas, la aplicación contará con un conjunto de capas cargadas previamente y almacenadas en memoria para ser utilizadas, estas capas se utilizarán para generar imágenes hechas con pixeles, cada capa contendrá la información de los distintos píxeles y al colocar una capa sobre otra estas irán formando una imagen más completa.

#### Inicio de Sesion
La aplicacion esta desarrollada en consola por lo que da inicio con el modulo de inicio de sesion que puede ser como admin o un cliente, cada uno tiene diferentes funcionalidades a las cuales puede acceder.
![Imagen](/Imagenes/1.png)

##### Admin
El usuario Admin es un usuario unico que tiene el rol de administrador y ciertas funciones solo habilitadas para el.
- Usuario: Admin
- Password: EDD2024
![Imagen](/Imagenes/4.png)
##### 1. Grafica Arbol B Usuarios
Esta opcion va a graficar en forma de lista los usuarios registrados
![Imagen](/Imagenes/6.png)
![Imagen](/Imagenes/7.png)
##### 2. Operaciones sobre Usuarios
En esta opcion el administrador puede modifcar usuarios.
![Imagen](/Imagenes/8.png)
Tiene las opciones para agregar, modificar y eliminar usuario.
![Imagen](/Imagenes/9.png)
- Agregar: 
Se le va a solicitar los datos para el nuevo usuario
![Imagen](/Imagenes/10.png)
- Modificar: 
Se le va a solicitar el dpi para encontrar al usuario a modificar
![Imagen](/Imagenes/11.png)
- Eliminar: 
Se le va a solicitar el dpi para encontrar al usuario a eliminar
![Imagen](/Imagenes/12.png)
##### 3. Carga masiva de usuarios
Opcion para cargar el archivo json con los usuarios 
![Imagen](/Imagenes/5.png)
##### 4. Reportes
Opcion para mostrar los reportes como administrador
![Imagen](/Imagenes/13.png)
- Buscar Cliente
Va a buscar toda la informacion de un cliente en especifico
![Imagen](/Imagenes/14.png)
- Listar Clientes
Muestra la lista de clientes cargados
![Imagen](/Imagenes/15.png)
##### 5. Cerrar Sesion
Va a cerrar la sesion como admistrador pero guardando los datos.
![Imagen](/Imagenes/16.png)


##### Usuario General
El usuario general es cualquier usuario que este previamente registrado y puede acceder a las funciones de un usuario normal.
![Imagen](/Imagenes/17.png)

##### 1. Carga masiva de archivos
Esta opcion es para la carga de archivos json de capas, imagenes y albums.
![Imagen](/Imagenes/18.png)
##### 2. Generacion de imagenes
Opcion para la generacion de imagenes por diferentes recorridos
![Imagen](/Imagenes/19.png)
- Por recorrido limitado
Consta de 3 opciones de recorrido.
-  - Preorder: Pregunta cuantas capas quiere incluir dentro de ese recorrido
![Imagen](/Imagenes/20.png)
y genera la imagen solicitada.
![Imagen](/Imagenes/21.png)
-  - Inorder:Pregunta cuantas capas quiere incluir dentro de ese recorrido
![Imagen](/Imagenes/22.png)
y muestra la imagen solicitadad
![Imagen](/Imagenes/23.png)
-  - Postorder:Pregunta cuantas capas quiere incluir dentro de ese recorrido
![Imagen](/Imagenes/24.png)
y genera la imagen
![Imagen](/Imagenes/25.png)
- Por arbol de imagenes
Solicitara el id de la imagen que se quiere buscar pero el recorrido que hara en el arbol es por amplitdu.
![Imagen](/Imagenes/26.png)
y genera la imagen
![Imagen](/Imagenes/27.png)
- Por capa
Esta opcion va a buscar una o varias capas a su eleccion para poder graficar
![Imagen](/Imagenes/28.png)
y genera la imagen con las capas solicitadas
![Imagen](/Imagenes/29.png)

##### 3. Ver estado de las estructuras
Muestra el estado de las estructuras
![Imagen](/Imagenes/30.png)
- 1 Ver arbol de imagenes
![Imagen](/Imagenes/31.png)
grafica arbol AVL:
![Imagen](/Imagenes/32.png)
- 2 Ver arbol de capas
![Imagen](/Imagenes/33.png)
grafica arbol ABB:
![Imagen](/Imagenes/34.png)
- 3 Ver listado de Albumes
![Imagen](/Imagenes/35.png)
grafica listado de albumes
![Imagen](/Imagenes/36.png)
- 4 Ver capa
Esta opcion pide el id de la capa a graficar
![Imagen](/Imagenes/37.png)
grafica generada
![Imagen](/Imagenes/38.png)
- 5 Ver imagen y arbol de capas
Esta opcion solicita el id de la imagen para buscar su abb de esa imagen
![Imagen](/Imagenes/39.png)
grafica generada
![Imagen](/Imagenes/40.png)

##### 4. Agregar Modificaciones
Modificaciones de las imagenes del usuario registrado
- 1 Registrar Imagen
Se puede agregar una nueva imagen ademas de agregarle capas a dicha imagen
![Imagen](/Imagenes/41.png)
- 2 Eliminar Imagen
Solicitara el id de la imagen a eliminar tanto del AVL como del album en caso que este agregada a uno.
![Imagen](/Imagenes/42.png)

##### 5. Reportes
Muestra los reportes del usuario
![Imagen](/Imagenes/43.png)
- 1 Top 5 imagenes con mas numero de capas
![Imagen](/Imagenes/44.png)
- 2 Todas las capas que son hojas
![Imagen](/Imagenes/45.png)
- 3 Profundidad del arbol de capas
![Imagen](/Imagenes/46.png)
- 4 Listar las capas en preorden, inorden, postorden
![Imagen](/Imagenes/47.png)
##### 6. Cerrar Sesion 
Opcion para cerrar sesion de usuario pero recordando sus datos.
![Imagen](/Imagenes/48.png)


#### Registro de Usuarios
El menu de inicio de sesion tambien posee la opcion de registrar usuarios para poder crear un cliente nuevo.
![Imagen](/Imagenes/2.png)
Solicitara informacion de nombre usuario, password y dpi para agregarlo
![Imagen](/Imagenes/49.png)

#### Salir
Opcion para salir del programa
![Imagen](/Imagenes/3.png)
