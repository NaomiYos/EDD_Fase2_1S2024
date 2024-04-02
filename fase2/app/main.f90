
program main
  use :: json_module
 use ::abb_m
 use :: matrix_m
  implicit none
  
  type(matrix_t) :: matrizcapa
  type(abb) :: arbolb
  type(abb) :: arbolimg
  integer :: opcion,opcion2,opcion_cliente
  character(len=20):: username, password,usernameC, passwordC,dpi,archivo

  do
    print *, 'Selecciona una opcion:'
      print *, '1. Inicio de sesion como administrador'
      print *, '2. Inicio de sesion como cliente'
      print *, '3. Salir'
      read *, opcion
      select case (opcion)
      case (1)
        print *, 'Ingrese el nombre de usuario:'
        read *, username
    
        print *, 'Ingrese la contrasenia:'
        read *, password
        if (username=="admin" .and. password=="EDD2024") then
              print *, 'Bienvenido'
              print *, "1.Carga masiva"
              print *, "2.Operaciones sobre usuarios"
              print *, "3.Arbol B de usuarios"
              read *, opcion2
              select case (opcion2)
              case (1)
              call lclientes()
              case (2)
              !call operacionesusuarios()
              case(3)
              !call arbolb()
              end select
            else
              print *, "Usuario o contraseña incorrecta."
            end if
            
      case (2)
        !print *, 'Ingrese el nombre de usuario:'
        !read *, usernameC
    
       ! print *, 'Ingrese la contrasenia:'
       ! read *, passwordC
       ! print *, 'Ingrese DPI:'
        !read *, dpi

           
       ! 
        do 
        print *, 'Selecciona una opcion:'
        print *, '1. Cargar capas'
        print *, '2. Cargar  Imagenes'
        print *, '3. Cargar  Album'
        print *, '4. Graficar'
        print *, "5.Volver"
        read *, opcion_cliente
        select case (opcion_cliente)
        case (1)
        print *, "5.Volver"
        read *, archivo
          call capas(archivo)
        case (2)
          call imagen()
        case(3)
          call lectoralbum()
        case(4)
          !call arbolb%preorden()
          !call arbolb%graficar()
          
        case(5)
          exit

          end select
        end do
      case (3)
            exit
          case default
            print *, 'Opcion no válida. Por favor, intenta de nuevo.'
      end select
            
end do
contains

subroutine lclientes()
  implicit none
  type(json_file) :: json 
  type(json_value), pointer :: list_p, client_p, attribute_p 
  type(json_core) :: jsonc 
  character(:), allocatable :: nombre, dpi,password
  integer :: i,size
  logical :: found 

  call json%initialize()
  call json%load(filename="clientes.json") 
  call json%info('', n_children=size) 
  call json%get_core(jsonc) 
  call json%get('', list_p, found) 
  
  do i = 1, size
    call jsonc%get_child(list_p, i, client_p, found=found)
    call jsonc%get_child(client_p, 'dpi', attribute_p, found=found)
    if(found) call jsonc%get(attribute_p, dpi)
    print *, "DPI: ", trim(dpi)
    call jsonc%get_child(client_p, 'nombre_cliente', attribute_p, found=found)
    if(found) call jsonc%get(attribute_p, nombre)
    print *, "Nombre: ", trim(nombre)
    call jsonc%get_child(client_p, 'password', attribute_p, found=found)
    if(found) call jsonc%get(attribute_p, password)
    print *, "password: ", trim(password)

    

  end do  
  call json%destroy()

  
end subroutine lclientes

subroutine capas(archivo_capas)
  implicit none
  type(json_file) :: json 
  type(json_value), pointer :: layer, pixel,datos_capa,pixel_attribute,attribute_capa,pixelpointer
  type(json_core) :: jsonc 
  character(len=20), intent(inout) ::archivo_capas
  character(:), allocatable :: color
  integer :: i,psize,fila,columna,id_capa,j,size
  logical :: found 
  
 
  


  call json%initialize()
  call json%load(filename=archivo_capas) 
  call json%info('', n_children=size) 
  call json%get_core(jsonc) 
  call json%get('', layer, found) 
 
  do i = 1, size
    call jsonc%get_child(layer, i, datos_capa, found=found)
    call jsonc%get_child(datos_capa, 'id_capa', attribute_capa, found=found)
    if(found) call jsonc%get(attribute_capa, id_capa)
    print *, "id: ", id_capa
   
    call matrizcapa%init()
    print *, "id insertadp"
    call jsonc%get_child(datos_capa, 'pixeles', attribute_capa, found=found)
    if(found) then
      call jsonc%info(attribute_capa, n_children=psize)
      print *,"Pixeles size: "
      print *, psize
      do j = 1, psize
        call jsonc%get_child(attribute_capa,j,pixel,found=found)
        if(found) then
          call jsonc%get_child(pixel,"fila", pixel_attribute,found=found)

          if (found) then
            call jsonc%get(pixel_attribute,fila )
          end if
          call jsonc%get_child(pixel,"columna", pixel_attribute,found=found)
          if (found) then
            call jsonc%get(pixel_attribute,columna )
          end if
          call jsonc%get_child(pixel,"color", pixel_attribute,found=found)
          if (found) then
            call jsonc%get(pixel_attribute,color)
            
          end if
        print *,"id caa", id_capa
        call matrizcapa%addm(fila,columna,color)
        !call matrizcapa%create_dot()
        end if
      end do
      end if
     call arbolb%insert(id_capa,matrizcapa)
      


    
  end do  
  
  !call arbolb%graficar()
  call json%destroy()

 
end subroutine capas

subroutine imagen()

  
  implicit none
  type(json_file) :: json 
  type(json_value), pointer :: list_images, img, attribute_image,capa
  type(json_core) :: jsonc 
  character(:), allocatable :: capas
  integer :: i,size,size_c,j,id_img,vcapa
  logical :: found 
  integer, dimension (:), allocatable :: darray 
  type(matrix_t) :: mtxcapa
  

  call json%initialize()
  call json%load(filename="imagenes.json") 
  call json%info('', n_children=size) 
  call json%get_core(jsonc) 
  call json%get('', list_images, found) 
  
  do i = 1, size
    call jsonc%get_child(list_images, i, img, found=found)
    call jsonc%get_child(img, 'id', attribute_image, found=found)
    if(found) call jsonc%get(attribute_image,id_img)
    print *, "id: ", id_img
    call jsonc%get_child(img, 'capas', attribute_image, found=found)
    if(found) call jsonc%info(attribute_image,n_children=size_c)
    print *,  "Capas:", size_c
    allocate(darray(size_c))
    do  j=1,size_c
      call jsonc%get_child(attribute_image,j,capa,found=found)
      call jsonc%get(capa,vcapa)
     ! print *,vcapa
      darray(j)=vcapa
      mtxcapa= buscar_m(vcapa) 
      
    end do
      print *,darray
      deallocate (darray)  

  end do  
  call json%destroy()
end subroutine imagen

subroutine lectoralbum()

  implicit none
  type(json_file) :: json 
  type(json_value), pointer :: list_album, album, attribute_album,imag
  type(json_core) :: jsonc 
  character(:), allocatable :: nombre_album
  integer :: i,size,size_c,j,id_albumes,vimage
  logical :: found 
  integer, dimension (:), allocatable :: images
  call json%initialize()
  call json%load(filename="albumes.json") 
  call json%info('', n_children=size) 
  call json%get_core(jsonc) 
  call json%get('', list_album, found) 
  
  do i = 1, size
    call jsonc%get_child(list_album, i, album, found=found)
    call jsonc%get_child(album, 'nombre_album', attribute_album, found=found)
    if(found) call jsonc%get(attribute_album,nombre_album)
    print *, "name: ", nombre_album

    call jsonc%get_child(album, 'imgs', attribute_album, found=found)
    if(found) call jsonc%info(attribute_album,n_children=size_c)
    print *,  "imgs:", size_c
    allocate(images(size_c))
    do  j=1,size_c
      call jsonc%get_child(attribute_album,j,imag,found=found)
      call jsonc%get(imag,vimage)
     ! print *,vcapa
      images(j)=vimage

      
    end do
      print *,images
      deallocate (images)  

  end do  
  call json%destroy()

  
end subroutine lectoralbum




end program main