program main
  use :: json_module
  use :: Avl_Tree
  use::routes
  use:: hash_table_m
  implicit none
  type(Tree_t) :: arbol_avl
  type(HashTable)::tablahash
type(edge_list) :: mygraph
  integer :: opcion2,opcion_archivo,sucursal
  character(len=20):: username, password,archivo,pass_sucursal
  call arbol_avl%newTree()
 ! print *, 'Ingrese el nombre de usuario:'
  !read *, username

  !print *, 'Ingrese la contrasenia:'
 ! read *, password
 ! if (username=="EDD1S2024" .and. password=="ProyectoFase3") then
    do 
        print *, 'Bienvenido'
        print *, "1.Carga de archivos"
        print *, "2.Sucursales"
        print *, "3.Reportes"
        read *, opcion2
        select case (opcion2)
        case (1)
          print *, "1.Carga de Sucursales"
          print *, "2.Carga de rutas"
          read *, opcion_archivo
          select case (opcion_archivo)
            case (1)
              print *, "Carga de sucursales, ingresa el nombre del archivo"
              read *,archivo
              call lector_sucursales(archivo)
            case(2)
              print *, "Carga de grafos, ingresa el nombre del archivo"
              read *,archivo
              call lector_rutas(archivo)
            end select

        case(2)
          print *,"ingresa el id de la sucursal"
          read *, sucursal
          print *,"ingresa la contraseña"
          read *,password
          print *, "Carga de tecnicos, ingresa el nombre del archivo"
              read *,archivo
          call lector_tecnico(archivo,sucursal,password)
          !call tablahash%print()
           
        case(3)
          
          
          !call mygraph%imprimirdatos()
         

        end select
      end do
  !else
        print *, "Usuario o contraseña incorrecta."
  !end if
  contains
 
subroutine lector_sucursales(ar_sucursales)
  implicit none
  type(json_file) :: json 
  type(json_value), pointer :: lists,sucursales,attribute,sucur_pointer,deptPointer,partPointer
  type(json_core) :: jsonc 
  character(len=20), intent(inout) ::ar_sucursales
  character(:), allocatable :: departamento,direccion,password
  integer:: size,id_sucursal,i
  logical :: found 
  call json%initialize()
        call json%load(filename=ar_sucursales)
        call json%print()
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', sucur_pointer, found)
        

        do i = 1, size
            call jsonc%get_child(sucur_pointer, i, deptPointer, found=found)


            call jsonc%get_child(deptPointer, 'id', partPointer, found=found)
            if (found) then
                call jsonc%get(partPointer, id_sucursal)
                print *, 'ID: ', id_sucursal
            end if
            
            call jsonc%get_child(deptPointer, 'departamento', partPointer, found=found)
            if (found) then
                call jsonc%get(partPointer, departamento)
                print *, 'Departamento: ', departamento
            end if
            
            call jsonc%get_child(deptPointer, 'direccion', partPointer, found=found)
            if (found) then
                call jsonc%get(partPointer, direccion)
                print *, 'Dirección: ', direccion
            end if
            
            call jsonc%get_child(deptPointer, 'password', partPointer, found=found)
            if (found) then
                call jsonc%get(partPointer, password)
                print *, 'Contraseña: ', password
            end if
            call arbol_avl%insert(id_sucursal, departamento, direccion, password)
       
        end do
        call arbol_avl%graficaravl()
        call json%destroy()
end subroutine lector_sucursales


subroutine lector_rutas(ar_rutas)
  implicit none
  type(json_file) :: json 
  type(json_value), pointer :: listr,grafos,attribute,attributegraf,grafo,ruta
  type(json_core) :: jsonc 
  character(len=20), intent(inout) ::ar_rutas
  integer:: size,id,sizegr,s1,s2,distancia,imp_mantenimiento,i,j,position
  logical :: found 

  call json%initialize()
  call json%load(filename=ar_rutas) 
  call json%info('', n_children=size) 
  call json%get_core(jsonc) 
  call json%get('', listr, found) 

  do i = 1, size
    call jsonc%get_child(listr, i, ruta, found=found)
    call jsonc%get_child(ruta, 'grafos', attribute, found=found)
    if(found) then
      call jsonc%info(attribute, n_children=sizegr)
     
      do j = 1, sizegr
        call jsonc%get_child(attribute,j,grafo,found=found)
        if(found) then
          call jsonc%get_child(grafo,"s1", attributegraf,found=found)
          if (found) then
            call jsonc%get(attributegraf,s1)
            print *,s1
          end if
          call jsonc%get_child(grafo,"s2", attributegraf,found=found)
          if (found) then
            call jsonc%get(attributegraf,s2)
            print *,s2
          end if
          call jsonc%get_child(grafo,"distancia", attributegraf,found=found)
          if (found) then
              call jsonc%get(attributegraf,distancia)
              print *,distancia
          end if
          call jsonc%get_child(grafo,"imp_mantenimiento", attributegraf,found=found)
          if (found) then
                call jsonc%get(attributegraf,imp_mantenimiento)
                print *,imp_mantenimiento
          end if
      ! call mygraph%insert_grafo(s1,s2,distancia,imp_mantenimiento)
        !call mygraph%imprimirdatos()
       !!call mygraph%createConnection(s1,s2)
          end if
        end do
      end if
   end do
   
   call json%destroy()
end subroutine lector_rutas

subroutine lector_tecnico(file_t,sucursal,contra)
  type(json_file) :: json 
  type(json_value), pointer :: tecnicos,tecnico,attribute
  type(json_core) :: jsonc 
 ! character(:),allocatable ::contra,file_t
  character(len=20),intent(inout)::contra,file_t
  character(:), allocatable ::nombre,apellido,genero,direccion,dpiS,telefonoS
  integer,intent(in)::sucursal
  integer:: size,i
  integer*8::dpi,telefono
  logical :: found ,flag
  found= buscar(sucursal,contra)
  if (found) then 
    call json%initialize()
    call json%load(filename=file_t) 
    call json%info('', n_children=size) 
    call json%get_core(jsonc) 
    call json%get('', tecnicos, found) 

    do i = 1, size
      call jsonc%get_child(tecnicos, i, tecnico, found=found)
      call jsonc%get_child(tecnico, 'dpi', attribute, found=found)
      if(found) call jsonc%get(attribute, dpiS)
      print *, "dpi ",dpiS
    
      call jsonc%get_child(tecnico ,'nombre', attribute, found=found)
      if(found) call jsonc%get(attribute, nombre)
      print *, "nombre: ",nombre

      call jsonc%get_child(tecnico, 'apellido', attribute, found=found)
      if(found) call jsonc%get(attribute, apellido)
      print *, "apellido: ",nombre

      call jsonc%get_child(tecnico, 'direccion', attribute, found=found)
      if(found) call jsonc%get(attribute, direccion)
      print *, "direccion: ",direccion

      call jsonc%get_child(tecnico, 'telefono', attribute, found=found)
      if(found) call jsonc%get(attribute, telefonoS)
      print *, "telefono: ", telefonoS
      read(dpiS,"(I13)") dpi
      read(telefonoS,"(I13)") telefono

      call tablahash%insert(dpi,nombre,apellido,direccion,telefono)

    end do
    call arbol_avl%inserthash(sucursal  ,tablahash)

    call json%destroy()
  end if
  
end subroutine lector_tecnico


       

end program main