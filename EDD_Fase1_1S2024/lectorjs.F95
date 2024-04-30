program leer_json
    use json_module
    implicit none
    type(json_file) :: json
    character(len=:), allocatable :: file_content
    integer :: iostat
    logical :: found
    integer :: id,img_g,img_p
    character(len) :: nombre
    
  
    ! Abrir el archivo json
    open(newunit=10, file='datos.json', action='read', status='old', iostat=iostat)
    if (iostat /= 0) then
      print *, 'Error al abrir el archivo'
      stop
    end if
  
    ! Leer el contenido del archivo
    allocate(character(len=1024) :: file_content)
    read(10, '(a)', iostat=iostat) file_content
    if (iostat /= 0) then
      print *, 'Error al leer el archivo'
      stop
    end if
    close(10)
  
    ! Crear un objeto json a partir del contenido del archivo
    call json%initialize()
    call json%parse(file_content)
    deallocate(file_content)
  
    ! Buscar y mostrar los valores de x, y, z en el objeto json
    call json%get('x', x, found)
    if (found) then
      print *, 'x = ', x
    else
      print *, 'x no encontrado'
    end if
  
    call json%get('y', y, found)
    if (found) then
      print *, 'y = ', y
    else
      print *, 'y no encontrado'
    end if
  
    call json%get('z', z, found)
    if (found) then
      print *, 'z = ', z
    else
      print *, 'z no encontrado'
    end if
  
    ! Finalizar el objeto json
    call json%destroy()
  end program leer_json