module pixelm
    implicit none
    private
    type :: Pixel
      integer :: fila,columna
      character(:), allocatable ::  color
    end type Pixel
    
end module pixelm
  