module arbolcliente
type cliente
    private
    character(:), allocatable :: nombre,contra,dpi
   ! type(ventanilla), pointer :: next => null()
    
    !type(pila) :: mipila = pila() ! Simple list
    end type cliente
end module arbolcliente