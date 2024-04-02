module avl_m
    use uuid_module
    use abb_m
    implicit none
    private

    type :: nodo
        integer :: valor
        type(abb):: abb_img
        integer :: altura = 1
        type(nodo), pointer :: derecha => null()
        type(nodo), pointer :: izquierda => null() 
    end type

    type, public :: avl
        type(nodo), pointer :: raiz_avl => null()
    
    contains
        procedure :: insert
        procedure :: delete_avl
        procedure :: preorden_avl
        procedure :: graficar_avl
    end type avl

contains
    subroutine insert(self, val, abb_img)
        class(avl), intent(inout) :: self
        type(abb),intent(in) :: abb_img
        integer, intent(in) :: val

        call insertRec_avl(self%raiz_avl, val,abb_img)
    end subroutine insert

    subroutine delete_avl(self, val)
        class(avl), intent(inout) :: self
        integer, intent(in) :: val

        self%raiz_avl => deleteRec_avl(self%raiz_avl, val)
    end subroutine delete_avl

    subroutine preorden_avl(self)
        class(avl), intent(in) :: self
        
        call preordenRec_avl(self%raiz_avl)
    end subroutine preorden_avl

    recursive subroutine insertRec_avl(raiz_avl, val,abb_img)
        type(nodo), pointer, intent(inout) :: raiz_avl
        type(abb),intent(in) :: abb_img
        integer, intent(in) :: val

        if(.not. associated(raiz_avl)) then
            allocate(raiz_avl)
            raiz_avl = nodo(valor=val)
        
        else if(val < raiz_avl%valor) then 
            call insertRec_avl(raiz_avl%izquierda, val,abb_img)

        else if(val > raiz_avl%valor) then
            call insertRec_avl(raiz_avl%derecha, val,abb_img)
        end if

        raiz_avl%altura = maximo(obtenerAltura(raiz_avl%izquierda), obtenerAltura(raiz_avl%derecha)) + 1

        if(obtenerBalance(raiz_avl) > 1) then
            if(obtenerBalance(raiz_avl%derecha) < 0) then
                raiz_avl%derecha => rotacionDerecha(raiz_avl%derecha)
                raiz_avl => rotacionIzquierda(raiz_avl)
            else
                raiz_avl => rotacionIzquierda(raiz_avl)
            end if
        end if

        if(obtenerBalance(raiz_avl) < -1) then
            if(obtenerBalance(raiz_avl%izquierda) > 0) then
                raiz_avl%izquierda => rotacionIzquierda(raiz_avl%izquierda)
                raiz_avl => rotacionDerecha(raiz_avl)

            else
                raiz_avl => rotacionDerecha(raiz_avl)
            end if
        end if
    end subroutine insertRec_avl

    recursive function deleteRec_avl(raiz_avl, val) result(res)
        type(nodo), pointer :: raiz_avl
        integer, intent(in) :: val

        type(nodo), pointer :: temp
        type(nodo), pointer :: res 
        
        if(.not. associated(raiz_avl)) then
            res => raiz_avl
            return
        end if

        if(val < raiz_avl%valor) then
            raiz_avl%izquierda => deleteRec_avl(raiz_avl%izquierda, val)
        
        else if(val > raiz_avl%valor) then
            raiz_avl%derecha => deleteRec_avl(raiz_avl%derecha, val)

        else
            if(.not. associated(raiz_avl%izquierda)) then
                temp => raiz_avl%derecha
                deallocate(raiz_avl)
                res => temp

            else if (.not. associated(raiz_avl%derecha)) then
                temp => raiz_avl%izquierda
                deallocate(raiz_avl)
                res => temp
            
            else
                call obtenerMayorDeMenores_avl(raiz_avl%izquierda, temp)
                raiz_avl%valor = temp%valor
                raiz_avl%izquierda => deleteRec_avl(raiz_avl%izquierda, temp%valor)
            end if
        end if

        res => raiz_avl
        if(.not. associated(raiz_avl)) return

        raiz_avl%altura = maximo(obtenerAltura(raiz_avl%izquierda), obtenerAltura(raiz_avl%derecha))

        if(obtenerBalance(raiz_avl) > 1) then
            if(obtenerBalance(raiz_avl%derecha) < 0) then
                raiz_avl%derecha => rotacionDerecha(raiz_avl%derecha)
                raiz_avl => rotacionIzquierda(raiz_avl)
            else
                raiz_avl => rotacionIzquierda(raiz_avl)
            end if
        end if

        if(obtenerBalance(raiz_avl) < -1) then
            if(obtenerBalance(raiz_avl%izquierda) > 0) then
                raiz_avl%izquierda => rotacionIzquierda(raiz_avl%izquierda)
                raiz_avl => rotacionDerecha(raiz_avl)

            else
                raiz_avl => rotacionDerecha(raiz_avl)
            end if
        end if

        res => raiz_avl
    end function deleteRec_avl

    function rotacionIzquierda(raiz_avl) result(raiz_avlDerecha)
        type(nodo), pointer, intent(in) :: raiz_avl
        type(nodo), pointer :: raiz_avlDerecha
        type(nodo), pointer :: temp

        raiz_avlDerecha => raiz_avl%derecha
        temp => raiz_avlDerecha%izquierda

        raiz_avlDerecha%izquierda => raiz_avl
        raiz_avl%derecha => temp

        raiz_avl%altura = maximo(obtenerAltura(raiz_avl%izquierda), obtenerAltura(raiz_avl%derecha)) + 1
        raiz_avlDerecha%altura = maximo(obtenerAltura(raiz_avlDerecha%izquierda), obtenerAltura(raiz_avlDerecha%derecha)) + 1
    end function rotacionIzquierda

    function rotacionDerecha(raiz_avl) result(raiz_avlIzquierda)
        type(nodo), pointer, intent(in) :: raiz_avl
        type(nodo), pointer :: raiz_avlIzquierda
        type(nodo), pointer :: temp

        raiz_avlIzquierda => raiz_avl%izquierda
        temp => raiz_avlIzquierda%derecha

        raiz_avlIzquierda%derecha => raiz_avl
        raiz_avl%izquierda => temp

        raiz_avl%altura = maximo(obtenerAltura(raiz_avl%izquierda), obtenerAltura(raiz_avl%derecha)) + 1
        raiz_avlIzquierda%altura = maximo(obtenerAltura(raiz_avlIzquierda%izquierda), obtenerAltura(raiz_avlIzquierda%derecha)) + 1
    end function rotacionDerecha

    recursive subroutine obtenerMayorDeMenores_avl(raiz_avl, mayor)
        type(nodo), pointer :: raiz_avl, mayor
        if(associated(raiz_avl%derecha)) then
            call obtenerMayorDeMenores_avl(raiz_avl%derecha, mayor)
        else
            mayor => raiz_avl
        end if
    end subroutine obtenerMayorDeMenores_avl

    recursive subroutine preordenRec_avl(raiz_avl)
        type(nodo), pointer, intent(in) :: raiz_avl

        if(associated(raiz_avl)) then
            print *, raiz_avl%valor
            call preordenRec_avl(raiz_avl%izquierda)
            call preordenRec_avl(raiz_avl%derecha)
        end if
    end subroutine preordenRec_avl

    function maximo(izquierda, derecha) result(res)
        integer, intent(in) :: izquierda
        integer, intent(in) :: derecha

        integer :: res
        res = derecha

        if(izquierda >= derecha) then
            res = izquierda
            return
        end if
    end function maximo

    function obtenerBalance(raiz_avl) result(res)
        type(nodo), pointer, intent(in) :: raiz_avl
        integer :: res
        
        res = obtenerAltura(raiz_avl%derecha) - obtenerAltura(raiz_avl%izquierda)
    end function

    function obtenerAltura(n) result(res)
        type(nodo), pointer :: n
        integer :: res
        res = 0

        if(.not. associated(n)) return
        res = n%altura
    end function obtenerAltura

    recursive subroutine imprimirRec(raiz_avl, nombre, io)
        type(nodo), pointer, intent(in) :: raiz_avl
        character(len=36), intent(in) :: nombre
        integer :: io

        character(len=36) :: derecha
        character(len=36) :: izquierda

        derecha = generate_uuid()
        izquierda = generate_uuid()

        if(associated(raiz_avl)) then
            !"Nodo_uuid"[Label="1"]
            write(io, *) '"Nodo'//nombre//'"[label= "', raiz_avl%valor, '"]'

            if(associated(raiz_avl%izquierda)) then
                !"Nodo_uuid"->"Nodo_uuidHijoIzquierdo"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//izquierda//'"'
            end if

            if(associated(raiz_avl%derecha)) then
                !"Nodo_uuid"->"Nodo_uuidHijoDerecho"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//derecha//'"'
            end if
            call imprimirRec(raiz_avl%izquierda, izquierda, io)
            call imprimirRec(raiz_avl%derecha, derecha, io)
        end if
    end subroutine imprimirRec

    subroutine graficar_avl(self)
        class(avl), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./avl_tree.dot")
        comando = "dot -Tpng ./avl_tree.dot -o ./avl_tree.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%raiz_avl)) then
            call imprimirRec(self%raiz_avl, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficar_avl
end module avl_m