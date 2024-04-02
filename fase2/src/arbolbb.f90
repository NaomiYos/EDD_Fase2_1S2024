module abb_m
    use uuid_module
    use matrix_m
    implicit none
    type :: nodoabb
        integer :: valor
        type(matrix_t):: matriz
        type(nodoabb), pointer :: derecha => null()
        type(nodoabb), pointer :: izquierda => null() 
        end type nodoabb
    type, public :: abb
        type(nodoabb), pointer :: raiz => null()
   
    contains
        procedure :: iniciar_m
        procedure :: graficar_m
        procedure :: insertbb
        procedure :: delete
        procedure :: preorden
        procedure :: graficar
    end type abb
    
contains
    subroutine insertbb(self, val,matriz)
        
        class(abb), intent(inout) :: self
        type(matrix_t),intent(in) :: matriz
        integer, intent(in) :: val
        if(.not. associated(self%raiz)) then
            allocate(self%raiz)
            self%raiz%valor=val
            self%raiz%matriz=matriz
        else
            call insertRec(self%raiz, val,matriz)
        end if
    end subroutine insertbb

    subroutine iniciar_m(self)
        class(abb), intent(inout) :: self
        call self%raiz%matriz%init()
    end subroutine iniciar_m

    function buscar_m(id) result(mbuscar)
        type(matrix_t),pointer :: mbuscar
        integer, intent(in) :: id
        type(nodoabb), pointer ::raiz
        mbuscar=>buscar(raiz,id)
        
    end function buscar_m

    subroutine graficar_m(self,id)
        class(abb), intent(inout) :: self
        integer, intent(in) :: id
        print *, "funcion graf"
        call graficar_mRec(self%raiz,id)
        
    end subroutine graficar_m

    recursive subroutine graficar_mRec(raiz2,id) 
    integer, intent(in) :: id
    type(nodoabb), pointer, intent(in) :: raiz2
        print *, "no nula"
        if(id < raiz2%valor) then
            call graficar_mRec(raiz2%izquierda,id)
            print *, "funcion iz"
        else if(id > raiz2%valor) then
            call graficar_mRec(raiz2%derecha,id)
            print *, "funcion der"
        else
            print *,"else graf", raiz2%valor
                !call raiz2%matriz%init()
                call raiz2%matriz%create_dot()

                
         end if
      
 


    end  subroutine graficar_mRec

    recursive function buscar(raiz,id) result(mresult)
        integer, intent(in) :: id
        type(matrix_t),pointer :: mresult
        type(nodoabb), pointer, intent(in) :: raiz

            if(id < raiz%valor) then
                mresult=> buscar(raiz%izquierda,id)
                return
            
            else if(id > raiz%valor) then
                mresult=> buscar(raiz%derecha,id)
                return
            else
                mresult=> raiz%matriz
                return
                
            end if
      

        end  function buscar

    subroutine delete(self, val)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val

        self%raiz => deleteRec(self%raiz, val)
    end subroutine delete

    subroutine preorden(self)
        class(abb), intent(in) :: self
        
        call preordenRec(self%raiz)
    end subroutine preorden

    subroutine graficar(self)
        class(abb), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./abb_tree.dot")
        comando = "dot -Tpng ./abb_tree.dot -o ./abb_tree.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%raiz)) then
            call imprimirRec_abb(self%raiz, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficar

    recursive subroutine insertRec(raiz, val,mtc)
        type(nodoabb), pointer, intent(inout) :: raiz
        type(matrix_t),intent(in) :: mtc

        integer, intent(in) :: val
       
            
        
         if(val < raiz%valor) then 
            if(.not. associated(raiz%izquierda)) then
                allocate(raiz%izquierda)
                raiz%izquierda%valor=val
                raiz%izquierda%matriz=mtc
            else
                call insertRec(raiz%izquierda, val, mtc)
            end if

        else if(val > raiz%valor) then
            if(.not. associated(raiz%derecha)) then
                allocate(raiz%derecha)
                raiz%derecha%valor=val
                raiz%derecha%matriz=mtc
            else
            call insertRec(raiz%derecha, val,mtc)
            end if
        end if
    end subroutine insertRec

    recursive function deleteRec(raiz, val) result(res)
        type(nodoabb), pointer :: raiz
        integer, intent(in) :: val

        type(nodoabb), pointer :: temp
        type(nodoabb), pointer :: res 
        
        if(.not. associated(raiz)) then
            res => raiz
            return
        end if

        if(val < raiz%valor) then
            raiz%izquierda => deleteRec(raiz%izquierda, val)
        
        else if(val > raiz%valor) then
            raiz%derecha => deleteRec(raiz%derecha, val)

        else
            if(.not. associated(raiz%izquierda)) then
                temp => raiz%derecha
                deallocate(raiz)
                res => temp
                return

            else if (.not. associated(raiz%derecha)) then
                temp => raiz%izquierda
                deallocate(raiz)
                res => temp
                return
            
            else
                call obtenerMayorDeMenores(raiz%izquierda, temp)
                raiz%valor = temp%valor
                raiz%izquierda => deleteRec(raiz%izquierda, temp%valor)
            end if
        end if

        res => raiz
    end function deleteRec

    recursive subroutine obtenerMayorDeMenores(raiz, mayor)
        type(nodoabb), pointer :: raiz, mayor
        if(associated(raiz%derecha)) then
            call obtenerMayorDeMenores(raiz%derecha, mayor)
        else
            mayor => raiz
        end if
    end subroutine obtenerMayorDeMenores

    recursive subroutine preordenRec(raiz)
        type(nodoabb), pointer, intent(in) :: raiz

        if(associated(raiz)) then
            print *, raiz%valor
            call preordenRec(raiz%izquierda)
            call preordenRec(raiz%derecha)
        end if
    end subroutine preordenRec

    recursive subroutine imprimirRec_abb(raiz, nombre, io)
        type(nodoabb), pointer, intent(in) :: raiz
        character(len=36), intent(in) :: nombre
        integer :: io

        character(len=36) :: derecha
        character(len=36) :: izquierda

        derecha = generate_uuid()
        izquierda = generate_uuid()

        if(associated(raiz)) then
            !"nodoabb_uuid"[Label="1"]
            write(io, *) '"nodoabb'//nombre//'"[label= "', raiz%valor, '"]'

            if(associated(raiz%izquierda)) then
                !"nodoabb_uuid"->"nodoabb_uuidHijoIzquierdo"
                write(io, *) '"nodoabb'//nombre//'"->"nodoabb'//izquierda//'"'
            end if

            if(associated(raiz%derecha)) then
                !"nodoabb_uuid"->"nodoabb_uuidHijoDerecho"
                write(io, *) '"nodoabb'//nombre//'"->"nodoabb'//derecha//'"'
            end if
            call imprimirRec_abb(raiz%izquierda, izquierda, io)
            call imprimirRec_abb(raiz%derecha, derecha, io)
        end if
    end subroutine imprimirRec_abb
end module abb_m