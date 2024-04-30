module Avl_Tree
    !use abb_m
    use uuid_module
    use hash_table_m
    implicit none
  
    ! Cons
    integer, parameter :: LEFT_HEAVY = -1
    integer, parameter :: BALANCED = 0
    integer, parameter :: RIGHT_HEAVY = +1
  
    type Node_t
        integer :: id,Factor
        character(:), allocatable ::dept,direccion,password
       type(HashTable):: tablahash
        type(Node_t), pointer :: Left => null()
        type(Node_t), pointer :: Right => null()
    end type Node_t
  
    type Tree_t
        type(Node_t), pointer :: root => null()
        contains
        procedure :: newTree
        procedure :: insert
        procedure :: graficaravl
        procedure :: buscar
        procedure:: inserthash

    end type Tree_t
  
    contains
  
    function NewNode(id,dept,dir,contra) result(nodePtr)
      type(Node_t), pointer :: nodePtr
      integer, intent(in) :: id
      character(:), allocatable,intent(in) ::dept,dir,contra
      allocate(nodePtr)
      nodePtr%id = id
      nodePtr%dept = dept
      nodePtr%direccion = dir
      nodePtr%password = contra
      nodePtr%Factor = 0
      nodePtr%Left => null()
      nodePtr%Right => null()
    end function NewNode
  
    subroutine newTree(self)
      class(Tree_t), intent(inout) :: self
      self%root => null()
    end subroutine newTree
  
    function rotationII(n, n1) result(result_node)
        type(Node_t), pointer :: n, n1, result_node
        
        n%Left => n1%Right
        n1%Right => n
        if (n1%Factor == -1) then
            n%Factor = 0
            n1%Factor = 0
        else
            n%Factor = -1
            n1%Factor = 1
        end if
        result_node => n1
    end function rotationII
  
    function rotationDD(n, n1) result(result_node)
        type(Node_t), pointer :: n, n1, result_node
  
        n%Right => n1%Left
        n1%Left => n
        if (n1%Factor == 1) then
            n%Factor = 0
            n1%Factor = 0
        else
            n%Factor = 1
            n1%Factor = -1
        end if
        result_node => n1
    end function rotationDD
  
    function rotationDI(n, n1) result(result_node)
      type(Node_t), pointer :: n, n1, result_node, n2
  
      n2 => n1%Left
      n%Right => n2%Left
      n2%Left => n
      n1%Left => n2%Right
      n2%Right => n1
      if (n2%Factor == 1) then
          n%Factor = -1
      else
          n%Factor = 0
      end if
      if (n2%Factor == -1) then
          n1%Factor = 1
      else
          n1%Factor = 0
      end if
      n2%Factor = 0
      result_node => n2
    end function rotationDI
  
    function rotationID(n, n1) result(result_node)
        type(Node_t), pointer :: n, n1, result_node, n2
        n2 => n1%Right
        n%Left => n2%Right
        n2%Right => n
        n1%Right => n2%Left
        n2%Left => n1
        if (n2%Factor == 1) then
            n1%Factor = -1
        else
            n1%Factor = 0
        end if
        if (n2%Factor == -1) then
            n%Factor = 1
        else
            n%Factor = 0
        end if
        n2%Factor = 0
        result_node => n2
    end function rotationID



recursive function insert2(root, id,dir,dept,contra, increase) result(result_node)
type(Node_t), pointer :: root, result_node, n1
logical :: increase
character(:), allocatable,intent(in) ::dept,dir,contra
integer, intent(in) :: id

if (.not. associated(root)) then
    allocate(result_node)
    root => NewNode(id,dir,dept,contra)
    increase = .true.
else if (id < root%id) then
    root%Left => insert2(root%Left, id,dir,dept,contra, increase)
    if (increase) then
      select case (root%Factor)
        case (RIGHT_HEAVY)
            root%Factor = 0
            increase = .false.
        case (BALANCED)
            root%Factor = -1
        case (LEFT_HEAVY)
            n1 => root%Left
            if (n1%Factor == -1) then
                root => rotationII(root, n1)
            else
                root => rotationID(root, n1)
            end if
            increase = .false.
      end select
    end if
else if (id > root%id) then
    root%Right => insert2(root%Right,id,dir,dept,contra, increase)
    if (increase) then
        select case (root%Factor)
        case (RIGHT_HEAVY)
            n1 => root%Right
            if (n1%Factor == 1) then
                root => rotationDD(root, n1)
            else
                root => rotationDI(root, n1)
            end if
            increase = .false.
        case (BALANCED)
            root%Factor = 1
        case (LEFT_HEAVY)
            root%Factor = 0
            increase = .false.
        end select
    end if
end if

result_node => root
end function insert2

subroutine insert(tree, id,dir,dept,contra)
  class(Tree_t), intent(inout) :: tree
  integer, intent(in) :: id
  logical :: increase
  character(:), allocatable,intent(in) ::dept,dir,contra
  increase = .false.
  tree%root => insert2(tree%root, id,dir,dept,contra, increase)
end subroutine insert

    recursive subroutine imprimirRec(root, nombre, io)
        type(Node_t), pointer, intent(in) :: root
        character(len=36), intent(in) :: nombre
        integer :: io

        character(len=36) :: Right
        character(len=36) :: Left

        Right = generate_uuid()
        Left = generate_uuid()

        if(associated(root)) then
            !"Node_t_uuid"[Label="1"]
            write(io, *) '"Node_t'//nombre//'"[label= "', root%id, '"]'

            if(associated(root%Left)) then
                !"Node_t_uuid"->"Node_t_uuidHijoIzquierdo"
                write(io, *) '"Node_t'//nombre//'"->"Node_t'//Left//'"'
            end if

            if(associated(root%Right)) then
                !"Node_t_uuid"->"Node_t_uuidHijoDerecho"
                write(io, *) '"Node_t'//nombre//'"->"Node_t'//Right//'"'
            end if
            call imprimirRec(root%Left, Left, io)
            call imprimirRec(root%Right, Right, io)
        end if
    end subroutine imprimirRec

    subroutine graficaravl(self)
        class(Tree_t), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./avl_tree.dot")
        comando = "dot -Tpng ./avl_tree.dot -o ./avl_tree.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%root)) then
            call imprimirRec(self%root, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficaravl

    function buscar(self,id,pass)result(flag)
        class(Tree_t), intent(in) :: self
        integer, intent(in) :: id
        character(:), allocatable,intent(in) ::pass
        logical :: flag
        flag=.false.
        print *, "ingresó a la búsqueda"
        flag=  busqueda(self%root,id,pass)
        
    end function buscar
    
    
    recursive function busqueda(raiz2,id,pass) result(flag)
        integer, intent(in) :: id
        character(:), allocatable,intent(in) ::pass
        logical :: flag
        type(Node_t), pointer, intent(in) :: raiz2

        flag=.false.
        
        
        print *, "ingresó a la búsqueda"
        if(id < raiz2%id) then
                flag= busqueda(raiz2%Left,id,pass)
                
            else if(id > raiz2%id) then
                flag= busqueda(raiz2%Right,id,pass)
            
            else
                if(pass==raiz2%password) then

                    flag=.true.
                    return
                end if    
        end if
    end  function  busqueda

    subroutine inserthash(self,id,tablah)
        class(Tree_t), intent(inout) :: self
        type(HashTable),intent(in)::tablah
        integer,intent(in)::id
        call inserthash_rec(self%root,id,tablah)
    
    end subroutine inserthash

    recursive subroutine inserthash_rec(raiz2,id,tablah)
    integer, intent(in) :: id
    type(Node_t), pointer, intent(in) :: raiz2
    type(HashTable), intent(in)::tablah

    
    
    
    if(id < raiz2%id) then
            call inserthash_rec(raiz2%Left,id,tablah)
            
        else if(id > raiz2%id) then
            call inserthash_rec(raiz2%Right,id,tablah)
        
        else
            raiz2%tablahash=tablah
            print *,"se agrego la tabla hash"
 
           
    end if
    end  subroutine inserthash_rec

   
  end module Avl_Tree