module routes
    implicit none
    type edge
        integer :: id
        integer :: weight
        integer :: parent_id
        integer :: printers
        type(edge), pointer :: next => null()
        type(edge), pointer :: prev => null()
    end type edge
    type edge_list
        type(edge), pointer :: head => null()
        type(edge), pointer :: tail => null()
    contains
        procedure :: agregar_ordenamiento_
        procedure :: agregar_ordenamiento__2 
        procedure :: extraer
        procedure :: vacio
        procedure :: hacer_conexion
        procedure :: hacer_conexion_2
        procedure :: add_peso
    end type edge_list
    type result
        integer :: id
        integer :: weight
        integer :: printers
        type(result), pointer :: next => null()
    end type result
    type result_list
        integer :: total_km, total_impr
        type(result), pointer :: head => null()
        type(result), pointer :: tail => null()
    contains
        procedure :: add_result
        procedure :: print
    end type result_list
    type node
        integer :: id 
        type(edge_list) :: neighbors
        type(node), pointer :: next => null()
    end type node
    type graph
        integer :: n_nodes
        type(node), pointer :: head => null()
    contains
        procedure :: insert_grafo
        procedure :: insert_grafo_2
        procedure :: add_node
        procedure :: add_edge
        procedure :: add_edge_2
        procedure :: get_node
        procedure :: print_graf
    end type graph
    type analyzer
        type(graph):: graph_data 
    contains
        procedure :: set_graph
        procedure :: obtener_rutacorta
        procedure :: obetener_rutalarga
    end type analyzer
contains
    ! Edge list methods
    subroutine agregar_ordenamiento_(this, id, weight, parent_id, sort_by_id, impresoras)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id, impresoras
        logical, intent(in) :: sort_by_id
        type(edge), pointer :: new_edge
        type(edge), pointer :: current
        type(edge), pointer :: previous
        allocate(new_edge)
        new_edge%id = id
        new_edge%weight = weight
        new_edge%parent_id = parent_id
        new_edge%printers = impresoras

        if (.not. associated(this%head)) then
            this%head => new_edge
            this%tail => new_edge
            return
        end if

        current => this%head
        previous => null()

        if (sort_by_id) then
            do while (associated(current))
                if (current%id > id) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        else
            do while (associated(current))
                if (current%weight > weight) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        end if

        if (.not. associated(previous)) then
            new_edge%next => this%head
            this%head%prev => new_edge
            this%head => new_edge
        else if (.not. associated(current)) then
            this%tail%next => new_edge
            new_edge%prev => this%tail
            this%tail => new_edge
        else
            previous%next => new_edge
            new_edge%prev => previous
            new_edge%next => current
            current%prev => new_edge
        end if
    end subroutine agregar_ordenamiento_

    subroutine agregar_ordenamiento__2(this, id, weight, parent_id, sort_by_id, printers)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: id, weight, parent_id, printers
        logical, intent(in) :: sort_by_id
        type(edge), pointer :: new_edge
        type(edge), pointer :: current
        type(edge), pointer :: previous
        allocate(new_edge)
        new_edge%id = id
        new_edge%weight = weight
        new_edge%parent_id = parent_id
        new_edge%printers = printers

        if (.not. associated(this%head)) then
            this%head => new_edge
            this%tail => new_edge
            return
        end if

        current => this%head
        previous => null()

        if (sort_by_id) then
            do while (associated(current))
                if (current%id > id) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        else
            do while (associated(current))
                if (current%weight < weight) then
                    exit
                end if
                previous => current
                current => current%next
            end do
        end if

        if (.not. associated(previous)) then
            new_edge%next => this%head
            this%head%prev => new_edge
            this%head => new_edge
        else if (.not. associated(current)) then
            this%tail%next => new_edge
            new_edge%prev => this%tail
            this%tail => new_edge
        else
            previous%next => new_edge
            new_edge%prev => previous
            new_edge%next => current
            current%prev => new_edge
        end if
    end subroutine agregar_ordenamiento__2

    function extraer(this) result(edge_res)
        class(edge_list), intent(inout) :: this
        type(edge), pointer :: edge_res
        if (.not. associated(this%head)) then
            edge_res => null()
            return
        end if
        edge_res => this%head
        this%head => this%head%next
        if (associated(this%head)) then
            this%head%prev => null()
        else
            this%tail => null()
        end if
    end function extraer
    function vacio(this) result(res)
        class(edge_list), intent(in) :: this
        logical :: res
        res = .not. associated(this%head)
    end function vacio
    subroutine hacer_conexion(this,  to_hacer_conexion)
        class(edge_list), intent(inout) :: this
        class(edge_list), intent(in) :: to_hacer_conexion
        type(edge), pointer :: current

        current => to_hacer_conexion%head
        do while (associated(current))
            call this%agregar_ordenamiento_(current%id, current%weight, current%parent_id, .FALSE., current%printers)
            current => current%next
        end do
        
    end subroutine hacer_conexion


    subroutine hacer_conexion_2(this,  to_hacer_conexion)
        class(edge_list), intent(inout) :: this
        class(edge_list), intent(in) :: to_hacer_conexion
        type(edge), pointer :: current

        current => to_hacer_conexion%head
        do while (associated(current))
            call this%agregar_ordenamiento__2(current%id, current%weight, current%parent_id, .FALSE., current%printers)
            current => current%next
        end do
        
    end subroutine hacer_conexion_2

    subroutine add_peso(this, weight, printers)
        class(edge_list), intent(inout) :: this
        integer, intent(in) :: weight, printers
        type(edge), pointer :: current
        current => this%head
        do while (associated(current))
            current%weight = current%weight + weight
            current%printers = current%printers + printers
            current => current%next
        end do        
    end subroutine add_peso
    ! Result list methods
    subroutine add_result(this,  id, weight, printers)
        class(result_list), intent(inout) :: this
        integer, intent(in) :: id, weight, printers
        type(result), pointer :: new_result
        allocate(new_result)
        new_result%id = id
        new_result%weight = weight
        new_result%printers = printers
        if (.not. associated(this%head)) then
            this%head => new_result
            this%tail => new_result
            return
        end if
        this%tail%next => new_result
        this%tail => new_result  
        this%total_km = this%tail%weight  
        this%total_impr = this%tail%printers
    end subroutine add_result
    subroutine print(this)
        class(result_list), intent(in) :: this
        type(result), pointer :: current
        current => this%head
        do while (associated(current))
            write(*,'(A, I0, A, I0)') 'Node: ', current%id, ", Acumulated Weight: ", current%weight
            current => current%next
        end do
    end subroutine print
    ! Graph methods
    subroutine insert_grafo(this, id, neighbor_id, weight, impresoras)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight, impresoras   
        type(node), pointer :: current

        current => this%get_node(id)
        if ( .NOT. associated(current) ) then
            call this%add_node(id)
            call this%add_edge(neighbor_id, weight, this%head, impresoras)
        else
            call this%add_edge(neighbor_id, weight, current, impresoras)
        end if
    end subroutine insert_grafo

    subroutine insert_grafo_2(this, id, neighbor_id, weight, printers)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, neighbor_id, weight, printers
        type(node), pointer :: current

        current => this%get_node(id)
        if ( .NOT. associated(current) ) then
            call this%add_node(id)
            call this%add_edge_2(neighbor_id, weight, this%head, printers)
        else
            call this%add_edge_2(neighbor_id, weight, current, printers)
        end if
    end subroutine insert_grafo_2

    subroutine add_node(this,  id)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id
        type(node), pointer :: new_node

        allocate(new_node)
        new_node%id = id
        
        if (.not. associated(this%head)) then
            this%head => new_node
            return
        end if

        new_node%next => this%head
        this%head => new_node
    end subroutine add_node
    subroutine add_edge(this, id, weight, parent, impresoras)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, weight, impresoras
        type(node), pointer :: parent
        type(node), pointer :: edge_node 
        edge_node => this%get_node(id)
        if ( .NOT. associated(edge_node) ) then
            call this%add_node(id)
        end if
        call parent%neighbors%agregar_ordenamiento_(id, weight, parent%id, .TRUE., impresoras)
        this%n_nodes = this%n_nodes + 1
    end subroutine add_edge

    subroutine add_edge_2(this, id, weight, parent, printers)
        class(graph), intent(inout) :: this
        integer, intent(in) :: id, weight, printers
        type(node), pointer :: parent
        type(node), pointer :: edge_node 
        edge_node => this%get_node(id)
        if ( .NOT. associated(edge_node) ) then
            call this%add_node(id)
        end if
        call parent%neighbors%agregar_ordenamiento__2(id, weight, parent%id, .TRUE., printers)
        this%n_nodes = this%n_nodes + 1
    end subroutine add_edge_2

    function get_node(this, id) result(retval)
        class(graph), intent(in) :: this
        integer, intent(in) :: id
        type(node), pointer :: retval
        type(node), pointer :: current
        current => this%head
        do while (associated(current))
            if (current%id == id) then
                retval => current
                return
            end if
            current => current%next
        end do
        retval => null()  
    end function get_node
    subroutine print_graf(this)
        class(graph), intent(in) :: this
        type(node), pointer :: current
        type(edge), pointer :: current_edge
        current => this%head
        do while (associated(current))
            write(*,'(A, I0)') 'Nodo: ', current%id
            current_edge => current%neighbors%head
            do while (associated(current_edge))
                write(*,'(A, I0, A, I0, A)', advance='no') 'Edge: ', current_edge%id, ", " ,current_edge%weight, " "
                current_edge => current_edge%next
            end do
            write(*, *) ''
            current => current%next
        end do
    end subroutine print_graf
    ! Analyzer methods  
    subroutine set_graph(this,  graph_p)
        class(analyzer), intent(inout) :: this
        type(graph), intent(in) :: graph_p
        this%graph_data = graph_p
    end subroutine set_graph
    function obtener_rutacorta(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_impr
        type(result_list), pointer :: retval
        type(edge_list), pointer :: queue
        type(node), pointer :: current_node
        type(edge), pointer :: current_edge
        print *, 'Getting shortest path from ', id_origin, ' to ', id_destination
        sub_total = 0
        sub_total_impr = 0
        allocate(retval)
        retval%total_km = 0
        allocate(queue)
        current_node => this%graph_data%get_node(id_origin)
        if ( associated(current_node) ) then
            call queue%hacer_conexion(current_node%neighbors)
            call retval%add_result(id_origin, 0, 0)
        end if
        do while ( .NOT. queue%vacio() )
            current_edge => queue%extraer()
            sub_total = current_edge%weight
            sub_total_impr = current_edge%printers
            current_node => this%graph_data%get_node(current_edge%id)
            if ( .NOT. associated(current_node) ) then
                print *, 'Node not found'
                exit
            end if
            if (current_node%id == id_destination) then
                print *, 'Found destination'
                call retval%add_result(current_node%id, sub_total, sub_total_impr)
                exit
            end if
            call current_node%neighbors%add_peso(sub_total, sub_total_impr)
            call queue%hacer_conexion(current_node%neighbors)
            call retval%add_result(current_node%id, sub_total, sub_total_impr)
            current_node => current_node%next
        end do
    end function obtener_rutacorta

    function obetener_rutalarga(this, id_origin, id_destination) result(retval)
        class(analyzer), intent(in) :: this
        integer, intent(in) :: id_origin, id_destination
        integer :: sub_total, sub_total_impr
        type(result_list), pointer :: retval
        type(edge_list), pointer :: queue
        type(node), pointer :: current_node
        type(edge), pointer :: current_edge
        integer, allocatable :: max_weights(:)
        logical, allocatable :: visited(:)
    
        allocate(retval)
        retval%total_km = 0
        retval%total_impr = 0
        allocate(queue)
        allocate(max_weights(this%graph_data%n_nodes))
        allocate(visited(this%graph_data%n_nodes))
        max_weights = -HUGE(0)  ! Inicializar pesos máximos con el menor valor posible
        visited = .false.       ! Inicializar todos los nodos como no visitados
    
        current_node => this%graph_data%get_node(id_origin)
        if ( associated(current_node) ) then
            call queue%hacer_conexion_2(current_node%neighbors)
            call retval%add_result(id_origin, 0, 0)
            max_weights(current_node%id) = 0
        end if
    
        do while ( .NOT. queue%vacio() )
            current_edge => queue%extraer()
            sub_total = current_edge%weight + max_weights(current_edge%parent_id)
            sub_total_impr = current_edge%printers
            if (sub_total > max_weights(current_edge%id)) then
                max_weights(current_edge%id) = sub_total
                current_node => this%graph_data%get_node(current_edge%id)
                if ( .NOT. associated(current_node) ) then
                    print *, 'Node not found'
                    exit
                end if
                if (current_node%id == id_destination) then
                    print *, 'Found destination'
                    call retval%add_result(current_node%id, sub_total, sub_total_impr)
                    exit
                end if
                if (.not. visited(current_node%id)) then
                    call current_node%neighbors%add_peso(sub_total - max_weights(current_node%id), sub_total_impr)
                    call queue%hacer_conexion_2(current_node%neighbors)
                    call retval%add_result(current_node%id, sub_total, sub_total_impr)
                    visited(current_node%id) = .true.
                end if
            end if
        end do
    end function obetener_rutalarga
    subroutine graficar(this, filename)
        class(graph), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(node), pointer :: current_node
        type(edge), pointer :: current_edge
        integer :: io_unit, io_stat
        
        open(newunit=io_unit, file=filename, status='replace', action='write', iostat=io_stat)
        if (io_stat /= 0) then
            print *, 'Error al abrir el archivo'
            return
        end if
        
        write(io_unit, *) 'digraph G {'
        write(io_unit, *) '    rankdir=LR;'  ! Esto coloca el gráfico de izquierda a derecha
        write(io_unit, *) '    node [shape=circle];'  ! Estilo de los nodos
        
        current_node => this%head
        do while (associated(current_node))
            current_edge => current_node%neighbors%head
            do while (associated(current_edge))
                if (current_node%id /= current_edge%id) then
                    write(io_unit, '(A, I0, A, I0, A, I0, A)') '    ', current_node%id, ' -> ', &
                    current_edge%id, ' [label="', current_edge%weight, '"];'
                end if
                current_edge => current_edge%next
            end do
            current_node => current_node%next
        end do
        
        write(io_unit, *) '}'
        close(io_unit)
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')     
        print *,"Arbol de Imagenes con Capas graficado con exito"   
        call system('start ' // trim(adjustl(filename)) // '.png')
    end subroutine graficar
    

end module routes