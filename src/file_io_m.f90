    module file_io_m ! abstract interface for file 
        implicit none
        type, abstract :: file_t
        contains  
            procedure(p_open ), deferred :: open_file
            procedure(p_close), deferred :: close_file
        end type file_t   
      
        abstract interface
            subroutine p_open(this, fn)
                import 
                class(file_t), intent(in out) :: this
                character(len = *), intent(in) :: fn
            end subroutine p_open
      
            subroutine p_close(this)
                import
                class(file_t), intent(in) :: this
            end subroutine p_close
        end interface
    end module file_io_m