      real(4) function seconds(time)
        
      implicit none

      real(4), intent(in) :: time
      
      ! Local Variables
      real(4) etime,elapsed(2)
!
! ----------------------------------------------------------------------
!      
      seconds = etime(elapsed) - time
 
      end function seconds
