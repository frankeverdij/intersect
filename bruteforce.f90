module mod_bruteforce

use mod_reallocate, only: reallocate

contains

subroutine bruteforce(nn,nseg,rednode,redsegment,bluenode,bluesegment,&
                      nin,intersections)

implicit none

integer, dimension(2), intent(in) :: nn, nseg
real(8), dimension(nn(1),2), intent(in) :: rednode
real(8), dimension(nn(2),2), intent(in) :: bluenode
integer, dimension(nseg(1),2), intent(in) :: redsegment
integer, dimension(nseg(2),2), intent(in) :: bluesegment

integer, intent(inout) :: nin
real(8), dimension(:,:), pointer :: intersections
!--
integer :: i,j,k,sect,nin_thread
real(8), dimension(2,2) :: node_red, cross
real(8), dimension(:,:), pointer :: intersections_thread
!$ integer :: OMP_get_thread_num

if (nin<10) nin=10
nin_thread=nin
nin=0
allocate(intersections(nin,2))

!$OMP PARALLEL DEFAULT(NONE) SHARED(nin,intersections) &
!$OMP FIRSTPRIVATE(nseg,rednode,bluenode,redsegment,bluesegment,nin_thread) &
!$OMP PRIVATE(i,j,sect,node_red,cross,k,intersections_thread)
allocate(intersections_thread(nin_thread,2))
k=0

!$OMP DO
do i=1,nseg(1)
  node_red(1,1:2)=rednode(redsegment(i,1),1:2)
  node_red(2,1:2)=rednode(redsegment(i,2),1:2)
  do j=1,nseg(2)
    call intersect(node_red(1,1:2),node_red(2,1:2),&
                   bluenode(bluesegment(j,1),1:2),&
                   bluenode(bluesegment(j,2),1:2),sect,cross)
    if (sect==2) then
      k=k+1
      intersections_thread(k,1:2)=cross(1,1:2)
      if (k==nin_thread) then
        nin_thread=nin_thread*1.34
!$      write (*,*) "thread",OMP_get_thread_num(),"resizing activated:"
        write(*,*) "resizing to ", nin_thread," intersections"
        intersections_thread => reallocate(intersections_thread,nin_thread,2)
        write(*,*) "end resize"
      endif
    endif
  enddo
enddo
!$OMP END DO

!$OMP CRITICAL
if (k>0) then
  intersections => reallocate(intersections,nin+k,2)
  do i=1,2
    do j=1,k
      intersections(nin+j,i)=intersections_thread(j,i)
    enddo
  enddo
  nin=nin+k
endif
!$OMP END CRITICAL

deallocate(intersections_thread)

!$OMP END PARALLEL

if (nin>0) call heapsort3(intersections(1:nin,1),&
                          intersections(1:nin,2),nin)

return

end subroutine bruteforce

end module mod_bruteforce
