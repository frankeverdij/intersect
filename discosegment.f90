subroutine creatediscosegment(ned,elemdisco,nbs,segment)

implicit none

integer, intent(in) :: ned
integer, dimension(ned), intent(in) :: elemdisco

integer, intent(inout) :: nbs
integer, dimension(ned,2), intent(inout) :: segment
!---
integer :: i,a,b

nbs=0
do i=1,ned/2
  nbs=nbs+1
  a=elemdisco(2*i-1)
  b=elemdisco(2*i)
  if (a>b) then
    segment(nbs,1)=b
    segment(nbs,2)=a
  else
    segment(nbs,1)=a
    segment(nbs,2)=b    
  endif
enddo

end subroutine creatediscosegment
