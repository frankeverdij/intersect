subroutine createsegment(ne,etype,elem,nseg,segment)

implicit none

integer, intent(in) :: ne, etype
integer, dimension(etype*ne), intent(in) :: elem

integer, intent(inout) :: nseg
integer, dimension(etype*ne,2), intent(inout) :: segment
!---
integer :: i,j,k,a,b
integer, dimension(etype*ne,2) :: dummy

nseg=0
do i=1,ne
  do j=1,etype
    nseg=nseg+1
    a=elem(etype*(i-1)+j)
    b=elem(etype*(i-1)+mod(j,etype)+1)
    if (a>b) then
      segment(nseg,1)=b
      segment(nseg,2)=a
    else
      segment(nseg,1)=a
      segment(nseg,2)=b    
    endif
    dummy(nseg,1)=i
  enddo
enddo

call heapsort2(segment(:,1),segment(:,2),dummy(:,1),nseg)

dummy(:,2)=0
k=1
! remove doubles
do i=2,nseg
  if ((segment(i,1)==segment(k,1)).AND.(segment(i,2)==segment(k,2))) then
    dummy(k,2)=dummy(i,1)
    if (dummy(k,1)>dummy(k,2)) then
      dummy(k,2)=dummy(k,1)
      dummy(k,1)=dummy(i,1)
    endif
  else
    k=k+1
    segment(k,:)=segment(i,:)
    dummy(k,:)=dummy(i,:)
  endif
enddo

nseg=k

end subroutine createsegment
