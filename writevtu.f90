module mod_writevtu

contains

subroutine writepoints(filename,n,a)
!
!     writes points info to a VTU file
!
implicit none

integer, intent(in) :: n
real(8), dimension(:,:), pointer :: a
character(len=*), intent(in) :: filename

integer :: i,j

open(unit=92,file=filename, status='unknown')

write(92,fmt="(a)") '<?xml version="1.0"?>'
write(92,"(a)") '<VTKFile type="UnstructuredGrid" version="0.1"  byte_order="LittleEndian" compressor="vtkZLibDataCompressor">'
write(92,fmt="(a)") '<UnstructuredGrid>'
write(92,fmt="(a23, i6, a18, i7, a2)") '<Piece NumberOfPoints="',n,'"  NumberOfCells="',n,'">'
write(92,fmt="(a)") '<Points>'
write(92,fmt="(a)") '<DataArray type="Float32" NumberOfComponents="3" format="ascii">'
do i=1,n
  write(92,fmt="( f13.6, f13.6, f4.1)") a(i,1),a(i,2),0.0
enddo
write(92,fmt="(a)") '</DataArray>'
write(92,fmt="(a)") '</Points>'
write(92,fmt="(a)") '<Cells>'
write(92,fmt="(a)") '<DataArray type="Int32" Name="connectivity" format="ascii">'
do i=1,n
  write(92,fmt="( i7)") i
enddo
write(92,fmt="(a)") '</DataArray>'
write(92,fmt="(a)") '<DataArray type="Int32" Name="offsets" format="ascii">'
do i=1,n
  write(92,fmt="( i7)") i
enddo	   
write(92,fmt="(a)") '</DataArray>'
write(92,fmt="(a)") '<DataArray type="UInt8" Name="types" format="ascii">'
do i=1,n
  write(92,fmt="( i7)") 1
enddo
write(92,fmt="(a)") '</DataArray>'
write(92,fmt="(a)") '</Cells>'
write(92,fmt="(a)") '</Piece>'
write(92,fmt="(a)") '</UnstructuredGrid>'
write(92,fmt="(a)") '</VTKFile>'

close(92)

end subroutine writepoints

subroutine writemesh(filename,ne,nn,etype,elem,node)
!
!     writes mesh info to a VTU file
!
implicit none

integer, intent(in) :: nn,ne,etype
integer, dimension(etype*ne), intent(in) :: elem
real(8), dimension(nn,2), intent(in) :: node
character(len=*), intent(in) :: filename

integer :: i,j

open(unit=92,file=filename, status='unknown')

write(92,fmt="(a)") '<?xml version="1.0"?>'
write(92,"(a)") '<VTKFile type="UnstructuredGrid" version="0.1"  byte_order="LittleEndian" compressor="vtkZLibDataCompressor">'
write(92,fmt="(a)") '<UnstructuredGrid>'
write(92,fmt="(a23, i6, a18, i7, a2)") '<Piece NumberOfPoints="',nn,'"  NumberOfCells="',ne,'">'
write(92,fmt="(a)") '<Points>'
write(92,fmt="(a)") '<DataArray type="Float32" NumberOfComponents="3" format="ascii">'
do i=1,nn
  write(92,fmt="( f13.6, f13.6, f4.1)") node(i,1),node(i,2),0.0
enddo
write(92,fmt="(a)") '</DataArray>'
write(92,fmt="(a)") '</Points>'
write(92,fmt="(a)") '<Cells>'
write(92,fmt="(a)") '<DataArray type="Int32" Name="connectivity" format="ascii">'
do i=1,ne
  write(92,fmt="( i7 i7 i7)") (elem(etype*(i-1)+j)-1,j=1,3)
enddo
write(92,fmt="(a)") '</DataArray>'
write(92,fmt="(a)") '<DataArray type="Int32" Name="offsets" format="ascii">'
do i=1,ne
  write(92,fmt="( i7)") etype*i
enddo	   
write(92,fmt="(a)") '</DataArray>'
write(92,fmt="(a)") '<DataArray type="UInt8" Name="types" format="ascii">'

if (etype==2) then
  j=3
elseif (etype==3) then
  j=5
elseif (etype==4) then
  j=9
else
  j=0
endif
  
do i=1,ne
  write(92,fmt="( i7)") j
enddo
write(92,fmt="(a)") '</DataArray>'
write(92,fmt="(a)") '</Cells>'
write(92,fmt="(a)") '</Piece>'
write(92,fmt="(a)") '</UnstructuredGrid>'
write(92,fmt="(a)") '</VTKFile>'

close(92)

end subroutine writemesh


subroutine writesect(sect,cross)

implicit none

real(8) :: cross(2,2)
integer :: sect

if (sect==0) write (*,*) "Lines are not intersecting"
if (sect==1) write (*,*) "Lines are parallel"
if (sect==2) write (*,*) "Lines are intersecting at",cross(1,1),cross(1,2)
if (sect==3) write (*,*) "Lines are intersecting at segment",cross(1,1),cross(1,2),&
              cross(2,1),cross(2,2)

return
end subroutine writesect

end module mod_writevtu
