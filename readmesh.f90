      subroutine readmeshvtuconst(ne,nn,etype)
!
!     reads # of elements,nodes, type of mesh from a vtu file
!
      integer, intent(inout) :: ne,nn,etype
      integer :: b,c,d,e
      character(len=80) :: a
            
      open(unit=90,file='mesh.vtu', status='old')
 20   read(90,FMT='(a80)') a
      if (a(:7) /= ' <Piece ') goto 20
      read (a(25:30), FMT = '(I6)') nn
      read (a(49:55), FMT = '(I7)') ne
      
 30   read(90,*) a
      if (a(:7) /= '<Cells> ') goto 30
      read(90,*) a
      read(90,*) b,c,d,e
      if (d==e) then
        etype=3
      else
        etype=4
      endif
      
      close(90)
      
      end !readmeshvtuconst


      subroutine readmeshvtu(ne,nn,etype,elem,node)
!
!     reads mesh elements and nodes from a vtu file
!      
      integer, intent(in) :: ne,nn,etype
      integer :: i,j

      integer, dimension(etype*ne), intent(inout) :: elem
      real(8), dimension(nn,2), intent(inout) :: node

      character(len=80) :: a
      
      open(unit=90,file='mesh.vtu', status='old')
      
 40   read(90,FMT='(a80)') a
      if (a(:8) /= '<Points>') goto 40
      read (90,*) a
      do i=1,nn
        read (90,*) node(i,1:2)
      enddo

 50   read(90,FMT='(a80)') a
      if (a(:7) /= '<Cells>') goto 50
      read (90,*) a
      do i=1,ne
        read (90,*) (elem(j+(i-1)*etype),j=1,etype)
	do j=1,etype
	  elem(j+(i-1)*etype)=elem(j+(i-1)*etype)+1
	enddo
      enddo
      close(90)
      
      end !readmshvtu
 
