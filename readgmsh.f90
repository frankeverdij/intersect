      subroutine readgmshconst(filename,ne,nn,etype)
!
!     reads # of elements,nodes, type of mesh from a msh file
!
      integer, intent(inout) :: ne,nn,etype
      integer :: b,c,d,e
      character(len=80) :: a
      character(len=*), intent(in) :: filename
            
      open(unit=90,file=filename, status='old')
 20   read(90,FMT='(a80)') a
      if (a(:6) /= '$Nodes') goto 20
      read (90, FMT = '(I7)') nn

 30   read(90,FMT='(a80)') a
      if (a(:9) /= '$Elements') goto 30
      read (90, FMT = '(I7)') ne
      
      read(90,*) b, c
      if ((c>0) .and. (c<4)) then
        etype=c+1
      else
        etype=0
      endif      
      close(90)
      
      end !readgmshconst


      subroutine readgmsh(filename,ne,nn,etype,elem,node)
!
!     reads mesh elements and nodes from a msh file
!      
      character(len=*), intent(in) :: filename
      integer, intent(inout) :: ne,nn,etype
      integer, dimension(etype*ne), intent(inout) :: elem
      real(8), dimension(nn,2), intent(inout) :: node

      integer :: i,j,k,b,c,d
      integer, allocatable :: dummy(:)
      character(len=80) :: a

      
      open(unit=90,file=filename, status='old')

 20   read(90,FMT='(a80)') a
      if (a(:6) /= '$Nodes') goto 20
      read(90, FMT = '(I7)') nn

 30   read(90,FMT='(a80)') a
      if (a(:9) /= '$Elements') goto 30
      read(90, FMT = '(I7)') ne
      read(90,*) b, c, d
      if ((c>0) .and. (c<4)) then
        etype=c+1
      else
        etype=0
      endif      
      allocate(dummy(d))
      rewind(90)
      
 40   read(90,FMT='(a80)') a
      if (a(:6) /= '$Nodes') goto 40
      read(90,FMT='(a80)') a
      do i=1,nn
        read (90,*) b, node(i,1:2)
      enddo

 50   read(90,FMT='(a80)') a
      if (a(:9) /= '$Elements') goto 50
      read(90,FMT='(a80)') a
      do i=1,ne
        read (90,*) b,c,e,(dummy(k),k=1,d),(elem(j+(i-1)*etype),j=1,etype)
      enddo
      deallocate(dummy)
      close(90)
      
      end !readgmsh
 
