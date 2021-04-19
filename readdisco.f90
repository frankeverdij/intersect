      subroutine readdiscovtuconst(ned,nnd,nofd)
!
!     reads # of elements, nodes and offsets from a discontinuity vtu file
!
      integer, intent(inout) :: ned,nnd,nofd
      character(len=80) :: a
            
      open(unit=90,file='discontinuity.vtu', status='old')
 60   read(90,FMT='(a80)') a
      if (a(:7) /= ' <Piece ') goto 60
      read (a(25:30), FMT = '(I6)') nnd
      read (a(49:55), FMT = '(I7)') nofd
      ned=nnd     
      
      close(90)
      
      end !readdiscovtuconst
      

      subroutine readdiscovtu(ned,nnd,nofd,elemdisco,nodedisco,offsetdisco)
!
!     reads discontinuous elements and nodes from a vtu file
!      
      integer, intent(in) :: ned,nnd,nofd
      integer :: i,j

      integer, dimension(ned), intent(inout) :: elemdisco
      integer, dimension(nofd), intent(inout) :: offsetdisco
      real(8), dimension(nnd,2), intent(inout) :: nodedisco

      character(len=80) :: a
      
      open(unit=90,file='discontinuity.vtu', status='old')
      
 40   read(90,FMT='(a80)') a
      if (a(:8) /= '<Points>') goto 40
      read (90,*) a
      do i=1,nnd
        read (90,*) nodedisco(i,1:2)
      enddo

 50   read(90,FMT='(a80)') a
      if (a(:7) /= '<Cells>') goto 50
      read (90,*) a
      read (90,*) (elemdisco(j),j=1,ned)
      read (90,*) a
      read (90,*) a
      read (90,*) (offsetdisco(j),j=1,nofd)
      do i=1,ned
        elemdisco(i)=elemdisco(i)+1
      enddo

      close(90)
      
      end !readdiscovtu
