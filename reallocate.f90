module mod_reallocate

implicit none
interface reallocate
  module procedure reallocate_im,reallocate_rm
end interface

contains

function reallocate_im(p,n,m)

integer, dimension(:,:), pointer :: p, reallocate_im
integer, intent(in) :: n,m
integer :: i,j,ierr
allocate(reallocate_im(n,m),stat=ierr)
if (ierr /= 0) then
  write(*,*) 'reallocate: problem in attempt to allocate memory'
  stop
endif
if (.not. associated(p)) return
do i=1,min(size(p,2),m)
  do j=1,min(size(p,1),n)
    reallocate_im(j,i)=p(j,i)
  enddo
enddo
deallocate(p)

end function reallocate_im


function reallocate_rm(p,n,m)

real(8), dimension(:,:), pointer :: p, reallocate_rm
integer, intent(in) :: n,m
integer :: i,j,ierr
allocate(reallocate_rm(n,m),stat=ierr)
if (ierr /= 0) then
  write(*,*) 'reallocate: problem in attempt to allocate memory'
  stop
endif
if (.not. associated(p)) return
do i=1,min(size(p,2),m)
  do j=1,min(size(p,1),n)
    reallocate_rm(j,i)=p(j,i)
  enddo
enddo
deallocate(p)

end function reallocate_rm

end module mod_reallocate

