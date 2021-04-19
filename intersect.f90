! This routine calculates if there is an intersection point or segment
! between two linear segments, given by 4 x:y coordinates
! 
! The code was written by means of the Algorithm proposed in
! http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
! By Paul Bourke
!
! Basically you have four outcomes:
!
! 0) Segments do not cross, ergo no intersection
! 1) Segments are parallel, but not coinciding, ergo no intersection
! 2) Segments cross eachother, indicating an intersection point
! 3) Segments are parallel and coinciding, which has four separate cases:
!
!      a    b c     d
!      ------ -------   no intersection
!
!      a    b c     d
!      ------.-------   one intersection point if b=c
!
!      a   cb   d
!      ----==----       one intersection Segment(!) (c,b)
!
!      a c    d  b
!      ___________      one intersection Segment(!) (c,d)
!        ------
!
! Input: the four coordinates of the two segments: a and b define the first,
!        and c and d define the second segment.
!
! Output: sect, which is (0=no intersection), (1=parallel), (2=intersection point)
!          and (3=intersection segment)
!         cross(2,2) array:
!          if sect is 0 or 1 cross(2,2) equals 0.0d0
!          if sect is 2, cross(1,1:2) contains the intersection point
!          if sect is 3, cross(1,1:2) and cross(2,1:2) contains the intersection segment
!
! 20081114: Frank Everdij, Delft University of Technology
!

subroutine intersect(a,b,c,d,sect,cross)

implicit none

real(8),parameter :: epsilon = 1.0d-10
real(8),dimension(2),intent(in) :: a,b,c,d

integer,intent(out) :: sect
real(8),intent(out) :: cross(2,2)
!--
real(8),dimension(2) :: aa, bb, cc, dd, sw
real(8) :: denom, ua, ub
integer :: first, second

cross=0.0d0
sect=0

! Compute denominator and line numerators for the two segments
!
denom = ( d(2)-c(2) )*( b(1)-a(1) )-( d(1)-c(1) )*( b(2)-a(2) )
ua    = ( d(1)-c(1) )*( a(2)-c(2) )-( d(2)-c(2) )*( a(1)-c(1) )
ub    = ( b(1)-a(1) )*( a(2)-c(2) )-( b(2)-a(2) )*( a(1)-c(1) )


! If denom is zero, then the segments are Parallel (sect=1)
!
if (abs(denom) < epsilon) then

  sect=1

! If the numerators are both zero the segments coincide
!
  if ((abs(ua) < epsilon) .and. (abs(ub) < epsilon)) then
    ua=abs(b(1)-a(1))
    ub=abs(b(2)-a(2))
    
! Determine whether we will be looking at x or y coordinates to avoid
! pure vertical/horizontal degenerate cases
!
    if (ua>ub) then
      first=1
      second=2
    else
      first=2
      second=1
    endif
    aa=a
    bb=b
    cc=c
    dd=d
    
! Some sorting needed    
!
    if (aa(first)>bb(first)) then
      sw=aa
      aa=bb
      bb=sw
    endif
    if (cc(first)>dd(first)) then
      sw=cc
      cc=dd
      dd=sw
    endif
    if (aa(first)>cc(first)) then
      sw=aa
      aa=cc
      cc=sw
      sw=bb
      bb=dd
      dd=sw
    endif

! Decision time, handle each coinciding case separately
!
    if (bb(first)<cc(first)) then
      sect=0
    else if (abs(bb(first)-cc(first)) < epsilon) then
      sect=2
      cross(1,1:2)=bb(1:2)
    else if (dd(first)>bb(first)) then
      sect=3
      cross(1,1:2)=cc(1:2)
      cross(2,1:2)=bb(1:2)
    else
      sect=3
      cross(1,1:2)=cc(1:2)
      cross(2,1:2)=dd(1:2)
    endif
  end if

else

! The segments are not parallel/coinciding so compute the (virtual) intersection point
!
  ua=ua/denom
  ub=ub/denom
  
! Final check: if the segment numerators both lie in the interval [0...1] then
! the intersection point is real and we compute its coordinates.
!  
  if((ua >= 0.0d0) .and. (ua <= 1.0d0) .and. (ub >= 0.0d0) .and. (ub <= 1.0d0)) then
    sect=2
    cross(1,1:2)=a(1:2)+ua*(b(1:2)-a(1:2))
  endif
endif

return

end subroutine intersect
