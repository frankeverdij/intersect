! Small program to demonstrate brute-force segment-segment intersection
! Frank Everdij 20081124
!
program insect

use mod_reallocate
use mod_bruteforce
use mod_writevtu

implicit none

! index 1 = red; index 2 = blue

integer, dimension(2) :: ne, nn, etype, nseg
integer :: nofd
integer :: nin, i, j
real(4) :: starttime,stoptime,seconds

integer, dimension(:), allocatable :: redelem,blueelem
real(8), dimension(:,:), allocatable :: rednode,bluenode
integer, dimension(:,:), allocatable :: redsegment,bluesegment
real(8), dimension(:,:), pointer  :: intersections

nin=10000
nullify(intersections)

! Start with reading the mesh and discontinuities

! Read the Red mesh
call readgmshconst('red.msh',ne(1),nn(1),etype(1))
allocate(redelem(etype(1)*ne(1)),rednode(nn(1),2))
call readgmsh('red.msh',ne(1),nn(1),etype(1),redelem,rednode)

write (*,*) "red mesh"
write (*,*) " nodes ",nn(1)," elements ",ne(1)," type ",etype(1)
call writemesh('red.vtu',ne(1),nn(1),etype(1),redelem,rednode)

! Read the Blue mesh
call readgmshconst('blue.msh',ne(2),nn(2),etype(2))
allocate(blueelem(etype(2)*ne(2)),bluenode(nn(2),2))
call readgmsh('blue.msh',ne(2),nn(2),etype(2),blueelem,bluenode)

write (*,*) "blue mesh"
write (*,*) " nodes ",nn(2)," elements ",ne(2)," type ",etype(2)
call writemesh('blue.vtu',ne(2),nn(2),etype(2),blueelem,bluenode)


! build "red" and "blue" segment lists from mesh and discontinuity data

allocate(redsegment(etype(1)*ne(1),2))
call createsegment(ne(1),etype(1),redelem,nseg(1),redsegment)

write (*,*) "red segments ",nseg(1)

allocate(bluesegment(etype(2)*ne(2),2))
call createsegment(ne(2),etype(2),blueelem,nseg(2),bluesegment)

write (*,*) "blue segments ",nseg(2)
write (*,*) "# of Possible intersections: ",nseg(1)*nseg(2)


! Perform brute-force search over all red-segment-blue-segment combinations

starttime=seconds(0.0)
call bruteforce(nn,nseg,rednode,redsegment(1:nseg(1),1:2),bluenode, &
&    bluesegment(1:nseg(2),1:2),nin,intersections)
stoptime=seconds(starttime)

write(*,*) "time for bruteforce search: ",stoptime
write(*,*) "found ",nin," intersections"


! output the intersection points

call writepoints('intersect.vtu',nin,intersections)

deallocate(redelem,rednode,blueelem,bluenode,redsegment,bluesegment,intersections)

end program insect
