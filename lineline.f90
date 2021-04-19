program lineline

implicit none

real(8) :: a(2), b(2), c(2), d(2), cross(2,2)
integer :: sect

a(1)=0.0d0

a(2)=0.0d0
b(1)=5.0d0
b(2)=5.0d0
c(1)=5.0d0
c(2)=0.0d0
d(1)=0.0d0
d(2)=5.0d0

    call intersect(a,b,c,d,sect,cross)
    call writesect(sect,cross)
    
a(1)=1.0d0
a(2)=3.0d0
b(1)=9.0d0
b(2)=3.0d0
c(1)=0.0d0
c(2)=1.0d0
d(1)=2.0d0
d(2)=1.0d0

    call intersect(a,b,c,d,sect,cross)
    call writesect(sect,cross)
    
a(1)=1.0d0
a(2)=5.0d0
b(1)=6.0d0
b(2)=8.0d0
c(1)=0.5d0
c(2)=3.0d0
d(1)=6.0d0
d(2)=4.0d0
    
    call intersect(a,b,c,d,sect,cross)
    call writesect(sect,cross)

a(1)=1.0d0
a(2)=1.0d0
b(1)=3.0d0
b(2)=8.0d0
c(1)=0.5d0
c(2)=2.0d0
d(1)=4.0d0
d(2)=7.0d0
    
    call intersect(a,b,c,d,sect,cross)
    call writesect(sect,cross)

a(1)=1.0d0
a(2)=2.0d0
b(1)=3.0d0
b(2)=6.0d0
c(1)=2.0d0
c(2)=4.0d0
d(1)=4.0d0
d(2)=8.0d0
    
    call intersect(a,b,c,d,sect,cross)
    call writesect(sect,cross)
    
a(1)=3.5d0
a(2)=9.0d0
b(1)=3.5d0
b(2)=0.5d0
c(1)=3.0d0
c(2)=1.0d0
d(1)=9.0d0
d(2)=1.0d0
    
    call intersect(a,b,c,d,sect,cross)
    call writesect(sect,cross)
    
a(1)=2.0d0
a(2)=3.0d0
b(1)=7.0d0
b(2)=9.0d0
c(1)=1.0d0
c(2)=2.0d0
d(1)=5.0d0
d(2)=7.0d0
    
    call intersect(a,b,c,d,sect,cross)
    call writesect(sect,cross)
    
end program lineline


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
