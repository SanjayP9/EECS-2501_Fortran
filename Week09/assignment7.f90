Program assignment7
use DyAllModule
implicit none

real, allocatable :: a(:,:)
real::average
integer:: n,m,i
print*, 'This program creates a nxm matrix using a random function'
print*, 'Then displays the average value of the matrix elements on the screen' 
print*, 'Enter n and m respectively (comma separated)'
read(*,*) n,m 
! creation of nxm random matrix
allocate(a(n,m))
a=RandMatrix(n,m)
print*, 'the generated matrix is'
do i=1,n
  write(*,*) a(i,:)
end do
Call Array(a,average)
print*, 'the average value of the matrix elements is:', average

end program assignment7

