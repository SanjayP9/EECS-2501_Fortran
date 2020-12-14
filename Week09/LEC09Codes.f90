Program DyAll
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

contains

function RandMatrix(rows,columns) result(matrix)
implicit none
integer::rows,columns,i,j
real:: matrix(rows,columns)
do i=1,rows
 do j=1,columns
   matrix(i,j)=real(mod(time(),5))*real(i*j)/real(i+j)
 end do
end do
end function RandMatrix

subroutine Array(x,AVG)
implicit none
real, dimension (:,:), intent(in):: x ! maximum array
integer:: s(2)
real, intent(out)::AVG
s=shape(x)
AVG=sum(x)/real(s(1)*s(2))
end subroutine Array

end program DyAll




program my_main_prog
! this program calculates radius and perimeter of a circle          
implicit none
real:: radius,a
real, parameter:: pi = 4*atan(1.0)
print*, 'Enter the radius for your circle'
read(*,*) radius
print*, 'Pi number is:', pi
print*, 'Perimeter of your circle is:', perimeter_circle(radius)
call area_circle(radius,a)
print*, 'Area of your circle is:', a

contains
      
   subroutine area_circle(r,area)
   implicit none          
   real,intent(in):: r
   real,intent(out):: area
   real, parameter:: pi = 4*atan(1.0) 
   area=pi*r**2   
   end subroutine area_circle 
    
   function perimeter_circle(r) result(p)  
   implicit none
   real::r,p
   real, parameter:: pi = 4*atan(1.0)
   p = 2*pi*r  
   end function perimeter_circle
    
end program my_main_prog



