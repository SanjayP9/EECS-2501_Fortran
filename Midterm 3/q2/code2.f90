program Code2
use code2module
implicit none
real,dimension (:), allocatable :: x
integer:: i,n,stat
real:: r,avg,stdDev,var
open(10,file='data.txt')
n=0
stat=0
do while(stat == 0)
     n=n+1
     read(10,*,iostat=stat)r
enddo
rewind(10)
allocate(x(n-1))
do i=1,n-1
  read(10,*) x(i)
end do
call cal(x,avg,stdDev,var)
Print*, 'size of data is:', n-1
Print*, 'average of data=', avg
Print*, 'standard deviation of data=', stdDev 
Print*, 'variance of data=', var
end program Code2