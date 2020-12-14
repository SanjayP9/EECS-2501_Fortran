Program q1
implicit none
  integer i,n,stat
  real temp
  real, allocatable::x(:),y(:)
  
  real xSum, ySum, xMean, yMean, numSum, denSum, slope, b
  
  xSum = 0
  ySum = 0
  
  open(20,file='XYdata.txt')
  n=-1
  stat=0
  do while(stat == 0)
     n=n+1
     read(20,*,iostat=stat)temp
  enddo
  
  allocate(x(n))
  allocate(y(n))
  rewind(20)
  do i=1,n
	read(20,*)x(i),y(i)
	xSum = xSum + x(i)
	ySum = ySum + y(i)
  end do
  xMean = xSum/n
  yMean = ySum/n
  
  do i=1,n
	numSum = numSum + ((x(i) - xMean) * (y(i) - yMean))
	denSum = denSum + (x(i) - xMean)**2
  end do
  
  slope = numSum / denSum
  
  b = ySum - (slope * xSum)
  
  print *, "the line equation is Y=", slope, "X+", b
 
  
end program q1