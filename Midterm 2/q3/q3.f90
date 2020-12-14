program q3
implicit none
  integer i,n,stat, num_of_elements, pos_index
  real x, min_val, pos_sum, max_val
  real, allocatable::file(:), positive(:)
  open(20,file='data.txt')
  n=-1
  stat=0
  do while(stat == 0)
     n=n+1
     read(20,*,iostat=stat)x
  enddo
  allocate(file(n))
  rewind(20)
  do i=1,n
  read(20,*)file(i)
  end do
  
  
print *, 'Size of the array is: ', n
print *, 'Please enter the number of elements you want to process. your entry should be an integer number > 1 and < ', n

read *, num_of_elements
if (num_of_elements > 1 .AND. num_of_elements < n) then
	print *, 'first n elements:'
	allocate(positive(num_of_elements))
	pos_index = 1
	min_val = file(1)
	
	do i=1,num_of_elements
		write(*,*) file(i)
		
		if (file(i) < min_val) then
			min_val = file(i)
		end if
		
		if (file(i) > 0) then 
			positive(pos_index) = file(i)
			pos_index = pos_index + 1
		end if 
	enddo
else
	return
end if

print *, 'min value from first n elements is: ', min_val

open(9, file = 'output.txt')
i = 1
max_val = positive(0)
do i = 1, pos_index - 1
	write(9, *) positive(i)
	if (max_val < positive(i)) then
		max_val = positive(i)
	end if 
end do

print *, 'Sum of positive elements: ', SUM(positive)
print *, 'Maximum element is: ', max_val
close(7)
	
End program q3