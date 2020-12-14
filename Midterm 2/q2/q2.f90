program q2
implicit none
real :: x, y
integer :: n

x = -1

do while(x < 1)
	n = n + 1
	y = 6.0*(x**3)+900.0*x+500.0
	if (y == 0.0) then
		print *, 'ANSWER: x=', x, ' Number of iterations: ', n
		return
	else
		print *, 'x=', x, 'y=', y
	end if
	
	x = x + 0.00001
end do
	
End program q2