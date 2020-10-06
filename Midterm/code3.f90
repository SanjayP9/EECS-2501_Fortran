program Code3
integer (kind=4) :: z
integer, dimension(10,10) :: matrix

print *, "input an integer entry"
read *, z

if (0 < z .AND. z < 6) then

	do i = 1,10
		do j = 1,10
			if ((i+j)**z > 10**6) then
				matrix(i,j) = 99999
			else
				matrix(i,j) = (i+j)**z
			end if
		end do
	end do

	print *, "The below 10x10 matrix is created based on your entry"
	do i = 1,10
		print *, (matrix(i, j), j =1,10)
	end do
	
end if




end program Code3