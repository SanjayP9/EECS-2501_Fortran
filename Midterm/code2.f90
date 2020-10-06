program Code2

real (KIND=4), dimension(5,3) :: matrix
character(len = 65) :: input
integer :: comma_count
character(len = 1) :: comma

real (KIND=4), dimension(5) :: results

do i = 1,5
	do j = 1,3
		matrix(i,j) = 0
	end do
end do


do i = 1,3
	print '(a)', "input a vector with exactly 5 elements of numbers (comma separated)"
	read (5,*) matrix(1,i), matrix(2,i), matrix(3,i), matrix(4,i), matrix(5,i)
end do

do j = 1,5
	do i = 1,3
		results(j) = results(j) + matrix(j,i)
	end do
	results(j) = results(j)/3
end do

print '(a)', "The entered vectors make the below 5x3 matrix:"

do j = 1,5
	print *, matrix(j,1), " ", matrix(j,2), " ", matrix(j,3) 
end do

print '(a)', "The average values of each entered vector respectively are:"
print *, results(1), " ", results(2), " ", results(3), " ", results(4), " ", results(5)


end program Code2