program week02

character(len = 65) :: name_of_the_user
integer :: desired_num

integer sum, i, max
sum = 0

! Heading
print '(a)', 'This program prints all odd numbers between 0 to maximum 1000. And calculates the sum of them.'

! Enter name
print '(a)', 'Please enter your name (use maximum 65 letters)'
read (*,100) name_of_the_user 
100 format(A)

! Desired integers number
print '(a)', 'Please enter your desire integers number (your number should be between 0 to 1000)'
read *, desired_num
print '(a,g0,a)', 'The odd numbers between 0 to ', desired_num, ' are:'
do i = 1, desired_num, 2
	sum = sum + i
	print '(g0)', i
end do
print '(a,g0)', 'The sum of these odd numbers is: ', sum
print '(a,a,a)', 'Thank you ', trim(name_of_the_user), ' for using this code.'

end program week02
