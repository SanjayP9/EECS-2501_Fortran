program Code3
implicit none

integer :: n
real ( kind = 8 ), dimension(4,4):: a
real ( kind = 8 ),dimension(4) :: d
real ( kind = 8 ),dimension(4,4) :: v


a = reshape((/ 16, 20, 16, 10, 20, 8, 4, 10, 16, 4, 6, 20, 10, 10, 20, 12 /), shape(a))

n = 4

call jacobi_eigenvalue(n, a, v, d, 10, 10)
print *, "Eigenvalues:"

do i=1, size(d) - 1
	print *, d(i)
end do

print *, "Eigenvectors:"
do i=1, 4
	do j=1, 4
		print *, v(i,j)
	end do
end do

end program Code3