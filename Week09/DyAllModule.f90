module DyAllModule
	implicit none
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

end module DyAllModule