program q1
implicit none
integer :: n

print *, 'Enter n'
read *, n
print *, ToH(n)

contains
	recursive integer function ToH(n) result(res)
		integer :: n
		
		if (n==1) then
			res = 1
		else
			res = 2 * ToH(n-1) + 1
		end if
	end function ToH
	
End program q1