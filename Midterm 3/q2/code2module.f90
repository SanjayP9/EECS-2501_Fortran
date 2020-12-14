module code2module
	contains
	subroutine cal(x, avg, stdDev, var, m)
		real, intent(inout):: avg
		real, intent(inout):: stdDev
		real, intent(inout):: var
		real, dimension(:), intent(in):: x
		integer, intent(in), optional:: m
		
		real xSum
		real varSum
		integer total;
		
		varSum = 0
		xSum = 0
		
		if (PRESENT(m)) then
			total = m
			
			do i=1, m
				xSum = xSum + x(i)
			end do
		else 
			total = SIZE(x)
			xSum = SUM(x)
		end if
		
		
		avg = xSum/SIZE(x);
		
		do i=1, total
			varSum = varSum + (x(i)*x(i))
		end do
		
		var = (varSum/total) - ((xSum**2)/(total**2))
		stdDev = SQRT(var)
		
	end subroutine cal
end module code2module