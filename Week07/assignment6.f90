Program assignment6
implicit none

	print *, "total length is: ", GET_LENGTH()

contains
	INTEGER FUNCTION GET_LENGTH()
		integer i,n,stat
		real x
		open(20,file='data.dat')
		n=-1
		stat=0
		do while(stat == 0)
			n=n+1
			read(20,*,iostat=stat)x
		enddo
		GET_LENGTH = n
	END FUNCTION GET_LENGTH
end program assignment6

