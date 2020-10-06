program Code1

real (KIND=8) :: z
real (KIND=8):: a
real (KIND=8):: numer
real (KIND=8):: denom

real (KIND=8):: result

read *, z

a = 1/(1+15*(z**3))
denom = (LOG(z-1)) - a
numer = ((z**2) - 1)**(4*SIN(z))

g = numer/denom

print *, g

end program Code1