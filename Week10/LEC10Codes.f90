program point
implicit none
integer, pointer:: ptr1, ptr2
integer, pointer, dimension(:):: aptr
integer, target:: trg
integer, target, dimension(10):: atrg
integer:: i
print*, associated(ptr1)
print*, associated(ptr2)
print*, associated(aptr)
ptr1=>trg
ptr2=>ptr1
aptr=>atrg
print*, associated(ptr1,trg)
print*, associated(ptr2,ptr1)
print*, associated(aptr,atrg)
ptr2=10
aptr(1:10)=(/7,8,9,10,11,12,15,-18,21,-36/)
print*, 'the pointer values are:',ptr1, ptr2,'the target value is:',trg
do i=1,10
print*, 'the array pointe',i,'value:',aptr(i),'the array target',i,' value is:',atrg(i)
enddo
nullify(ptr1)
nullify(ptr2)
nullify(aptr)
print*, 'the pointer values are:',ptr1, ptr2,'the target value is:',trg
print*, associated(ptr1,trg)
print*, associated(ptr2,ptr1)
print*, associated(ptr2,trg)
print*, associated(aptr,atrg)
end program point



program goto_example
implicit none
integer:: i,z,check
real:: a
print*, 'enter your integer input'
50 read(*,*) z
check=mod(z,3)-1
print*, z, check 

IF(check)10,20,30
10 a=0.1*z; assign 100 to i; goto 60
20 a=0.2*z; assign 200 to i; goto 60
30 a=0.3*z; assign 300 to i; goto 60

60 go to i, (100, 200, 300)

100 print*, 'lets try again',a
    goto 50
200 print*, 'lets try again',a
    goto 50
300 print*, 'lets stop here',a
    stop

end program goto_example