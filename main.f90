program main
use vars
integer kstep
tt=0.0
call init()

kstep=1
do while (tt<=t_end)
	call streaming()
	call bc()
	call getMacro()
	call getFeq()
	call collision()
	call output(kstep)
	kstep=kstep+1
	tt=tt+dt
	call set_moving_boundary()
enddo
call output(kstep-1)
call deallocateField()
print *, "done!"
end program
!----------------------------------------------------------------------------
subroutine getMacro() 
use vars
integer i,j,k

do i=1,ied
do j=1,jed
	if (ph(i,j)>0) then
		rho(i,j)=0.0
		u(i,j)=0.0
		v(i,j)=0.0
		do k=0,Q
			rho(i,j)=rho(i,j)+f(k,i,j)
			u(i,j)=u(i,j)+ei(k,1)*f(k,i,j)
			v(i,j)=v(i,j)+ei(k,2)*f(k,i,j)
		enddo
		u(i,j)=u(i,j)/rho(i,j)
		v(i,j)=v(i,j)/rho(i,j)
	endif
enddo
enddo

i=1
do j=1,jed-1
	if (ph(i,j)==2) then
		u(i,j)=u0
		v(i,j)=0.0
	endif
enddo
call reset_velocity()
end subroutine
