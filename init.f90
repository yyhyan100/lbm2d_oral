subroutine init() 
	use vars
	use io_parameters
	integer i,j
	real uv,eu,tao
	open(10,file="control.in")
	read(10,*) ied,jed,dx,dt,omega
	read(10,*) u0,rho_in,rho_out,tao
	read(10,*) t_end,kstep_save,kstep_view,file_format
	read(10,*) output_filename
	close(10)
	ist=1
	jst=1
	omg=1.0/tao
	print *, "nu = ", (2*tao-1)*dx*dx/dt/6 
	print *, "Re = ", ied*u0*dt*6/(2*tao-1)*dx
	call allocateField()
	call gengrid()
	u(:,:)=0.0
	v(:,:)=0.0
	rho(:,:)=rho_in
	u(1,51:jed)=u0
	ei(:,:)=0.0
	ei(1,1)=1.0
	ei(5,1)=1.0
	ei(8,1)=1.0
	ei(3,1)=-1.0
	ei(6,1)=-1.0
	ei(7,1)=-1.0
	ei(5,2)=1.0
	ei(2,2)=1.0
	ei(6,2)=1.0
	ei(4,2)=-1.0
	ei(7,2)=-1.0
	ei(8,2)=-1.0
	wi(0)=4.0/9
	wi(1:4)=1.0/9
	wi(5:8)=1.0/36

	ph(:,:)=2
	
call set_init_interface()
call init_f()
end subroutine
!-----------------------------------------------
subroutine set_init_interface()
use vars
integer i,j,k1,k2
ph(ist,:)=1
ph(:,jst)=1
ph(:,jed)=1
call set_top_boundary()
call set_moving_boundary()
end subroutine
!-----------------------------------------------
subroutine init_f()
use vars
integer i,j
real uv,eu
do i=1,ied
	do j=1,jed
		if (ph(i,j)>0) then
			uv=u(i,j)**2+v(i,j)**2
			do k=0,Q
				eu=ei(k,1)*u(i,j)+ei(k,2)*v(i,j)
				feq(k,i,j)=wi(k)*rho(i,j)*(1+3*eu+4.5*eu*eu-1.5*uv)
			enddo
		endif
	enddo
enddo
f(:,:,:)=feq(:,:,:)
end subroutine
!-----------------------------------------------
subroutine gengrid() 
use vars
do i=1,ied
do j=1,jed
	x(i,j)=(-300+i-1)*dx
	y(i,j)=(j-1)*dx
enddo
enddo
end subroutine

subroutine set_top_boundary()
use vars
integer i,j

open(22, file='./top_profile/j_index.wall')
do i=1,ied
	read(22,*) j0
	! j0=sqrt(90000.0-(i-200)**2)-150
	! print*, j0
	! j0=100
	! j0=i/4+1
	! j0=150-i/4
	upper_jed(i)=j0
enddo

do i=1,ied
	j0=upper_jed(i)
	ph(i,j0)=defWall
	do j = j0+1, jed
		ph(i,j)=defNotCal
	enddo
enddo
close(22)
end subroutine