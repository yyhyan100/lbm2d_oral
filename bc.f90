subroutine bc()
!call non_eq_extra()
call inlet_vel()
call inlet_wall()
call outlet_wall()
call outlet_open()
call inner_wall()
! call upper_wall()
call lower_wall()
!call bc_macro()
end subroutine
!----------------------------------------------------------------------------
subroutine inlet_vel() ! Zou-He boundary condition for velocity inlet
	use vars
	integer i,j
	i=1
	do j=2,jed-1
	if (ph(i,j)==defInner) then
		rho(i,j)=(f(0,i,j)+f(2,i,j)+f(4,i,j)+2*(f(3,i,j)+f(6,i,j)+f(7,i,j)))/(1.0-u(i,j))
		f(1,i,j)=f(3,i,j)+2*rho(i,j)*u(i,j)/3.0
		f(5,i,j)=f(7,i,j)-0.5*(f(2,i,j)-f(4,i,j))+rho(i,j)*u(i,j)/6.0
		f(8,i,j)=f(6,i,j)+0.5*(f(2,i,j)-f(4,i,j))+rho(i,j)*u(i,j)/6.0
	endif
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine inlet_wall()  ! bounce back boundary condition
	use vars
	integer i,j
	i=1
	do j=2,jed-1
		if(ph(i,j)==defWall) then
			if(ph(i+1,j)==defInner) f(1,i,j)=f(3,i,j)
			if(ph(i,j+1)==defInner) f(2,i,j)=f(4,i,j)
			if(ph(i,j-1)==defInner) f(4,i,j)=f(2,i,j)
			if(ph(i+1,j+1)==defInner) f(5,i,j)=f(7,i,j)
			if(ph(i+1,j-1)==defInner) f(8,i,j)=f(6,i,j)
		endif
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine outlet_open() ! open boundary condition
	use vars
	integer i,j
	i=ied
	do j=2,jed-1
		if(ph(i,j)==defInner) then
			f(3,i,j)=2*f(3,i-1,j)-f(3,i-2,j)
			f(6,i,j)=2*f(6,i-1,j)-f(6,i-2,j)
			f(7,i,j)=2*f(7,i-1,j)-f(7,i-2,j)
		endif
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine outlet_wall()  ! bounce back boundary condition
	use vars
	integer i,j
	i=ied
	do j=2,jed-1
		if(ph(i,j)==defWall) then
			if(ph(i,j+1)==defInner) f(2,i,j)=f(4,i,j)
			if(ph(i-1,j)==defInner) f(3,i,j)=f(1,i,j)
			if(ph(i,j-1)==defInner) f(4,i,j)=f(2,i,j)
			if(ph(i-1,j+1)==defInner) f(6,i,j)=f(8,i,j)
			if(ph(i-1,j-1)==defInner) f(7,i,j)=f(5,i,j)
		endif
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine outlet_pressure() ! pressure boundary condition
	use vars
	integer i,j
	i=ied
	do j=2,jed-1
	if (ph(i,j)==defInner) then
		u(i,j)=(f(0,i,j)+f(2,i,j)+f(4,i,j)+2.0*(f(1,i,j)+f(5,i,j)+f(8,i,j)))/rho_out-1.0
		f(3,i,j)=f(1,i,j)-2*rho_out*u(i,j)/3
		f(6,i,j)=f(8,i,j)-0.5*(f(2,i,j)-f(4,i,j))-rho_out*u(i,j)/6.0
		f(7,i,j)=f(5,i,j)+0.5*(f(2,i,j)-f(4,i,j))-rho_out*u(i,j)/6.0
!		v(i,j)=0.0
	endif
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine upper_open() ! open boundary condition
	use vars
	integer i,j
	j=jed
	do i=1,ied
	if (ph(i,j)==defInner) then
		f(4,i,j)=2*f(4,i,j-1)-f(4,i,j-2)
		f(7,i,j)=2*f(7,i,j-1)-f(7,i,j-2)
		f(8,i,j)=2*f(8,i,j-1)-f(8,i,j-2)
	endif
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine upper_wall() ! bounce back boundary condition
	use vars
	integer i,j
	j=jed
	do i=1,ied
	if (ph(i,j)==defWall) then
		f(4,i,j)=f(2,i,j)
		f(7,i,j)=f(5,i,j)
		f(8,i,j)=f(6,i,j)
	endif
	enddo 
end subroutine
!----------------------------------------------------------------------------
subroutine lower_wall() ! bounce back boundary condition
	use vars
	integer i,j
	j=1
	do i=1,ied
		if(ph(i,j)==defWall) then
			if(ph(i,j+1)==defInner) f(2,i,j)=f(4,i,j)
			if(ph(i+1,j+1)==defInner) f(5,i,j)=f(7,i,j)
			if(ph(i-1,j+1)==defInner) f(6,i,j)=f(8,i,j)
		endif
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine inner_wall()  ! bounce back boundary condition for inner bounds
	use vars
	integer i,j,k
	real dist
	do i=2,ied-1
	do j=2,jed-1
		if(ph(i,j)==defWall) then
			if(ph(i+1,j)==defInner) f(1,i,j)=f(3,i,j)-2*wi(3)*rho(i,j)*(ei(3,1)*u(i,j)+ei(3,2)*v(i,j))/3.0
			if(ph(i,j+1)==defInner) f(2,i,j)=f(4,i,j)-2*wi(4)*rho(i,j)*(ei(4,1)*u(i,j)+ei(4,2)*v(i,j))/3.0
			if(ph(i-1,j)==defInner) f(3,i,j)=f(1,i,j)-2*wi(1)*rho(i,j)*(ei(1,1)*u(i,j)+ei(1,2)*v(i,j))/3.0
			if(ph(i,j-1)==defInner) f(4,i,j)=f(2,i,j)-2*wi(2)*rho(i,j)*(ei(2,1)*u(i,j)+ei(2,2)*v(i,j))/3.0
			if(ph(i+1,j+1)==defInner) f(5,i,j)=f(7,i,j)-2*wi(7)*rho(i,j)*(ei(7,1)*u(i,j)+ei(7,2)*v(i,j))/3.0
			if(ph(i-1,j+1)==defInner) f(6,i,j)=f(8,i,j)-2*wi(8)*rho(i,j)*(ei(8,1)*u(i,j)+ei(8,2)*v(i,j))/3.0
			if(ph(i-1,j-1)==defInner) f(7,i,j)=f(5,i,j)-2*wi(5)*rho(i,j)*(ei(5,1)*u(i,j)+ei(5,2)*v(i,j))/3.0
			if(ph(i+1,j-1)==defInner) f(8,i,j)=f(6,i,j)-2*wi(6)*rho(i,j)*(ei(6,1)*u(i,j)+ei(6,2)*v(i,j))/3.0
		endif
	enddo
	enddo
end subroutine


