subroutine set_moving_boundary()
use vars
alpha=omega*tt
print *, "angle=",alpha
do i=1,ied
do j=1,jed
	if (y(i,j) < -tan(alpha)*x(i,j)) ph(i,j)=0
enddo
enddo
call reset_interface()
call reset_velocity()
end subroutine

!-----------------------------------------------
subroutine reset_interface()
use vars
integer i,j,k1,k2

do i=2,ied-1
do j=2,jed-1
	if (ph(i,j)==0) then
		do k1=-1,1,2
		do k2=-1,1,2
			if(ph(i+k1,j+k2)==2) ph(i+k1,j+k2)=1	
		enddo
		enddo
	endif
enddo
enddo

j=1
do i=2,ied-1
	if (ph(i,j)==0) then
		if(ph(i,j+1)==2) ph(i,j+1)=1	
		if(ph(i+1,j+1)==2) ph(i+1,j+1)=1
		if(ph(i-1,j+1)==2) ph(i-1,j+1)=1
	endif
enddo

i=1
do j=2,jed-1
	if (ph(i,j)==0) then
		if(ph(i,j+1)==2) ph(i,j+1)=1	
		if(ph(i+1,j+1)==2) ph(i+1,j+1)=1
		if(ph(i+1,j)==2) ph(i+1,j)=1
	endif
enddo
end subroutine
!-----------------------------------------------
subroutine reset_velocity()
use vars
integer i,j,k1,k2
real dist
do i=2,ied-1
do j=2,jed-1
	if (ph(i,j)==1) then
		dist=sqrt(x(i,j)**2+y(i,j)**2)
		v(i,j)=omega*dist*cos(alpha)
		u(i,j)=omega*dist*sin(alpha)
	endif
enddo
enddo
end subroutine
