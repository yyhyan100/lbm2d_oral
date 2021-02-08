subroutine set_moving_boundary()
use vars
real angle
angle=omg*tt
do i=1,ied
do j=1,jed
	if (y(i,j) < -tan(angle)*x(i,j)) ph(i,j)=0
enddo
enddo
call reset_interface()
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

end subroutine
