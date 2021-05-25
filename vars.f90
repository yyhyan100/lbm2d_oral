module vars
integer,parameter:: Q=8
integer, parameter:: defNone=0
integer, parameter:: defWall=1
integer, parameter:: defInner=2
integer, parameter:: defMovingWall=3
integer, parameter:: defInlet=4
integer, parameter:: defOutlet=5

integer ied,jed,ist,jst
integer ied_tongue
real dx,dt,tt,t_end,alpha,omega
real ei(0:Q,2)
real, pointer:: f(:,:,:), feq(:,:,:), u(:,:), v(:,:), rho(:,:)
integer, pointer:: ph(:,:), jed_top(:)
real, pointer:: x(:,:), y(:,:)
real wi(0:Q),g(0:Q)
real omg,u0,rho_in,rho_out
contains

subroutine allocateField()
	allocate(f(0:Q,ied,jed))
	allocate(feq(0:Q,ied,jed))
	allocate(u(ied,jed))
	allocate(v(ied,jed))
	allocate(rho(ied,jed))
	allocate(ph(ied,jed))	
	allocate(jed_top(ied))	
	allocate(x(ied,jed))
	allocate(y(ied,jed))
end subroutine

subroutine deallocateField()
	deallocate(f,feq,u,v,rho,ph,x,y)
end subroutine
end module

!----------------------------------------------------------------------------
module io_parameters
	character(len=50),save:: output_filename
	integer,save:: kstep_view,kstep_save,data_binary,file_format
end module io_parameters

