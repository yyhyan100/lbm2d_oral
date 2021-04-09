subroutine output(kstep)
use vars
use io_parameters
integer kstep
if (mod(kstep,kstep_view)==0) then
	print *, "step=",kstep
endif
if (mod(kstep,kstep_save)==0) then
	if (file_format==1) then
		call output_tecplot(kstep)
		! call output_vtk_scalar(kstep,ist,ied,jst,jed,dx,dx,rho,'rho')
	endif
endif
end subroutine
!----------------------------------------------------------------------------
subroutine output_tecplot(kstep) ! 输出几何及物理量 （tecplot格式）
use vars
use io_parameters
character*50 num2char,filename
integer kstep
filename=trim(output_filename)
call NumToChar(kstep, num2char, len(num2char))
filename=trim(filename)//trim(num2char)//".dat"
print*, "write data file ...", filename
open(99,file=filename)
	write(99,*) "variables=x,y,rho,u,v,ph"
	write(99,*)  "zone ", "i= ", ied, " j= ", jed
	do j=1,jed
		do i=1,ied
			write(99,*) x(i,j),y(i,j),rho(i,j),u(i,j),v(i,j),ph(i,j)
        enddo
	enddo
close(99)
end subroutine output_tecplot

!----------------------------------------------------------------------------	  
subroutine NumToChar(ninput, name1, length)
      character(len=length) name1
	  character(len=128)::tmp
	  dimension ibit(12)
      logical highbit
    
	  n = ninput
      do ihigh = 8, 0, -1
	    if (n / 10**ihigh.ne.0) then
	      exit
	    endif
	  enddo

      do i = 1, ihigh
	    itmp = 10**(ihigh - i + 1)
        ibit(i) = n / itmp
	    n = n - ibit(i) * itmp
	  enddo
	  ibit(ihigh + 1) = n

      highbit=.false.
      do j=1, ihigh + 1
        if (highbit) then
          tmp=TRIM(name1)//char(48+ibit(j))
          name1=TRIM(tmp)
        else if (ibit(j).ne.0) then
          name1=char(48+ibit(j))
          highbit=.true.
        endif
      enddo
end subroutine

subroutine output_vtk_vector(n,ist,ied,jst,jed,dx,dy,vector,vector_name)
    implicit none
    integer, intent(in) :: n,ist,ied,jst,jed
    real, intent(in) :: dx,dy
    real, dimension(ist:ied, jst:jed, 2), intent(in) :: vector
    character(len=*), intent(in) :: vector_name
    integer :: i,j
    character(8) :: cStep
    character(200) :: file_name

    write(cStep,'(i4.4)') n
    file_name='./vtk/'//trim(vector_name)//'.'//trim(cStep)//'.vtk'
    open(15, file=trim(file_name))


    write(15,'(a)')'# vtk DataFile Version 2.0'
    write(15,*)'Comment goes here'
    write(15,*)'ASCII'
    write(15,*)
    write(15,*)'DATASET STRUCTURED_POINTS'
    write(15,*)'DIMENSIONS    ',ied-ist+1, jed-jst+1, 1
    write(15,*)
    write(15,*)'ORIGIN    0.000   0.000   0.000'
    write(15,*)'SPACING   ',dx, dy,'   1.000'
    write(15,*)
    write(15,*)'POINT_DATA', (ied-ist+1)*(jed-jst+1)
    write(15,*)'VECTORS '//trim(vector_name)//' double'
    write(15,*)
    do j=jst,jed
        write(15,*) (vector(i,j,1), vector(i,j,2), 0, i= ist,ied)
    end do
    close(15)

end subroutine output_vtk_vector

subroutine output_vtk_scalar(n,ist,ied,jst,jed,dx,dy,scalar,scalar_name)
    implicit none
    integer, intent(in) :: n,ist,ied,jst,jed
    real, intent(in) :: dx,dy
    real, dimension(ist:ied, jst:jed), intent(in) :: scalar
    character(len=*), intent(in) :: scalar_name
    integer :: i,j
    character(8) :: cStep

    write(cStep,'(i4.4)') n
    open(15, file=trim(adjustl(scalar_name))//'.'//trim(adjustl(cStep))//'.vtk')!, 


    write(15,'(a)')'# vtk DataFile Version 2.0'
    write(15,*)'Comment goes here'
    write(15,*)'ASCII'
    write(15,*)
    write(15,*)'DATASET STRUCTURED_POINTS'
    write(15,*)'DIMENSIONS    ',ied-ist+1, jed-jst+1, 1
    write(15,*)
    write(15,*)'ORIGIN    0.000   0.000   0.000'
    write(15,*)'SPACING   ',dx, dy,'   1.000'
    write(15,*)
    write(15,*)'POINT_DATA', (ied-ist+1)*(jed-jst+1)
    write(15,*)'SCALARS '//trim(adjustl(scalar_name))//' double'
    write(15,*)'LOOKUP_TABLE default'
    write(15,*)
    do j=jst,jed
        write(15,*) (scalar(i,j), i= ist,ied)
    end do
    close(15)

end subroutine output_vtk_scalar