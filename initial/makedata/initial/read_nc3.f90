!
! Read NetCDF(version 3) files
! producted by Takashi Unuma, Kyoto Univ.
! Last modified: 2012/11/14
!

program read_nc3
  implicit none

  include 'netcdf.inc'

!  integer :: i,j
  integer :: nx,ny
  integer :: ncid,status,varid
  character :: fname*45
  character :: var*10
  real, allocatable, dimension(:,:) :: sla,msla


  status = nf_def_var(ncid,"time",nf_real,1,timeid,varid)
  status = nf_put_att_text(ncid,varid,"def",34,"time since beginning of simulation")
  status = nf_put_att_text(ncid,varid,"units",33,"minutes since 2000-01-01 00:00:00")
  
  status = nf_def_var(ncid,"ni",nf_real,1,niid,varid)
  status = nf_put_att_text(ncid,varid,"def",40,"west-east location of scalar grid points")
  status = nf_put_att_text(ncid,varid,"units",11,"degree_east")
  
  status = nf_def_var(ncid,"nj",nf_real,1,njid,varid)
  status = nf_put_att_text(ncid,varid,"def",42,"south-north location of scalar grid points")
  status = nf_put_att_text(ncid,varid,"units",12,"degree_north")
  
!  status = nf_def_var(ncid,"nk",nf_real,1,nkid,varid)
!  status = nf_put_att_text(ncid,varid,"def",36,"nominal height of scalar grid points")
!  status = nf_put_att_text(ncid,varid,"units",2,"km")
  
  
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! read namelist
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  namelist /param_netcdf3/ fname,var,nx,ny
  open(unit=10,file='namelist.netcdf3', &
       form='formatted',status='old',   &
       access='sequential')
  read(10,nml=param_netcdf3)
  close(unit=10)

  allocate(sla(nx,ny),msla(nx,ny))
  
  ! open the original file
  status=nf_open(fname,nf_write,ncid)
  if (status.ne.nf_noerr) then
     call stoprnc3(status)
  else
     print *, "FILE NAME  = ",fname
     print *, "ncid       = ",ncid
  end if

  ! get the varid
  status=nf_inq_varid(ncid,var,varid)
  if (status.ne.nf_noerr) then
     call stoprnc3(status)
  else
     print *, "VALUE NAME = ",var
     print *, "varid       = ",varid
  end if

  ! read the value array
  status=nf_get_var_real(ncid,varid,sla)
  if (status.ne.nf_noerr) then
     call stoprnc3(status)
  end if
  
!  do j=1,ny
!     do i=1,nx
!        msla(i,j)=sla(i,j)*real(10.)
!        print *, i,j,sla(i,j),msla(i,j)
!     end do
!  end do

  open(unit=20,file='output.bin',       &
       form='unformatted',status='new', &
       access='direct',recl=nx*ny*4)
  write(20,rec=1) sla
  close(unit=20)

  status = nf_put_var_real(ncid,varid,msla)
  if (status.ne.nf_noerr) then
     call stoprnc3(status)
  else
     print *, "Replaced."
     print *, "Close."
  end if

  stop
end program read_nc3


subroutine stoprnc3(status)
  implicit none
  integer :: status
  character :: nf_strerror*80
  
  print *, nf_strerror(status)
  print *, 'PROGRAM ERROR!!!'
  
  stop
end subroutine stoprnc3
