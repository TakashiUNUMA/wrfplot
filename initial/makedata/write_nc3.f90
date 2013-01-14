!ccccccccccccccccccccccccccccccccccccccccccccccccc
!
! Write NetCDF(version 3, COARDS) files
! producted by Takashi Unuma, Kyoto Univ.
! Last modified: 2012/11/14
!
!ccccccccccccccccccccccccccccccccccccccccccccccccc

program write_nc3 
  implicit none 

  include 'netcdf.inc'
  
  character (len = *), parameter :: fname = "output.nc"
  integer :: i,j
  integer, parameter :: NX = 101, NY = 101
  integer :: status, ncid, varid
  integer :: niid, njid, timeid
  integer, dimension(:,:), allocatable :: data_out

  allocate(data_out(NY, NX))

  do i = 1, NX
     do j = 1, NY
        data_out(j, i) = (i - 1) * NY + (j - 1)
     end do
  end do


  status = nf_create(fname, NF_CLOBBER, ncid)
  if (status.ne.nf_noerr) then
     call stopwnc3(status)
  else
     print *, "FILE NAME = ", fname
     print *, "ncid = ", ncid
  end if

!  status = nf_def_dim(ncid,'ni',NX,niid)
!  status = nf_def_dim(ncid,'nj',NY,njid)

!  status = nf_def_var(ncid,"time",nf_real,1,timeid,varid)
!  status = nf_put_att_text(ncid,varid,"def",34,"time since beginning of simulation")
!  status = nf_put_att_text(ncid,varid,"units",33,"minutes since 2000-01-01 00:00:00")
  
!  status = nf_def_var(ncid,"ni",nf_real,1,niid,varid)
!  status = nf_put_att_text(ncid,varid,"def",40,"west-east location of scalar grid points")
!  status = nf_put_att_text(ncid,varid,"units",11,"degree_east")
  
!  status = nf_def_var(ncid,"nj",nf_real,1,njid,varid)
!  status = nf_put_att_text(ncid,varid,"def",42,"south-north location of scalar grid points")
!  status = nf_put_att_text(ncid,varid,"units",12,"degree_north")
  
!  status = nf_inq_dimid(ncid,'time',timeid)
!  status = nf_inq_dimid(ncid,'ni',niid)
!  status = nf_inq_dimid(ncid,'nj',njid)

  status = nf_def_dim(ncid, "x", NX, niid)
  if (status.ne.nf_noerr) then
     call stopwnc3(status)
  else
     print *, "niid = ", niid
  end if
  status = nf_def_dim(ncid, "y", NY, njid)
  if (status.ne.nf_noerr) then
     call stopwnc3(status)
  else
     print *, "njid = ", njid
  end if

  status = nf_def_var(ncid, "data_out", NF_REAL, 2, (/niid,njid/), varid)
  if (status.ne.nf_noerr) then
     call stopwnc3(status)
  end if

  status = nf_enddef(ncid)
  if (status.ne.nf_noerr) then
     call stopwnc3(status)
  end if

  status = nf_put_var_real(ncid, varid, data_out)
  if (status.ne.nf_noerr) then
     call stopwnc3(status)
  end if

  status = nf_close(ncid)
  if (status.ne.nf_noerr) then
     call stopwnc3(status)
  else
     print *, "File closed."
  end if

end program write_nc3

subroutine stopwnc3(status)
  implicit none
  integer :: status
  character :: nf_strerror*80
  
  print *, nf_strerror(status)
  print *, 'PROGRAM ERROR!!!'
  
  stop
end subroutine stopwnc3
