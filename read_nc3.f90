!
! Read NetCDF(version 3) files
! producted by Takashi Unuma, Kyoto Univ.
! Last modified: 2012/11/14
!

program read_nc3
  implicit none

  include 'netcdf.inc'

  integer :: i,j,k,l
  integer :: nx,ny,nz,nt
  integer :: ncid,status,varid
  character :: fname*45
  character :: var*10
  real, allocatable, dimension(:,:,:,:) :: sla

  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  ! read namelist
  !ccccccccccccccccccccccccccccccccccccccccccccccccc
  namelist /param_netcdf3/ fname,var,nx,ny,nz,nt
  open(unit=10,file='namelist.netcdf3',form='formatted',status='old',access='sequential')
  read(10,nml=param_netcdf3)
  close(unit=10)

  allocate(sla(nx,ny,nz,nt))
  
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

  do l=1,nt
  do k=1,nz
  do j=1,ny
  do i=1,nx
     if(sla(i,j,k,l).ge.1.e20) then
        sla(i,j,k,l)=-999.
     end if
  end do
  end do
  end do
  end do

  open(unit=20,file='output.bin',form='unformatted',status='new',access='direct',recl=nx*ny*4)
  do l=1,nt
  do k=1,nz
     write(20,rec=k) ((sla(i,j,k,l),i=1,nx),j=ny,1,-1)
     print '(a17,4i5,f10.5)', "sla(1,1,k,l)   = ",1,1,k,l,sla(1,1,k,l)
     print '(a17,4i5,f10.5)', "sla(nx,ny,k,l) = ",nx,ny,k,l,sla(nx,ny,k,l)
  end do
  end do
  close(unit=20)

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
