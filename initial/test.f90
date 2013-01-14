program calc_index
  use Thermo_Function
  implicit none
  integer :: i,j
  integer, parameter :: nx=481
  integer, parameter :: ny=505
  real, dimension(:,:), allocatable :: temp,rh,prmsl
  real, dimension(:,:), allocatable :: press,es,qv,thetae
  integer,parameter :: debug_level=100

  ! allocate values
  allocate( temp(nx,ny), rh(nx,ny), prmsl(nx,ny) )
  allocate( press(nx,ny), es(nx,ny), qv(nx,ny), thetae(nx,ny) )
  if(debug_level.ge.100) print *, "success allocate"

  ! read temp
  open(unit=10, file="temp.bin",            &
       form='unformatted', access='direct', &
       status='old', recl=nx*ny*4)
  read(unit=10,rec=1) temp
  close(unit=10)
  if(debug_level.ge.100) print *, "success open file of temp"
  if(debug_level.ge.100) print *, "check: temp(1,1) ",temp(1,1)

  ! read rh
  open(unit=11, file="rh.bin",            &
       form='unformatted', access='direct', &
       status='old', recl=nx*ny*4)
  read(unit=11,rec=1) rh
  close(unit=11)
  if(debug_level.ge.100) print *, "success open file of rh"
  if(debug_level.ge.100) print *, "check: rh(1,1) ",rh(1,1)

  ! read prmsl
  open(unit=12, file="prmsl.bin",            &
       form='unformatted', access='direct', &
       status='old', recl=nx*ny*4)
  read(unit=12,rec=1) prmsl
  close(unit=12)
  if(debug_level.ge.100) print *, "success open file of prmsl"
  if(debug_level.ge.100) print *, "check: prmsl(1,1) ",prmsl(1,1)

  ! calculate indexes
  do j=1,ny
  do i=1,nx
     press(i,j)=prmsl(i,j)*0.01
     es(i,j)=RHT_2_e(rh(i,j),temp(i,j))
     qv(i,j)=eP_2_qv(es(i,j),prmsl(i,j))
     thetae(i,j)=TqvP_2_thetae(temp(i,j),qv(i,j),prmsl(i,j))
  end do
  end do
  if(debug_level.ge.100) print *, "check: press(1,1) ",press(1,1)
  if(debug_level.ge.100) print *, "check: es(1,1) ",es(1,1)
  if(debug_level.ge.100) print *, "check: qv(1,1) ",qv(1,1)
  if(debug_level.ge.100) print *, "check: thetae(1,1) ",thetae(1,1)

  ! output press
  open(unit=20, file="press.bin",            &
       form='unformatted', access='direct', &
       status='new', recl=nx*ny*4)
  write(unit=20,rec=1) press
  close(unit=20)
  if(debug_level.ge.100) print *, "success open file of press"

  ! output qv
  open(unit=21, file="qv.bin",            &
       form='unformatted', access='direct', &
       status='new', recl=nx*ny*4)
  write(unit=21,rec=1) qv
  close(unit=21)
  if(debug_level.ge.100) print *, "success open file of qv"

  ! output thetae
  open(unit=22, file="thetae.bin",            &
       form='unformatted', access='direct', &
       status='new', recl=nx*ny*4)
  write(unit=22,rec=1) thetae
  close(unit=22)
  if(debug_level.ge.100) print *, "success open file of thetae"

  stop
end program calc_index
