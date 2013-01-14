program undef2nan_parallel

  integer :: nxp,nyp,nk1,i,j
  real, dimension(:,:), allocatable :: idata
  real nan
  data nan/Z'7fffffff'/

  namelist /param_index/ nxp,nyp,nk1
  open(unit=1,file='namelist.index',form='formatted',status='old',access='sequential')
  read(1,nml=param_index)
  close(unit=1)

  allocate(idata(nxp,nyp))

  ! input file
  open(10,file='input.out',access='direct',status='old',form='unformatted',recl=nxp*nyp*4)
  read(10,rec=1) idata
  close(10)

  ! parallel region
  !$OMP PARALLEL PRIVATE(i,j,nxp,nyp), SHARED(idata)
  !$OMP DO 
  do j=1, nyp
     do i=1, nxp
        if (idata(i,j).lt.-900) then
           idata(i,j)=nan
        endif
     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL

  ! output file
  open(11,file='output.out',access='direct',status='new',form='unformatted',recl=nxp*nyp*4)
  write(11,rec=1) idata
  close(11)

  deallocate(idata)

  stop
end program undef2nan_parallel
