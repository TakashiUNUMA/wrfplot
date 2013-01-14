!
! Program of calcucate indexes for WRF-ARW
! produced by Takashi Unuma, Kyoto Univ.
! Last modified: 2013/01/14
!

program calc_index

  use Thermo_Function
  use Thermo_Advanced_Function
  use Thermo_Advanced_Routine

  implicit none
  integer :: i, j, k, num
  integer :: nxp,nyp,nk1
!  real                                :: nan,undeff,z_ref
  real, dimension(:),     allocatable :: pressp,pint,x,y,z
!  real, dimension(:,:),   allocatable :: temp,rh,prmsl
!  real, dimension(:,:),   allocatable :: press,es,qv,thetae,td
  real, dimension(:,:),   allocatable :: cape2d,cin2d,lcl2d,lfc2d,lnb2d
  real, dimension(:,:),   allocatable :: eh,srh
  real, dimension(:,:),   allocatable :: ltemp500,ssi,brn,cor,ki,tt,pw
  real, dimension(:,:,:), allocatable :: tempp,qvp,uuu,vvv,www,hgt,dbz
  real, dimension(:,:,:), allocatable :: esp,thetaep,wspd,zeta,eta,xi
  real, dimension(:,:,:), allocatable :: ptp,rhop,pvp,tdp,qfu,qfv,qfwind
!  data nan/Z'7fffffff'/
  integer,parameter :: debug_level=100

  namelist /param_index/ nxp,nyp,nk1
  open(unit=1,file='namelist.index',form='formatted',status='old',access='sequential')
  read(1,nml=param_index)
  close(unit=1)

  ! allocate values
  allocate( pressp(nk1), pint(nk1), x(nxp), y(nyp), z(nk1) )
!  allocate( temp(nxs,nys), rh(nxs,nys), prmsl(nxs,nys), press(nxs,nys) )
!  allocate( es(nxs,nys), qv(nxs,nys), thetae(nxs,nys), td(nxs,nys) )
  allocate( tempp(nxp,nyp,nk1), qvp(nxp,nyp,nk1), hgt(nxp,nyp,nk1) )
  allocate( esp(nxp,nyp,nk1), thetaep(nxp,nyp,nk1), dbz(nxp,nyp,nk1) )
  allocate( uuu(nxp,nyp,nk1), vvv(nxp,nyp,nk1), www(nxp,nyp,nk1) )
  allocate( ptp(nxp,nyp,nk1), rhop(nxp,nyp,nk1), pvp(nxp,nyp,nk1) )
  allocate( tdp(nxp,nyp,nk1), ki(nxp,nyp), tt(nxp,nyp), pw(nxp,nyp) )
  allocate( lcl2d(nxp,nyp), lfc2d(nxp,nyp), lnb2d(nxp,nyp), wspd(nxp,nyp,nk1) )
  allocate( cape2d(nxp,nyp), cin2d(nxp,nyp), cor(nxp,nyp), ssi(nxp,nyp) )
  allocate( eh(nxp,nyp), srh(nxp,nyp), ltemp500(nxp,nyp), brn(nxp,nyp) )
  allocate( qfu(nxp,nyp,nk1), qfv(nxp,nyp,nk1), qfwind(nxp,nyp,nk1) )
  allocate( zeta(nxp,nyp,nk1), eta(nxp,nyp,nk1), xi(nxp,nyp,nk1) )
  if(debug_level.ge.100) print *, "DEBUG: Success allocate"

  ! read temp [K] for surface
  num=10
!  open(unit=num, file="temp.bin",form='unformatted',access='direct',status='old',recl=nxs*nys*4)
!  read(unit=num,rec=1) temp
!  close(unit=num)
!  if(debug_level.ge.100) print *, "DEBUG: Success open file of temp"
!  if(debug_level.ge.100) print *, "DEBUG: temp(1,1)    ",temp(1,1)

  ! read rh [%] for surface
!  open(unit=num, file="rh.bin",form='unformatted',access='direct',status='old',recl=nxs*nys*4)
!  read(unit=num,rec=1) rh
!  close(unit=num)
!  if(debug_level.ge.100) print *, "DEBUG: Success open file of rh"
!  if(debug_level.ge.100) print *, "DEBUG: rh(1,1)      ",rh(1,1)

  ! read prmsl [Pa] for surface
!  open(unit=num, file="prmsl.bin",form='unformatted',access='direct',status='old',recl=nxs*nys*4)
!  read(unit=num,rec=1) prmsl
!  close(unit=num)
!  if(debug_level.ge.100) print *, "DEBUG: Success open file of prmsl"
!  if(debug_level.ge.100) print *, "DEBUG: prmsl(1,1)   ",prmsl(1,1)


  ! read temp [K] for pressure
  num=10
  open(unit=num, file="ttt.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=num,rec=1) tempp
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of tempp"
  if(debug_level.ge.100) print *, "DEBUG: tempp(1,1,1) ",tempp(1,1,1)

  ! read qv [kg/kg] for pressure
  open(unit=num, file="qvp.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=num,rec=1) qvp
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of qvp"
  if(debug_level.ge.100) print *, "DEBUG: qvp(1,1,1)   ",qvp(1,1,1)

  ! read hgt [m] for pressure
  open(unit=num, file="hgt.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=num,rec=1) hgt
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of hgt"
  if(debug_level.ge.100) print *, "DEBUG: hgt(1,1,1)   ",hgt(1,1,1)

  ! read uuu [m/s] for pressure
  open(unit=num, file="uuu.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=num,rec=1) uuu
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of uuu"
  if(debug_level.ge.100) print *, "DEBUG: uuu(1,1,1)   ",uuu(1,1,1)

  ! read vvv [m/s] for pressure
  open(unit=num, file="vvv.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=num,rec=1) vvv
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of vvv"
  if(debug_level.ge.100) print *, "DEBUG: vvv(1,1,1)   ",vvv(1,1,1)

  ! read www [m/s] for pressure
  open(unit=num, file="www.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=num,rec=1) www
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of www"
  if(debug_level.ge.100) print *, "DEBUG: www(1,1,1)   ",www(1,1,1)

  ! read cape [J/kg] for pressure
  open(unit=num, file="mcape.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*4)
  read(unit=num,rec=1) cape2d
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of cape"
  if(debug_level.ge.100) print *, "DEBUG: cape(1,1)    ",cape2d(1,1)

  ! read dbz [dBZ] for pressure
  open(unit=num, file="dbz.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=num,rec=1) dbz
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of dbz"
  if(debug_level.ge.100) print *, "DEBUG: dbz(1,1,1)   ",dbz(1,1,1)

  ! calculate indexes for surface
!!$omp parallel default(shared)
!!$omp do private(i,j)
!  do j=1,nys
!  do i=1,nxs
!     press(i,j)=prmsl(i,j)*real(0.01)
!     es(i,j)=RHT_2_e( rh(i,j), temp(i,j) )
!     qv(i,j)=eP_2_qv( es(i,j), prmsl(i,j) )
!     thetae(i,j)=TqvP_2_thetae( temp(i,j), qv(i,j), prmsl(i,j) )
!     td(i,j)=es_TD(es(i,j))
!  end do
!  end do
!!$omp end do
!!$omp end parallel
!  if(debug_level.ge.100) print *, "DEBUG: press(1,1)   ",press(1,1)
!  if(debug_level.ge.100) print *, "DEBUG: es(1,1)      ",es(1,1)
!  if(debug_level.ge.100) print *, "DEBUG: qv(1,1)      ",qv(1,1)
!  if(debug_level.ge.100) print *, "DEBUG: thetae(1,1)  ",thetae(1,1)
!  if(debug_level.ge.100) print *, "DEBUG: td(1,1)      ",td(1,1)

  ! calculate indexes for pressure
  do i=1,nxp
     x(i)=real(i)
  end do
  do j=1,nyp
     y(j)=real(j)
  end do
  do k=1,nk1
     z(k)=real(k)
  end do

  call calc_press(pressp,pint)

!  call hydro_pres( z, rho, z_ref, p_ref, pressp )

!$omp parallel default(shared)
!$omp do private(i,j,k)
  do k=1,nk1
  do j=1,nyp
  do i=1,nxp
!     pressp()=thetaT_2_P( th(i,j,k), temp(i,j,k) )
     thetaep(i,j,k)=TqvP_2_thetae( tempp(i,j,k),qvp(i,j,k),pressp(k) )
     wspd(i,j,k)=sqrt(uuu(i,j,k)**2+vvv(i,j,k)**2)
     rhop(i,j,k)=TP_2_rho( tempp(i,j,k),pressp(k) )
     ptp(i,j,k)=theta_dry( tempp(i,j,k),pressp(k) )
     esp(i,j,k)=qvP_2_e( qvp(i,j,k),pressp(k) )
     tdp(i,j,k)=es_TD( esp(i,j,k) )
     CALL calc_qflux( pressp(k),qvp(i,j,k),uuu(i,j,k),vvv(i,j,k),qfu(i,j,k),qfv(i,j,k),qfwind(i,j,k) )
     if(uuu(i,j,k).le.-999.) wspd(i,j,k)=-999.
  end do
  end do
  end do
!$omp end do
!$omp end parallel
  if(debug_level.ge.100) print *, "DEBUG: pressp(1)      ",pressp(1)
  if(debug_level.ge.100) print *, "DEBUG: esp(1,1,1)     ",esp(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: qvp(1,1,1)     ",qvp(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: thetaep(1,1,1) ",thetaep(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: wspd(1,1,1)    ",wspd(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: rhop(1,1,1)    ",rhop(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: ptp(1,1,1)     ",ptp(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: tdp(1,1,1)     ",tdp(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: qfu(1,1,1)     ",qfu(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: qfv(1,1,1)     ",qfv(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: qfwind(1,1,1)  ",qfwind(1,1,1)


!$omp parallel default(shared)
!$omp do private(i,j)
  do j=1,nyp
  do i=1,nxp
     ki(i,j)=tempp(i,j,22)+tdp(i,j,22)+tdp(i,j,29)-tempp(i,j,29)-tempp(i,j,36)
     tt(i,j)=tempp(i,j,22)+tdp(i,j,22)-real(2.)*tempp(i,j,36)
     pw(i,j)=precip_water( pressp(:),qvp(i,j,:) )
!     ltemp500(i,j)=moist_laps_temp( pressp(1),tempp(i,j,1),pressp(36) )
!     ssi(i,j)=tempp(i,j,36)-ltemp500(i,j)
     call calc_helicity( 3,37,uuu(i,j,:),vvv(i,j,:),eh(i,j),srh(i,j) )
     call calc_brn( 11, 37, uuu(i,j,:), vvv(i,j,:), cape2d(i,j), brn(i,j) )
     cor(i,j)=real(0.0001)
     if(tempp(i,j,28).le.-999.) ki(i,j)=-999.
     if(tempp(i,j,28).le.-999.) tt(i,j)=-999.
  end do
  end do
!$omp end do
!$omp end parallel
  CALL Ertel_PV( x, y, z, uuu, vvv, www, rhop, ptp, cor, pvp )
  CALL curl_3d( x, y, z, uuu, vvv, www, zeta, eta, xi, -999. )
  if(debug_level.ge.100) print *, "DEBUG: ki(1,1)        ",ki(1,1)
  if(debug_level.ge.100) print *, "DEBUG: tt(1,1)        ",tt(1,1)
  if(debug_level.ge.100) print *, "DEBUG: pw(1,1)        ",pw(1,1)
!  if(debug_level.ge.100) print *, "DEBUG: ssi(1,1)       ",ssi(1,1)
  if(debug_level.ge.100) print *, "DEBUG: eh(1,1)        ",eh(1,1)
  if(debug_level.ge.100) print *, "DEBUG: srh(1,1)       ",srh(1,1)
  if(debug_level.ge.100) print *, "DEBUG: brn(1,1)       ",brn(1,1)
  if(debug_level.ge.100) print *, "DEBUG: pvp(1,1,1)     ",pvp(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: zeta(1,1,1)    ",zeta(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: eta(1,1,1)     ",eta(1,1,1)
  if(debug_level.ge.100) print *, "DEBUG: xi(1,1,1)      ",xi(1,1,1)

!  if(debug_level.ge.100) go to 999

  ! output press [hPa] for surface
  num=20
!  open(unit=num, file="press.bin",form='unformatted',access='direct',recl=nxs*nys*4)
!  write(unit=num,rec=1) press
!  close(unit=num)
!  if(debug_level.ge.100) print *, "DEBUG: Success output press"

  ! output qv [g/kg] for surface
!  open(unit=num, file="qv.bin",form='unformatted',access='direct',recl=nxs*nys*4)
!  write(unit=num,rec=1) qv*real(1000)
!  close(unit=num)
!  if(debug_level.ge.100) print *, "DEBUG: Success output qv"

  ! output thetae [K] for surface
!  open(unit=num, file="thetae.bin",form='unformatted',access='direct',recl=nxs*nys*4)
!  write(unit=num,rec=1) thetae
!  close(unit=num)
!  if(debug_level.ge.100) print *, "DEBUG: Success output thetae"

  ! output td [K] for surface
!  open(unit=num, file="td.bin",form='unformatted',access='direct',recl=nxs*nys*4)
!  write(unit=num,rec=1) td
!  close(unit=num)
!  if(debug_level.ge.100) print *, "DEBUG: Success output thetae"


  ! output ki [C] for pressure
  open(unit=num, file="ki.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) ki-real(273.15)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output ki.bin"

  ! output tt [K] for pressure
  open(unit=num, file="tt.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) tt
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output tt.bin"

  ! output pw [mm] for pressure
  open(unit=num, file="pw.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) pw*real(1.020408)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output pw.bin"

  ! output ehi [m^2/s^2*J/kg] for pressure
  open(unit=num, file="ehi.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) (cape2d*srh)/real(160000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output eh.bin"

  ! output srh [m^2/s^2] for pressure
  open(unit=num, file="srh.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) srh
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output srh.bin"

  ! output brn [-] for pressure
  open(unit=num, file="brn.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) brn
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output brn.bin"

  ! output ssi [K] for pressure
!  open(unit=num, file="ssi.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=num,rec=1) ssi
!  close(unit=num)
!  if(debug_level.ge.100) print *, "DEBUG: Success output ssi.bin"


  ! output qv1000 [g/kg] for pressure
  open(unit=num, file="qv1000.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) qvp(:,:,1)*real(1000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv1000"

  ! output qfwind975 [g/(m^2*s^1)] for pressure
  open(unit=num, file="qfwind975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) qfwind(:,:,6)*real(1000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output qfwind975"

  ! output qfu975 [g/(m^2*s^1)] for pressure
  open(unit=num, file="qfu975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) qfu(:,:,6)*real(1000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output qfu975"

  ! output qfv975 [g/(m^2*s^1)] for pressure
  open(unit=num, file="qfv975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) qfv(:,:,6)*real(1000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output qfv975"

  ! output thetae975 [K] for pressure
  open(unit=num, file="thetae975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) thetaep(:,:,6)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output thetae975"

  ! output qv975 [K] for pressure
  open(unit=num, file="qv975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) qvp(:,:,6)*real(1000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv975"

  ! output u975 [m/s] for pressure
  open(unit=num, file="u975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) uuu(:,:,6)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output u975"

  ! output v975 [m/s] for pressure
  open(unit=num, file="v975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) vvv(:,:,6)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output v975"


  ! output qv950 [g/kg] for pressure
  open(unit=num, file="qv950.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) qvp(:,:,15)*real(1000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv950"

  ! output zeta950 [1/s] for pressure
  open(unit=num, file="zeta950.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) zeta(:,:,11)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output zeta950"

  ! output zeta950 [1/s] for pressure
  open(unit=num, file="eta950.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) eta(:,:,11)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output eta950"

  ! output zeta950 [1/s] for pressure
  open(unit=num, file="xi950.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) xi(:,:,11)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output xi950"

  ! output dbz950 [dBZ] for pressure
  open(unit=num, file="dbz950.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) dbz(:,:,11)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output dbz950"


  ! output qv925 [g/kg] for pressure
  open(unit=num, file="qv925.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) qvp(:,:,15)*real(1000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv925"


  ! output dbz900 [dBZ] for pressure
  open(unit=num, file="dbz900.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) dbz(:,:,18)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output dbz900"


  ! output dbz850 [dBZ] for pressure
  open(unit=num, file="dbz850.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) dbz(:,:,22)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output dbz850"


  ! output dbz800 [dBZ] for pressure
  open(unit=num, file="dbz800.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) dbz(:,:,25)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output dbz800"


  ! output temp700 [C] for pressure
  open(unit=num, file="temp700.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) tempp(:,:,29)-real(273.15)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output temp700"

  ! output qv700 [g/kg] for pressure
  open(unit=num, file="qv700.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) qvp(:,:,29)*real(1000.)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv700"


  ! output pvp500 [PVU] for pressure
  open(unit=num, file="pv500.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) pvp(:,:,36)*real(0.01)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output pv500"

  ! output temp500 [C] for pressure
  open(unit=num, file="temp500.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) tempp(:,:,36)-real(273.15)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output temp500"

  ! output pvp300 [PVU] for pressure
  open(unit=num, file="pv300.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) pvp(:,:,43)*real(0.01)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output pv300"

  ! output wspd250 [m/s] for pressure
  open(unit=num, file="wspd250.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) wspd(:,:,45)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output wspd250"

  ! output pvp200 [PVU] for pressure
  open(unit=num, file="pv200.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=num,rec=1) pvp(:,:,47)*real(0.01)
  close(unit=num)
  if(debug_level.ge.100) print *, "DEBUG: Success output pv200"



!  deallocate( temp,rh,prmsl,press,es,qv,thetae,td,pint )
  deallocate( tempp,hgt,pressp,x,y,z,esp,qvp,thetaep )
  deallocate( uuu,vvv,www,wspd,ptp,rhop,pvp,tdp,ki,tt,pw )
  deallocate( lcl2d,lfc2d,lnb2d,cape2d,cin2d,cor,eh,srh )
  deallocate( ssi,ltemp500,brn,qfu,qfv,qfwind,zeta,eta,xi )
  if(debug_level.ge.100) print *, "DEBUG: Success deallocate all the values"

  if(debug_level.ge.100) print *, "DEBUG: Everything cool !!!"

!999 print *, "debug stop"

  stop
end program calc_index
