!
! Program of calcucate indexes for WRF-ARW
! produced by Takashi Unuma, Kyoto Univ.
! Last modified: 2013/01/11
!

program calc_index

  use Thermo_Function
  use Thermo_Advanced_Function
  use Thermo_Advanced_Routine

  implicit none
  integer :: i, j, k
  integer :: nxp,nyp,nk1
!  real                                :: nan,undeff,z_ref
  real, dimension(:),     allocatable :: pressp,pint,x,y,z
!  real, dimension(:,:),   allocatable :: temp,rh,prmsl
!  real, dimension(:,:),   allocatable :: press,es,qv,thetae,td
  real, dimension(:,:),   allocatable :: cape2d,cin2d,lcl2d,lfc2d,lnb2d
  real, dimension(:,:),   allocatable :: eh,srh
  real, dimension(:,:),   allocatable :: ltemp500,ssi,brn,cor,ki,tt,pw
  real, dimension(:,:,:), allocatable :: tempp,qvp,uuu,vvv,www,hgt,dbz
  real, dimension(:,:,:), allocatable :: esp,thetaep,wspd
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
  if(debug_level.ge.100) print *, "DEBUG: Success allocate"

  ! read temp [K] for surface
!  open(unit=10, file="temp.bin",form='unformatted',access='direct',status='old',recl=nxs*nys*4)
!  read(unit=10,rec=1) temp
!  close(unit=10)
!  if(debug_level.ge.100) print *, "DEBUG: Success open file of temp"
!  if(debug_level.ge.100) print *, "DEBUG: temp(1,1)    ",temp(1,1)

  ! read rh [%] for surface
!  open(unit=11, file="rh.bin",form='unformatted',access='direct',status='old',recl=nxs*nys*4)
!  read(unit=11,rec=1) rh
!  close(unit=11)
!  if(debug_level.ge.100) print *, "DEBUG: Success open file of rh"
!  if(debug_level.ge.100) print *, "DEBUG: rh(1,1)      ",rh(1,1)

  ! read prmsl [Pa] for surface
!  open(unit=12, file="prmsl.bin",form='unformatted',access='direct',status='old',recl=nxs*nys*4)
!  read(unit=12,rec=1) prmsl
!  close(unit=12)
!  if(debug_level.ge.100) print *, "DEBUG: Success open file of prmsl"
!  if(debug_level.ge.100) print *, "DEBUG: prmsl(1,1)   ",prmsl(1,1)


  ! read temp [K] for pressure
  open(unit=13, file="ttt.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=13,rec=1) tempp
  close(unit=13)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of tempp"
  if(debug_level.ge.100) print *, "DEBUG: tempp(1,1,1) ",tempp(1,1,1)

  ! read qv [kg/kg] for pressure
  open(unit=14, file="qvp.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=14,rec=1) qvp
  close(unit=14)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of qvp"
  if(debug_level.ge.100) print *, "DEBUG: qvp(1,1,1)   ",qvp(1,1,1)

  ! read hgt [m] for pressure
  open(unit=15, file="hgt.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=15,rec=1) hgt
  close(unit=15)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of hgt"
  if(debug_level.ge.100) print *, "DEBUG: hgt(1,1,1)   ",hgt(1,1,1)

  ! read uuu [m/s] for pressure
  open(unit=16, file="uuu.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=16,rec=1) uuu
  close(unit=16)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of uuu"
  if(debug_level.ge.100) print *, "DEBUG: uuu(1,1,1)   ",uuu(1,1,1)

  ! read vvv [m/s] for pressure
  open(unit=17, file="vvv.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=17,rec=1) vvv
  close(unit=17)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of vvv"
  if(debug_level.ge.100) print *, "DEBUG: vvv(1,1,1)   ",vvv(1,1,1)

  ! read www [m/s] for pressure
  open(unit=18, file="www.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=18,rec=1) www
  close(unit=18)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of www"
  if(debug_level.ge.100) print *, "DEBUG: www(1,1,1)   ",www(1,1,1)

  ! read cape [J/kg] for pressure
  open(unit=19, file="mcape.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*4)
  read(unit=19,rec=1) cape2d
  close(unit=19)
  if(debug_level.ge.100) print *, "DEBUG: Success open file of cape"
  if(debug_level.ge.100) print *, "DEBUG: cape(1,1)    ",cape2d(1,1)

  ! read dbz [dBZ] for pressure
  open(unit=20, file="dbz.bin",form='unformatted',access='direct',status='old',recl=nxp*nyp*nk1*4)
  read(unit=20,rec=1) dbz
  close(unit=20)
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
     call calc_qflux( pressp(k),qvp(i,j,k),uuu(i,j,k),vvv(i,j,k),qfu(i,j,k),qfv(i,j,k),qfwind(i,j,k) )
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
!     ltemp500(i,j)=moist_laps_temp( pressp(1),tempp(i,j,1),pressp(45) )
!     ssi(i,j)=tempp(i,j,45)-ltemp500(i,j)
     call calc_helicity( 3,37,uuu(i,j,:),vvv(i,j,:),eh(i,j),srh(i,j) )
     call calc_brn( 11, 37, uuu(i,j,:), vvv(i,j,:), cape2d(i,j), brn(i,j) )
!     cor(i,j)=real(0.0001)
     if(tempp(i,j,28).le.-999.) ki(i,j)=-999.
     if(tempp(i,j,28).le.-999.) tt(i,j)=-999.
  end do
  end do
!$omp end do
!$omp end parallel
!  call Ertel_PV( x, y, z, uuu, vvv, www, rhop, ptp, cor, pvp )
  if(debug_level.ge.100) print *, "DEBUG: ki(1,1)        ",ki(1,1)
  if(debug_level.ge.100) print *, "DEBUG: tt(1,1)        ",tt(1,1)
  if(debug_level.ge.100) print *, "DEBUG: pw(1,1)        ",pw(1,1)
!  if(debug_level.ge.100) print *, "DEBUG: ssi(1,1)       ",ssi(1,1)
  if(debug_level.ge.100) print *, "DEBUG: eh(1,1)        ",eh(1,1)
  if(debug_level.ge.100) print *, "DEBUG: srh(1,1)       ",srh(1,1)
  if(debug_level.ge.100) print *, "DEBUG: brn(1,1)       ",brn(1,1)
!  if(debug_level.ge.100) print *, "DEBUG: pvp(1,1,1)     ",pvp(1,1,1)

!  if(debug_level.ge.100) go to 999

  ! output press [hPa] for surface
!  open(unit=20, file="press.bin",form='unformatted',access='direct',recl=nxs*nys*4)
!  write(unit=20,rec=1) press
!  close(unit=20)
!  if(debug_level.ge.100) print *, "DEBUG: Success output press"

  ! output qv [g/kg] for surface
!  open(unit=21, file="qv.bin",form='unformatted',access='direct',recl=nxs*nys*4)
!  write(unit=21,rec=1) qv*real(1000)
!  close(unit=21)
!  if(debug_level.ge.100) print *, "DEBUG: Success output qv"

  ! output thetae [K] for surface
!  open(unit=22, file="thetae.bin",form='unformatted',access='direct',recl=nxs*nys*4)
!  write(unit=22,rec=1) thetae
!  close(unit=22)
!  if(debug_level.ge.100) print *, "DEBUG: Success output thetae"

  ! output td [K] for surface
!  open(unit=23, file="td.bin",form='unformatted',access='direct',recl=nxs*nys*4)
!  write(unit=23,rec=1) td
!  close(unit=23)
!  if(debug_level.ge.100) print *, "DEBUG: Success output thetae"


  ! output cape2d [J/kg] for pressure
!  open(unit=30, file="cape.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=30,rec=1) cape2d
!  close(unit=30)
!  if(debug_level.ge.100) print *, "DEBUG: Success output cape2d"

  ! output cin2d [J/kg] for pressure
!  open(unit=31, file="cin.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=31,rec=1) real(-1.)*cin2d
!  close(unit=31)
!  if(debug_level.ge.100) print *, "DEBUG: Success output cin2d"

  ! output lcl2d [m] for pressure
!  open(unit=32, file="lcl.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=32,rec=1) lcl2d
!  close(unit=32)
!  if(debug_level.ge.100) print *, "DEBUG: Success output lcl2d"

  ! output lfc2d [m] for pressure
!  open(unit=33, file="lfc.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=33,rec=1) lfc2d
!  close(unit=33)
!  if(debug_level.ge.100) print *, "DEBUG: Success output lfc2d"

  ! output lnb2d [m] for pressure
!  open(unit=34, file="lnb.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=34,rec=1) lnb2d
!  close(unit=34)
!  if(debug_level.ge.100) print *, "DEBUG: Success output lnb2d"

  ! output temp500 [K] for pressure
  open(unit=35, file="temp500.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=35,rec=1) tempp(:,:,36)-real(273.15)
  close(unit=35)
  if(debug_level.ge.100) print *, "DEBUG: Success output temp500"

  ! output thetae975 [K] for pressure
  open(unit=36, file="thetae975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=36,rec=1) thetaep(:,:,6)
  close(unit=36)
  if(debug_level.ge.100) print *, "DEBUG: Success output thetae975"

  ! output wspd250 [m/s] for pressure
  open(unit=37, file="wspd250.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=37,rec=1) wspd(:,:,45)
  close(unit=37)
  if(debug_level.ge.100) print *, "DEBUG: Success output wspd250"

  ! output qv975 [K] for pressure
  open(unit=38, file="qv975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=38,rec=1) qvp(:,:,6)*real(1000.)
  close(unit=38)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv975"

  ! output pvp200 [K] for pressure
!  open(unit=39, file="pv200.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=39,rec=1) pvp(:,:,14)*real(0.01)
!  close(unit=39)
!  if(debug_level.ge.100) print *, "DEBUG: Success output pv200.bin"

  ! output pvp300 [K] for pressure
!  open(unit=40, file="pv300.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=40,rec=1) pvp(:,:,12)*real(0.01)
!  close(unit=40)
!  if(debug_level.ge.100) print *, "DEBUG: Success output pv300.bin"

  ! output pvp500 [K] for pressure
!  open(unit=41, file="pv500.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=41,rec=1) pvp(:,:,10)*real(0.01)
!  close(unit=41)
!  if(debug_level.ge.100) print *, "DEBUG: Success output pv500.bin"

  ! output ki [C] for pressure
  open(unit=42, file="ki.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=42,rec=1) ki-real(273.15)
  close(unit=42)
  if(debug_level.ge.100) print *, "DEBUG: Success output ki.bin"

  ! output tt [K] for pressure
  open(unit=43, file="tt.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=43,rec=1) tt
  close(unit=43)
  if(debug_level.ge.100) print *, "DEBUG: Success output tt.bin"

  ! output pw [mm] for pressure
  open(unit=44, file="pw.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=44,rec=1) pw*real(1.020408)
  close(unit=44)
  if(debug_level.ge.100) print *, "DEBUG: Success output pw.bin"

  ! output ehi [m^2/s^2*J/kg] for pressure
  open(unit=45, file="ehi.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=45,rec=1) (cape2d*srh)/real(160000.)
  close(unit=45)
  if(debug_level.ge.100) print *, "DEBUG: Success output eh.bin"

  ! output srh [m^2/s^2] for pressure
  open(unit=46, file="srh.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=46,rec=1) srh
  close(unit=46)
  if(debug_level.ge.100) print *, "DEBUG: Success output srh.bin"

  ! output brn [-] for pressure
  open(unit=47, file="brn.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=47,rec=1) brn
  close(unit=47)
  if(debug_level.ge.100) print *, "DEBUG: Success output brn.bin"

  ! output ssi [K] for pressure
!  open(unit=48, file="ssi.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
!  write(unit=48,rec=1) ssi
!  close(unit=48)
!  if(debug_level.ge.100) print *, "DEBUG: Success output ssi.bin"

  ! output temp700 [C] for pressure
  open(unit=49, file="temp700.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=49,rec=1) tempp(:,:,29)-real(273.15)
  close(unit=49)
  if(debug_level.ge.100) print *, "DEBUG: Success output temp700"

  ! output qv700 [g/kg] for pressure
  open(unit=50, file="qv700.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=50,rec=1) qvp(:,:,29)*real(1000.)
  close(unit=50)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv700"

  ! output qfwind975 [g/(m^2*s^1)] for pressure
  open(unit=51, file="qfwind975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=51,rec=1) qfwind(:,:,6)*real(1000.)
  close(unit=51)
  if(debug_level.ge.100) print *, "DEBUG: Success output qfwind975"

  ! output qfu975 [g/(m^2*s^1)] for pressure
  open(unit=52, file="qfu975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=52,rec=1) qfu(:,:,6)*real(1000.)
  close(unit=52)
  if(debug_level.ge.100) print *, "DEBUG: Success output qfu975"

  ! output qfv975 [g/(m^2*s^1)] for pressure
  open(unit=53, file="qfv975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=53,rec=1) qfv(:,:,6)*real(1000.)
  close(unit=53)
  if(debug_level.ge.100) print *, "DEBUG: Success output qfv975"

  ! output qv1000 [g/kg] for pressure
  open(unit=54, file="qv1000.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=54,rec=1) qvp(:,:,1)*real(1000.)
  close(unit=54)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv1000"

  ! output qv950 [g/kg] for pressure
  open(unit=55, file="qv950.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=55,rec=1) qvp(:,:,15)*real(1000.)
  close(unit=55)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv950"

  ! output qv925 [g/kg] for pressure
  open(unit=56, file="qv925.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=56,rec=1) qvp(:,:,15)*real(1000.)
  close(unit=56)
  if(debug_level.ge.100) print *, "DEBUG: Success output qv925"

  ! output u975 [m/s] for pressure
  open(unit=57, file="u975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=57,rec=1) uuu(:,:,6)
  close(unit=57)
  if(debug_level.ge.100) print *, "DEBUG: Success output u975"

  ! output v975 [m/s] for pressure
  open(unit=58, file="v975.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=58,rec=1) vvv(:,:,6)
  close(unit=58)
  if(debug_level.ge.100) print *, "DEBUG: Success output v975"

  ! output dbz800 [m/s] for pressure
  open(unit=59, file="dbz800.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=59,rec=1) dbz(:,:,25)
  close(unit=59)
  if(debug_level.ge.100) print *, "DEBUG: Success output dbz800"

  ! output dbz850 [m/s] for pressure
  open(unit=59, file="dbz850.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=59,rec=1) dbz(:,:,22)
  close(unit=59)
  if(debug_level.ge.100) print *, "DEBUG: Success output dbz850"

  ! output dbz900 [m/s] for pressure
  open(unit=59, file="dbz900.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=59,rec=1) dbz(:,:,18)
  close(unit=59)
  if(debug_level.ge.100) print *, "DEBUG: Success output dbz900"

  ! output dbz950 [m/s] for pressure
  open(unit=59, file="dbz950.bin",form='unformatted',access='direct',recl=nxp*nyp*4)
  write(unit=59,rec=1) dbz(:,:,11)
  close(unit=59)
  if(debug_level.ge.100) print *, "DEBUG: Success output dbz950"

!  deallocate( temp,rh,prmsl,press,es,qv,thetae,td,pint )
!  deallocate( tempp,rhp,hgt,pressp,x,y,z,esp,qvp,thetaep )
!  deallocate( uuu,vvv,www,wspd,ptp,rhop,pvp,tdp,ki,tt,pw )
!  deallocate( lcl2d,lfc2d,lnb2d,cape2d,cin2d,cor,eh,srh )
!  deallocate( ssi,ltemp500,brn,qfu,qfv,qfwind )
!  if(debug_level.ge.100) print *, "DEBUG: Success deallocate all the values"

  if(debug_level.ge.100) print *, "DEBUG: Everything cool !!!"

!999 print *, "debug stop"

  stop
end program calc_index
