ccccc6cccccccccccccccccccccccccccccccccccccccccccc
c
c     Calculate qvapor fluxs
c     Producted by Fumie Murata and Yuka Shiozaki
c     Modified by Takashi Unuma, Kochi Univ.
c     Last update: 2011/11/26
c
cccccccccccccccccccccccccccccccccccccccccccccccccc

      program calc_qflux

      integer NLON              !" Number of x grids
      integer NLAT              !" Number of y grids
      parameter(NLON=41)        !" Set x grid num
      parameter(NLAT=41)        !" Set y grid num
      real*8 temp(NLON,NLAT)    !" Temperture [deg C]
      real*8 rh(NLON,NLAT)      !" Rel. Humid. [%]
      real*8 u(NLON,NLAT)       !" Zonal Wind [m/s]
      real*8 v(NLON,NLAT)       !" Meridional Wind [m/s]
      real*8 fu(NLON,NLAT)      !" Flux of u [g*m/s]
      real*8 fv(NLON,NLAT)      !" Flux of v [g*m/s]
      real*8 tfu(NLON,NLAT)     !" Int. Flux of u [g*m/s]
      real*8 tfv(NLON,NLAT)     !" Int. Flux of u [g*m/s]
      real*8 press              !" Pressure [hPa]
      real*8 pp                 !" Press. for flux [hPa]

      integer i                 !" For x-grids
      integer j                 !" For y-grids
      integer k                 !" For z-grids

c     Initialization
      do j= 1, NLAT
         do i= 1, NLON
            tfu(i,j) = 0.
            tfv(i,j) = 0.
         end do
      end do

c     Open file
c      open(10,
c     &     file='./grads.fwrite_1000',
c     &     form='unformatted',
c     &     access='direct',
c     &     recl=NLON*NLAT*4)
c      read(10,rec=1) temp
c      read(10,rec=2) rh
c      read(10,rec=3) u
c      read(10,rec=4) v
c      close(10)

c     Calcuate Qfluxs of each levels and Integration
      do k = 1, 13              !" 1000~275 [hPa]
         call calcpress(k, pp, press)
         do j = 1, NLAT
            do i = 1, NLON
               call calfwind(press, temp(i,j), 
     &              rh(i,j), u(i,j), fu(i,j))
               call calfwind(press, temp(i,j),
     &              rh(i,j), v(i,j), fv(i,j)) 
               tfu(i,j) = tfu(i,j) + fu(i,j) * pp
               tfv(i,j) = tfv(i,j) + fv(i,j) * pp
            end do
         end do
      end do

c     Calclate Total Qvapor Fluxs each grids
      do j = 1, NLAT
         do i = 1, NLON
            tfu(i,j) = tfu(i,j) / real(9.8)
            tfv(i,j) = tfv(i,j) / real(9.8)
         end do
      end do

c      open(11,
c     &     file='./data.out',
c     &     form='unformatted',
c     &     access='direct',
c     &     recl=NLON*NLAT*4)
c      write(11,rec=1) tfu     
c      write(11,rec=2) tfv
c      close(11)

      stop
      end program calc_qflux


c     ------------------------------------------------
      subroutine calcpress(k, pp, press)
c     ------------------------------------------------
c     k       in   vertical grid          [ - ]
c     pp      out  for calculating flux   [hPa]
c     press   out  pressure               [hPa]
      
      integer k
      real*8 pp
      real*8 press

      if(k.eq.1) then
         press = 1000.
         pp = 25.
      else if(k.eq.2) then
         press = 975.
         pp = 25.
      else if(k.eq.3) then
         press = 950.
         pp = 25.
      else if(k.eq.4) then
         press = 925.
         pp = 25.
      else if(k.eq.5) then
         press = 900.
         pp = 25.
      else if(k.eq.6) then
         press = 850.
         pp = 50.
      else if(k.eq.7) then
         press = 800.
         pp = 100.
      else if(k.eq.8) then
         press = 700.
         pp = 100.
      else if(k.eq.9) then
         press = 600.
         pp = 100.
      else if(k.eq.10) then
         press = 500.
         pp = 100.
      else if(k.eq.11) then
         press = 400.
         pp = 100.
      else if(k.eq.12) then
         press = 300.
         pp = 100.
      else if(k.eq.13) then
         press = 250.
         pp = 25.
      else if(k.eq.14) then
         press = 200.
         pp = 0.
      else if(k.eq.15) then
         press = 150.
         pp = 0.
      else if(k.eq.16) then
         press = 100.
         pp = 0.
      end if
      
      return
      end subroutine calcpress

c     ------------------------------------------------
      subroutine calfwind(press, temp ,rh , wind, fwind)
c     ------------------------------------------------
c     press   in   pressure               [hPa]
c     temp    in   temperature            [K]
c     rh      in   relative humidity      [%]
c     wind    in   wind                   [m/s]
c     fwind   out  saturated mixing ratio [g*m/s]
      
      real*8 Epsiron
      real*8 es
      real*8 e
      real*8 rh
      real*8 temp
      real*8 qv
      real*8 press
      real*8 wind
      real*8 fwind
      parameter(Epsiron = 0.622)

      es = 6.11*exp(17.27*(temp-273.15)/(temp-35.86))
      e = rh /100. * es
      qv = Epsiron * e / (press - e)
      fwind = wind * qv

      return 
      end subroutine calfwind
