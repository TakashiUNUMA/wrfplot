subroutine calc_helicity(kmin,kmax,uwnd,vwnd, eh,srh)
  implicit none

  integer :: k,kmin,kmax,count
  real, dimension(54) :: uwnd,vwnd
  real :: sumu,sumv,meanu,meanv
  real :: eh,srh
  real :: du,dv,ubar,vbar,ehu,ehv,srhu,srhv

!  print *, "Now, calculate Storm wind"

  sumu=0.
  sumv=0.
  count=0
  do k=kmin, kmax
     if ((uwnd(k).gt.-900.).and.(vwnd(k).gt.-900.)) then
        sumu=sumu+uwnd(k)
        sumv=sumv+vwnd(k)
        count=count+1
     end if
  end do
  meanu=sumu/count
  meanv=sumv/count
    
!  print *, "Now, calculate SR and Environmental Helicity"
  
  eh=0.
  srh=0.
  do k=kmin+1, kmax
     if ((uwnd(k).gt.-900.).and.(vwnd(k).gt.-900.)) then
        du=uwnd(k)-uwnd(k-1)
        dv=vwnd(k)-vwnd(k-1)
        ubar=real(0.5)*(uwnd(k)+uwnd(k-1))
        vbar=real(0.5)*(vwnd(k)+vwnd(k-1))
        ehu=-dv*ubar
        ehv=du*vbar
        eh=eh+ehu+ehv
        srhu=-dv*(ubar-meanu)
        srhv=du*(vbar-meanv)
        srh=srh+srhu+srhv
     else
        eh=-999.
        srh=-999.
     end if
  end do

  if(uwnd(kmax).le.-999.) eh=-999.
  if(uwnd(kmax).le.-999.) srh=-999.

  return
end subroutine calc_helicity
