subroutine calc_brn(kmax1,kmax2,uwnd,vwnd,cape, brn)
  implicit none
  integer :: k,kmax1,kmax2
  real, dimension(54) :: uwnd,vwnd
  real :: u1,u2,v1,v2,cape, brn

  u1=0.
  u2=0.
  v1=0.
  v2=0.

  do k=3,kmax1
     u1=u1+uwnd(k)
     v1=v1+vwnd(k)
  end do

  do k=3,kmax2
     u2=u2+uwnd(k)
     v2=v2+vwnd(k)
  end do

  u1=u1/real(kmax1)
  v1=v1/real(kmax1)
  u2=u2/real(kmax2)
  v2=v2/real(kmax2)

  brn=cape/(real(0.5)*( (u2-u1)**2+(v2-v1)**2 ) )

  if(uwnd(kmax1).le.-999.) brn=-999.

  return
end subroutine calc_brn
