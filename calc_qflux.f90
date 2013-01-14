subroutine calc_qflux( pressp, qvp, uuu, vvv, qfu, qfv, qfwind )
  implicit none

  real :: pressp,qvp,uuu,vvv
  real :: qfu,qfv,qfwind

  qfu=(1/real(9.81))*qvp*uuu*((100000.-pressp)*real(0.01))
  qfv=(1/real(9.81))*qvp*vvv*((100000.-pressp)*real(0.01))

  qfwind=sqrt(qfu**2+qfv**2)

  if(uuu.le.-999.) qfu=-999.
  if(vvv.le.-999.) qfv=-999.
  if(uuu.le.-999.) qfwind=-999.

  return
end subroutine calc_qflux
