subroutine calc_press(pressp, pint)
  implicit none
  integer :: k
  real, dimension(54) :: pressp,pint

  do k=1,54
     if(k.eq.1) then
        pressp(1)=100000.
        pint(1)=500.
     else if(k.eq.2) then
        pressp(2)=99500.
        pint(2)=500.
     else if(k.eq.3) then
        pressp(3)=99000.
        pint(3)=500.
     else if(k.eq.4) then
        pressp(4)=98500.
        pint(4)=500.
     else if(k.eq.5) then
        pressp(5)=98000.
        pint(5)=500.
     else if(k.eq.6) then
        pressp(6)=97500.
        pint(6)=600.
     else if(k.eq.7) then
        pressp(7)=96900.
        pint(7)=700.
     else if(k.eq.8) then
        pressp(8)=97100.
        pint(8)=700.
     else if(k.eq.9) then
        pressp(9)=96400.
        pint(9)=700.
     else if(k.eq.10) then
        pressp(10)=95700.
        pint(10)=700.
     else if(k.eq.11) then
        pressp(11)=95000.
        pint(11)=500.
     else if(k.eq.12) then
        pressp(12)=94500.
        pint(12)=500.
     else if(k.eq.13) then
        pressp(13)=94000.
        pint(13)=700.
     else if(k.eq.14) then
        pressp(14)=93300.
        pint(14)=800.
     else if(k.eq.15) then
        pressp(15)=92500.
        pint(15)=700.
     else if(k.eq.16) then
        pressp(16)=91800.
        pint(16)=800.
     else if(k.eq.17) then
        pressp(17)=91000.
        pint(17)=1000.
     else if(k.eq.18) then
        pressp(18)=90000.
        pint(18)=1000.
     else if(k.eq.19) then
        pressp(19)=88000.
        pint(19)=1000.
     else if(k.eq.20) then
        pressp(20)=87000.
        pint(20)=1500.
     else if(k.eq.21) then
        pressp(21)=86500.
        pint(21)=1500.
     else if(k.eq.22) then
        pressp(22)=85000.
        pint(22)=1500.
     else if(k.eq.23) then
        pressp(23)=83500.
        pint(23)=2000.
     else if(k.eq.24) then
        pressp(24)=81500.
        pint(24)=1500.
     else if(k.eq.25) then
        pressp(25)=80000.
        pint(25)=2500.
     else if(k.eq.26) then
        pressp(26)=77500.
        pint(26)=2500.
     else if(k.eq.27) then
        pressp(27)=75000.
        pint(27)=2500.
     else if(k.eq.28) then
        pressp(28)=72500.
        pint(28)=2500.
     else if(k.eq.29) then
        pressp(29)=70000.
        pint(29)=4000.
     else if(k.eq.30) then
        pressp(30)=66000.
        pint(30)=3000.
     else if(k.eq.31) then
        pressp(31)=63000.
        pint(31)=3000.
     else if(k.eq.32) then
        pressp(32)=60000.
        pint(32)=2500.
     else if(k.eq.33) then
        pressp(33)=57500.
        pint(33)=2500.
     else if(k.eq.34) then
        pressp(34)=55000.
        pint(34)=2500.
     else if(k.eq.35) then
        pressp(35)=52500.
        pint(35)=2500.
     else if(k.eq.36) then
        pressp(36)=50000.
        pint(36)=3500.
     else if(k.eq.37) then
        pressp(37)=47500.
        pint(37)=3500.
     else if(k.eq.38) then
        pressp(38)=43000.
        pint(38)=3000.
     else if(k.eq.39) then
        pressp(39)=40000.
        pint(39)=2500.
     else if(k.eq.40) then
        pressp(40)=37500.
        pint(40)=2500.
     else if(k.eq.41) then
        pressp(41)=35000.
        pint(41)=2500.
     else if(k.eq.42) then
        pressp(42)=32500.
        pint(42)=2500.
     else if(k.eq.43) then
        pressp(43)=30000.
        pint(43)=2500.
     else if(k.eq.44) then
        pressp(44)=27500.
        pint(44)=2500.
     else if(k.eq.45) then
        pressp(45)=25000.
        pint(45)=5000.
     else if(k.eq.46) then
        pressp(46)=20000.
        pint(46)=5000.
     else if(k.eq.47) then
        pressp(47)=15000.
        pint(47)=5000.
     else if(k.eq.48) then
        pressp(48)=10000.
        pint(48)=1000.
     else if(k.eq.49) then
        pressp(49)=9000.
        pint(49)=1000.
     else if(k.eq.50) then
        pressp(50)=8000.
        pint(50)=1000.
     else if(k.eq.51) then
        pressp(51)=7000.
        pint(51)=1000.
     else if(k.eq.52) then
        pressp(52)=6000.
        pint(52)=1000.
     else if(k.eq.53) then
        pressp(53)=5000.
        pint(53)=1000.
     else if(k.eq.54) then
        pressp(54)=4000.
        pint(54)=50.
     end if
  end do

  return
end subroutine calc_press
