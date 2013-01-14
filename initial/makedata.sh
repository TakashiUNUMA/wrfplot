#!/bin/sh

#file="tsukuba_0.1km_initial.nc"
#xnum=499
#ynum=499
#znum=59
#xnum=174
#ynum=179
#file="msm_2.5km_d01_2008-05-13_07:00.nc"
#xnum=450
#ynum=450
file="msm_0.5km_d03_2008-05-13_05:00.nc"
xnum=500
ynum=500
znum=54


cat > namelist.org <<EOF
&param_netcdf3
 fname = FILE
 var   = VAR
 nx    = XNUM
 ny    = YNUM
 nz    = ZNUM
 nt    = 1
/

EOF

for var in U V W QVAPOR tk HGT
do
    sed "s/VAR/${var}/g" namelist.org > namelist.netcdf3
    sed -i "s/FILE/${file}/g" namelist.netcdf3
    sed -i "s/XNUM/${xnum}/g" namelist.netcdf3
    sed -i "s/YNUM/${ynum}/g" namelist.netcdf3
    sed -i "s/ZNUM/${znum}/g" namelist.netcdf3
    ./read_nc3
    
    if test ${var} = "U"
    then
	var="uuu"
    elif test ${var} = "V"
    then
	var="vvv"
    elif test ${var} = "W"
    then
	var="www"
    elif test ${var} = "QVAPOR"
    then
	var="qvp"
    elif test ${var} = "tk"
    then
	var="ttt"
    elif test ${var} = "HGT"
    then
	var="hgt"
    fi
    
    mv output.bin ${var}.bin
    
done

for var in mcape mcin lcl lfc RAINNC
do

    znum=1
    sed "s/VAR/${var}/g" namelist.org > namelist.netcdf3
    sed -i "s/FILE/${file}/g" namelist.netcdf3
    sed -i "s/XNUM/${xnum}/g" namelist.netcdf3
    sed -i "s/YNUM/${ynum}/g" namelist.netcdf3
    sed -i "s/ZNUM/${znum}/g" namelist.netcdf3
    ./read_nc3
    
    if test ${var} = "RAINNC"
    then
	var="rainnc"
    fi
    
    mv output.bin ${var}.bin
    
done

rm -f namelist.org
rm -f namelist.netcdf3
