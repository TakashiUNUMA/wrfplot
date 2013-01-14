#!/bin/sh
#
# wrfplot.sh
#
# produced by Takashi Unuma, Kyoto Univ.
# Last modified: 2013/01/10
#

### for debug
debug_level=100

### for picture option
picopt=1

### for draw range 
# default
#dRANGE="1/174/1/179";dPRJ="x0.0228";vINT=20
#dRANGE="1/450/1/450";dPRJ="x0.0088";vINT=20
dRANGE="1/499/1/499";dPRJ="x0.008";vINT=25
#dRANGE="120/150/22.4/47.6";dPRJ="m0.131";vINT=2
# case 20120506
#dRANGE="135.0/145.0/31.0/39.5";dPRJ="m0.3925";vINT=1
# case 20080513
#dRANGE="132/135/32/35";dPRJ="m1.31";vINT=0.2


############################################################
# executable section
############################################################
#source ~/.bashrc
#alias rm="rm"
#WGRIB2=/usr/local/grads-2.0.a9/bin/wgrib2
#export PATH="/home/unuma/usr/local/gmt-4.5.3.jp/bin:${PATH}"
#export PATH="/home/unuma/usr/local/bin:${PATH}"

#wwwdir="/home/unuma/www"

echo "Start Time = `date +%F_%T`"
 
#export LANG=en_US
#date=$(TZ=JST-9 date +%Y%m%d)
#hh=$(TZ=JST-9 date +%H)

if test $# -eq 1
then
    input=$1
    date=${input:0:8}
    hh=${input:8:2}
    echo "Time: ${date}${hh} (UTC)"
fi

yyyy=${date:0:4}
mm=${date:4:2}
dd=${date:6:2}

if test ${debug_level} -ge 100
then
    echo "DEBUG MODE"
    ./calc_index
else
    ./calc_index > wrfindex.log 2>&1
fi


#RANGE="125/146.75/25.6/47.975"
#PRJ="m0.131"
#INT="0.125/0.125"
#RANGE="1/174/1/179"
#RANGE="1/450/1/450"
RANGE="1/499/1/499"
INT="1"
if test -s "ttt.bin"
then
    # for surface data
#    xyz2grd temp.bin   -Gtemp.grd   -R${RANGE} -I${INT0} -ZTLf
#    xyz2grd u10.bin    -Gu10.grd    -R${RANGE} -I${INT0} -ZTLf
#    xyz2grd v10.bin    -Gv10.grd    -R${RANGE} -I${INT0} -ZTLf
#    xyz2grd press.bin  -Gpress.grd  -R${RANGE} -I${INT0} -ZTLf
#    xyz2grd qv.bin     -Gqv.grd     -R${RANGE} -I${INT0} -ZTLf
#    xyz2grd thetae.bin -Gthetae.grd -R${RANGE} -I${INT0} -ZTLf
#    xyz2grd td.bin     -Gtd.grd     -R${RANGE} -I${INT0} -ZTLf

    # for pressure data
    xyz2grd mcape.bin       -Gmcape.grd       -R${RANGE} -I${INT} -ZTLf
    xyz2grd mcin.bin        -Gmcin.grd        -R${RANGE} -I${INT} -ZTLf
    xyz2grd lcl.bin         -Glcl.grd         -R${RANGE} -I${INT} -ZTLf
    xyz2grd lfc.bin         -Glfc.grd         -R${RANGE} -I${INT} -ZTLf
    xyz2grd pw.bin          -Gpw.grd          -R${RANGE} -I${INT} -ZTLf
    xyz2grd ki.bin          -Gki.grd          -R${RANGE} -I${INT} -ZTLf
    xyz2grd tt.bin          -Gtt.grd          -R${RANGE} -I${INT} -ZTLf
    xyz2grd ehi.bin         -Gehi.grd         -R${RANGE} -I${INT} -ZTLf
    xyz2grd srh.bin         -Gsrh.grd         -R${RANGE} -I${INT} -ZTLf
#    xyz2grd ssi.bin         -Gssi.grd         -R${RANGE} -I${INT} -ZTLf
    xyz2grd brn.bin         -Gbrn.grd         -R${RANGE} -I${INT} -ZTLf
    xyz2grd u975.bin        -Gu975.grd        -R${RANGE} -I${INT} -ZTLf
    xyz2grd v975.bin        -Gv975.grd        -R${RANGE} -I${INT} -ZTLf
    xyz2grd qv1000.bin      -Gqv1000.grd      -R${RANGE} -I${INT} -ZTLf
    xyz2grd qv975.bin       -Gqv975.grd       -R${RANGE} -I${INT} -ZTLf
    xyz2grd qv950.bin       -Gqv950.grd       -R${RANGE} -I${INT} -ZTLf
    xyz2grd qv925.bin       -Gqv925.grd       -R${RANGE} -I${INT} -ZTLf
    xyz2grd qfu975.bin      -Gqfu975.grd      -R${RANGE} -I${INT} -ZTLf
    xyz2grd qfv975.bin      -Gqfv975.grd      -R${RANGE} -I${INT} -ZTLf
    xyz2grd qfwind975.bin   -Gqfwind975.grd   -R${RANGE} -I${INT} -ZTLf
    xyz2grd thetae975.bin   -Gthetae975.grd   -R${RANGE} -I${INT} -ZTLf
    xyz2grd qv700.bin       -Gqv700.grd       -R${RANGE} -I${INT} -ZTLf
    xyz2grd temp700.bin     -Gtemp700.grd     -R${RANGE} -I${INT} -ZTLf
    xyz2grd temp500.bin     -Gtemp500.grd     -R${RANGE} -I${INT} -ZTLf
#    xyz2grd pv500.bin       -Gpv500.grd       -R${RANGE} -I${INT} -ZTLf
#    xyz2grd pv300.bin       -Gpv300.grd       -R${RANGE} -I${INT} -ZTLf
#    xyz2grd pv200.bin       -Gpv200.grd       -R${RANGE} -I${INT} -ZTLf
#    xyz2grd wspd250.bin     -Gwspd250.grd     -R${RANGE} -I${INT} -ZTLf
#    xyz2grd wspd975.bin     -Gwspd975.grd     -R${RANGE} -I${INT} -ZTLf

else
    echo "fail xyz2grd section"
    exit 1
fi

wind=0
for file in mcape mcin lcl lfc pw ki tt ehi srh brn thetae975 qfwind975 qv1000 qv975 qv950 qv925 temp700 qv700 temp500
do
    rm -f .gmt*
    gmtdefaults -D > .gmtdefaults4
    gmtset HEADER_FONT_SIZE       6p
    gmtset LABEL_FONT_SIZE        6p
    gmtset ANOT_FONT_SIZE         5p
    gmtset ANNOT_OFFSET_PRIMARY 0.03c
    gmtset BASEMAP_TYPE        plain
    gmtset TICK_LENGTH        -0.10c
    gmtset FRAME_PEN           0.20p
    gmtset GRID_PEN            0.20p
    gmtset TICK_PEN            0.25p
    gmtset MEASURE_UNIT           cm
    gmtset PAPER_MEDIA            a4
    gmtset VECTOR_SHAPE            2
    
    gmtsta='-P -K'
    gmtcon='-P -K -O'
    gmtend='-P -O'
    
    if test ${picopt} -eq 1
    then
	ymdh=$(/home/unuma/usr/local/bin/utc2jst ${yyyy}${mm}${dd}${hh}00 | cut -c1-10)
	Y=${ymdh:0:4}
	m=${ymdh:4:2}
	M=$(/home/unuma/usr/local/bin/mm2MMM ${m})
	D=${ymdh:6:2}
	H=${ymdh:8:2}
	psfile=${file}_${Y}${m}${D}${H}.ps
    else
	psfile=${file}.ps
    fi

    if test ${file} = "prmsl"
    then
	unucpt prmsl 99200 104800 400
	unit="[Pa]"
	wind=0
    elif test ${file} = "press"
    then
	unucpt press 992 1048 4
	unit="[hPa]"
	wind=0
    elif test ${file} = "temp"
    then
	unucpt temp 232 312 3
	scaleopt="a30f3"
	unit="[K]"
	wind=0
    elif test ${file} = "temp500"
    then
	unucpt temp500 -53 0 3
	scaleopt="a30f3"
	unit="[C]"
	wind=0
    elif test ${file} = "temp700"
    then
	unucpt temp700 -44 9 3
	scaleopt="a30f3"
	unit="[K]"
	wind=0
    elif test ${file} = "td"
    then
	unucpt td 232 312 3
	scaleopt="a30f3"
	unit="[K]"
	wind=0
    elif test ${file} = "thetae"
    then
	unucpt thetae 270 330 3
	scaleopt="a30f3"
	unit="[K]"
	wind=1
    elif test ${file} = "thetae975"
    then
	unucpt thetae975 240 330 6
	scaleopt="a30f3"
	unit="[K]"
	wind=2
    elif test ${file} = "rh"
    then
	unucpt rh 0 100 10
	scaleopt="a20f10"
	unit="[%]"
	wind=0
    elif test ${file} = "qv"
    then
	unucpt qv 0 20 2
	scaleopt="a4f2"
	unit="[g/kg]"
	wind=1
    elif test ${file} = "qv1000"
    then
	unucpt qv1000 0 20 2
	scaleopt="a4f2"
	unit="[g/kg]"
	wind=0
    elif test ${file} = "qv975"
    then
	unucpt qv975 0 20 2
	scaleopt="a4f2"
	unit="[g/kg]"
	wind=2
    elif test ${file} = "qv950"
    then
	unucpt qv950 0 20 2
	scaleopt="a4f2"
	unit="[g/kg]"
	wind=0
    elif test ${file} = "qv925"
    then
	unucpt qv925 0 20 2
	scaleopt="a4f2"
	unit="[g/kg]"
	wind=0
    elif test ${file} = "qv700"
    then
	unucpt qv700 0 20 2
	scaleopt="a4f2"
	unit="[g/kg]"
	wind=0
    elif test ${file} = "mcape"
    then
	unucpt mcape 0 1000 100
	scaleopt="a200f100"
	unit="[J/kg]"
	wind=0
    elif test ${file} = "mcin"
    then
	unucpt mcin 0 250 25
	scaleopt="a50f25"
	unit="[J/kg]"
	wind=0
    elif test ${file} = "lcl"
    then
	unucpt lcl 500 2000 250
	scaleopt="a500f250"
	unit="[m]"
	wind=0
    elif test ${file} = "lfc"
    then
	unucpt lfc 500 5000 250
	scaleopt="a1000f250"
	unit="[m]"
	wind=0
    elif test ${file} = "lnb"
    then
	unucpt lnb 500 10000 500
	scaleopt="a2500f500"
	unit="[m]"
	wind=0
    elif test ${file} = "wspd250"
    then
	unucpt wspd250 0 100 10
	scaleopt="a20f10"
	unit="[m/s]"
	wind=0
    elif test ${file} = "pv200"
    then
	unucpt pv200 0 5 0.5
	scaleopt="a1f0.5"
	unit="[PVU]"
	wind=0
    elif test ${file} = "pv300"
    then
	unucpt pv300 0 5 0.5
	scaleopt="a1f0.5"
	unit="[PVU]"
	wind=0
    elif test ${file} = "pv500"
    then
	unucpt pv500 0 5 0.5
	scaleopt="a1f0.5"
	unit="[PVU]"
	wind=0
    elif test ${file} = "pw"
    then
	unucpt pw 0 100 10
	scaleopt="a10f5"
	unit="[mm]"
	wind=0
    elif test ${file} = "ki"
    then
	unucpt ki 0 60 5
	scaleopt="a10f5"
	unit="[C]"
	wind=0
    elif test ${file} = "tt"
    then
	unucpt tt 0 60 5
	scaleopt="a10f5"
	unit="[K]"
	wind=0
    elif test ${file} = "ehi"
    then
	unucpt ehi 0 3 0.2
	scaleopt="a1f0.2"
	unit="[m^2/s^2*J/kg]"
	wind=0
    elif test ${file} = "srh"
    then
	unucpt srh 25 350 25
	scaleopt="a50f25"
	unit="[m^2/s^2]"
	wind=0
    elif test ${file} = "ssi"
    then
	unucpt ssi -9 9 3
	scaleopt="a3f1"
	unit="[K]"
	wind=0
    elif test ${file} = "brn"
    then
	unucpt brn 0 60 5
	scaleopt="a10f5"
	unit="[-]"
	wind=0
    elif test ${file} = "qfwind975"
    then
	unucpt qfwind975 0 500 50
	scaleopt="a100f50"
	unit="[g/(m^2*s)]"
	wind=3
    else
	echo "Not supported"
	exit 1
    fi

    if test ${file} = "prmsl" -o ${file} = "press"
    then
        # grdcontour
	grdcontour ${file}.grd -J${dPRJ} -R${dRANGE} -Ba50WSne -W0.25,255/0/0 -A4tf3 -Ccpalet_${file}.cpt -X1.0 -Y1.0 ${gmtsta} > ${psfile}

        # pscoast
#	pscoast -J${dPRJ} -R${dRANGE} -W1 -A10 -Di ${gmtcon} >> ${psfile}
    elif test ${file} = "srh" -o ${file} = "brn"
    then
	# grdimage
	grdimage ${file}.grd -J${dPRJ} -R${dRANGE} -Ba50WSne -Ccpalet_${file}.cpt -X1.0 -Y1.0 ${gmtsta} > ${psfile}

        # pscoast
#	pscoast -J${dPRJ} -R${dRANGE} -W1 -A10 -Di -W0.5,128/128/128 ${gmtcon} >> ${psfile}

	# psscale
	gmtset FRAME_PEN 0.10p
	gmtset GRID_PEN  0.10p
	gmtset TICK_PEN  0.10p
	gmtset ANOT_FONT_SIZE 3p
	psscale -D1.0/-0.3/2.0/0.1h -Ccpalet_${file}.cpt -B${scaleopt}/:"${unit}": ${gmtcon} >> ${psfile}
    else
        # grdimage
	grdimage ${file}.grd -J${dPRJ} -R${dRANGE} -Ba50WSne -Ccpalet_${file}.cpt -X1.0 -Y1.0 ${gmtsta} > ${psfile}

        # grdcontour
	grdcontour ${file}.grd -J${dPRJ} -R${dRANGE} -W0.25,0/0/0 -A- -Ccpalet_${file}.cpt ${gmtcon} >> ${psfile}

        # pscoast
#	pscoast -J${dPRJ} -R${dRANGE} -W1 -A10 -Di -W0.5,128/128/128 ${gmtcon} >> ${psfile}

	# psscale
	gmtset FRAME_PEN 0.10p
	gmtset GRID_PEN  0.10p
	gmtset TICK_PEN  0.10p
	gmtset ANOT_FONT_SIZE 3p
	psscale -D1.0/-0.3/2.0/0.1h -Ccpalet_${file}.cpt -B${scaleopt}/:"${unit}": ${gmtcon} >> ${psfile}
    fi

if test ${wind} -eq 1
then
    grdvector u10.grd v10.grd -J${dPRG} -R${dRANGE} -S50 -Q0.005/0.1/0.05n0.25 -G0 -I${vINT}/${vINT} ${gmtcon} >> ${psfile}
    echo " 4.0 0.65 0.0 0.5" | psxy -R1/100/1/100 -Jx1.0 -Sv0.005/0.1/0.05 -G0 -N ${gmtcon} >> ${psfile}
    echo " 4.25 0.5 3 0.0 0 MC 25 [m/s]" | pstext -R -J -N ${gmtcon} >> ${psfile}
elif test ${wind} -eq 2
then
    grdvector u975.grd v975.grd -J${dPRG} -R${dRANGE} -S50 -Q0.005/0.1/0.05n0.25 -G0 -I${vINT}/${vINT} ${gmtcon} >> ${psfile}
    echo " 4.0 0.65 0.0 0.5" | psxy -R1/100/1/100 -Jx1.0 -Sv0.005/0.1/0.05 -G0 -N ${gmtcon} >> ${psfile}
    echo " 4.25 0.5 3 0.0 0 MC 25 [m/s]" | pstext -R -J -N ${gmtcon} >> ${psfile}
elif test ${wind} -eq 3
then
    grdvector qfu975.grd qfv975.grd -J${dPRG} -R${dRANGE} -S1000 -Q0.005/0.1/0.05n0.25 -G0 -I${vINT}/${vINT} ${gmtcon} >> ${psfile}
    echo " 4.0 0.65 0.0 0.5" | psxy -R1/100/1/100 -Jx1.0 -Sv0.005/0.1/0.05 -G0 -N ${gmtcon} >> ${psfile}
    echo " 4.5 0.5 3 0.0 0 MC 500 [g/(m^2*s)]" | pstext -R -J -N ${gmtcon} >> ${psfile}
fi

    ymdh=$(/home/unuma/usr/local/bin/utc2jst ${yyyy}${mm}${dd}${hh}00 | cut -c1-10)
    Y=${ymdh:0:4}
    m=${ymdh:4:2}
    M=$(/home/unuma/usr/local/bin/mm2MMM ${m})
    D=${ymdh:6:2}
    H=${ymdh:8:2}

# labels (pstext)
# x     y   size angle font place comment
    cat << EOF | pstext -R1/100/1/100 -Jx1.0 -N ${gmtend} >> ${psfile}
 1.0   5.2    6   0.0   0    ML    JMA MSM ${file}
 4.95  5.2    6   0.0   0    MR    ${H}JST ${D}${M}${Y}
# 3.5   0.5    4   0.0   0    MC    ${unit}
 0.4   0.25   1   0.0   0    ML    .
 5.5   0.25   1   0.0   0    MR    .
 0.4   5.5    1   0.0   0    ML    .
 5.5   5.5    1   0.0   0    MR    .
EOF

    unurast_g ${psfile}

done

wwwdir="/home/unuma/www"
if test ${debug_level} -ge 100
then
    echo "DEBUG MODE"
else
    mv press.png qv.png temp.png cape.png cin.png lcl.png lfc.png lnb.png td.png temp500.png temp700.png qv700.png qv975.png thetae975.png wspd250.png pv?00.png ki.png pw.png tt.png ehi.png srh.png ssi.png brn.png qfwind975.png ${wwwdir}/autoplot/jmamsm/
    rm -f Z*bin press.bin prmsl.bin thetae.bin rh.bin temp.bin [uv]10.bin qv.bin cape.bin cin.bin lcl.bin lfc.bin lnb.bin hgt.bin uuu.bin vvv.bin ttt.bin rhh.bin td.bin temp???.bin qv1000.bin qv???.bin thetae???.bin wspd???.bin [uv]???.bin www.bin pv???.bin tt.bin pw.bin ki.bin ehi.bin srh.bin ssi.bin brn.bin qf[uv]???.bin qfwind???.bin
    rm -f press.grd prmsl.grd thetae.grd qv.grd temp.grd [uv]10.grd rh.grd cape.grd cin.grd lcl.grd lfc.grd lnb.grd td.grd temp???.grd qv1000.grd qv???.grd thetae???.grd wspd???.grd [uv]???.grd pv???.grd tt.grd pw.grd ki.grd ehi.grd srh.grd ssi.grd brn.grd qf[uv]???.grd qfwind???.grd
fi

rm -f press.ps prmsl.ps thetae.ps qv.ps temp.ps cape.ps cin.ps lfc.ps lcl.ps lnb.ps td.ps temp???.ps qv1000.ps qv???.ps thetae???.ps wspd???.ps [uv]???.ps pv???.ps pw.ps tt.ps ki.ps ehi.ps srh.ps ssi.ps brn.ps qfwind???.ps
rm -f cpalet_*
rm -f .gmt*
