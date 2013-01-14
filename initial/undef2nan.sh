#!/bin/sh
#
# UNDEF2NAN  ver.1.1
# producted by Takashi Unuma, Kochi Univ.
# modified  by Takashi Unuma, Kyoto Univ.
# Last update: 2013/01/11
#

for data in $*
do
echo "Now execute: ${data}"

thread=4
FC=ifort
FCOPTIM="-O3 -assume byterecl -warn all -openmp"

nlon=500
nlat=500

if test -s ${data}
then
    ln -s ${data} input.out
    cat > tmp_undef2nan.f << EOF
c#######################################
      program undef2nan_parallel

      integer nlon, nlat ,i, j
      parameter(nlon=${nlon})
      parameter(nlat=${nlat})
      real idata(nlon,nlat)
      real nan
      data nan/Z'7fffffff'/

c     input file
      open(10,
     &     file='input.out',
     &     access='direct',
     &     status='old',
     &     form='unformatted',
     &     recl=nlon*nlat*4)
      read(10,rec=1) idata
      close(10)

c     parallel region
c$OMP PARALLEL PRIVATE(i,j,nlat,nlon), SHARED(idata)
c$OMP DO 
      do j=1, nlat
         do i=1, nlon
            if (idata(i,j).lt.-900) then
               idata(i,j)=nan
            endif
         end do
      end do
c$OMP END DO
c$OMP END PARALLEL

c     output file
      open(11,
     &     file='output.out',
     &     access='direct',
     &     status='new',
     &     form='unformatted',
     &     recl=nlon*nlat*4)
      write(11,rec=1) idata
      close(11)

      stop
      end program undef2nan_parallel
c#######################################
EOF

# compile
    export LANG='en_US'
    ${FC} ${FCOPTIM} tmp_undef2nan.f -o tmp_undef2nan.exe
    export OMP_NUM_THREADS=${thread}
    ./tmp_undef2nan.exe
    mv ${data} ${data}_backup
    mv output.out ${data}

    rm -f tmp_undef2nan.f
    rm -f tmp_undef2nan.exe
#    rm -f tmp_lst
    rm -f input.out
else
    echo "Please make data."
    echo "${data} does not exist."
fi

done