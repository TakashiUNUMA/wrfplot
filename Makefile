#
# This Makefile producted by cm1r16
# Last modified: 2013/01/05
#

#SHELL = /bin/sh


#-----------------------------------------------------------------------------
#                      NETCDF SECTION
#  (uncomment the following four lines if you want netcdf output capability)
#   (also, make sure the paths to netcdf files are correct for your machine)
#              (NOTE: Don't change lines 3 and 4!)
#
#NETCDF = /home/unuma/usr/local/netcdf-4.1.3
#HDF5 = /home/unuma/usr/local/hdf5-1.8.7
#ZLIB = /home/unuma/usr/local/zlib-1.2.5
#OUTPUTINC = -I$(NETCDF)/include
#OUTPUTLIB = -L$(NETCDF)/lib -L$(ZLIB)/lib -L$(HDF5)/lib
#OUTPUTOPT = -DNETCDF
#LINKOPTS  = -lhdf5_hl -lhdf5 -lm -lcurl -lnetcdff -lnetcdf
#-----------------------------------------------------------------------------
#                         HDF SECTION
#  (uncomment the following four lines if you want hdf output capability)
#   (also, make sure the paths to hdf files are correct for your machine)
#              (NOTE: Don't change lines 3 and 4!)
# Note: You may need to remove -lsz.
#
#OUTPUTINC = -I/usr/local/hdf5/include
#OUTPUTLIB = -L/usr/local/hdf5/lib -L/usr/local/szip/lib
#OUTPUTOPT = -DHDFOUT
#LINKOPTS  = -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lsz -lz -lm
#-----------------------------------------------------------------------------
#                         STPK SECTION
OUTPUTINC = -I/home/unuma/usr/local/unulibstpk/include -I/usr/local/netcdf-3.6.3-intelxe/include
OUTPUTLIB = -L/home/unuma/usr/local/unulibstpk/lib -L/usr/local/netcdf-3.6.3-intelxe/lib
LINKOPTS  = -lstpk -lnetcdf
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
#                     HARDWARE SECTION
#-- Choose the appropriate architecture, and uncomment all lines 
#-- in that section.
#-----------------------------------------------------------------------------
#  Linux, single processor, Portland Group compiler
#FC   = pgf90
#OPTS = -Mfree -pc 64 -Kieee -Ktrap=none -O2
#-----------------------------------------------------------------------------
#  Linux, single processor, Intel fortran compiler
FC   = ifort
#OPTS = -O0 -warn all -check all -traceback -FR -assume byterecl -fp-model precise -openmp -openmp-report1
OPTS = -O3 -xSSE4.2 -fma -ipo -unroll0 -fno-alias -FR -assume byterecl -fp-model precise -openmp
#-----------------------------------------------------------------------------
#  Linux, single processor, using g95 compiler
#FC   = g95
#OPTS = -ffree-form -O2
#-----------------------------------------------------------------------------
#  Linux, single processor, using gfortran compiler
#FC   = gfortran
#OPTS = -ffree-form -O2
#-----------------------------------------------------------------------------
#CPP  = cpp -C -P -traditional
#-----------------------------------------------------------------------------
#-- You shouldn't need to change anything below here
#-----------------------------------------------------------------------------

SRC   = calc_index.f90    \
	calc_brn.f90      \
	calc_helicity.f90 \
	calc_press.f90    \
	calc_qflux.f90
SRC1   = read_nc3.f90
SRC2   = undef2nan.f90

OBJS = $(addsuffix .o, $(basename $(SRC)))
OBJS1 = $(addsuffix .o, $(basename $(SRC1)))
OBJS2 = $(addsuffix .o, $(basename $(SRC2)))

FFLAGS  =  $(OPTS)

.SUFFIXES:
.SUFFIXES:      .f90 .o

all : calc_index read_nc3 undef2nan

calc_index:	$(OBJS)
		$(FC) $(OBJS) $(FFLAGS) $(OUTPUTINC) $(OUTPUTLIB) $(LINKOPTS) -o calc_index

read_nc3:	$(OBJS1)
		$(FC) $(OBJS1) $(FFLAGS) $(OUTPUTINC) $(OUTPUTLIB) $(LINKOPTS) -o read_nc3

undef2nan:	$(OBJS2)
		$(FC) $(OBJS2) $(FFLAGS) -o undef2nan

.f90.o:
		$(FC) $(FFLAGS) $(OUTPUTINC) -c $*.f90

clean:
		rm -f *.o *.a *.mod *.f90~ *genmod.*  Makefile~ calc_index read_nc3 undef2nan

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
