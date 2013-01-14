#
# This Makefile producted by cm1r16
# Last modified: 2012/10/25
#

#SHELL = /bin/sh


#-----------------------------------------------------------------------------
#                      NETCDF SECTION
#  (uncomment the following four lines if you want netcdf output capability)
#   (also, make sure the paths to netcdf files are correct for your machine)
#              (NOTE: Don't change lines 3 and 4!)
#
OUTPUTINC = -I/usr/local/netcdf-3.6.3-intelxe/include
OUTPUTLIB = -L/usr/local/netcdf-3.6.3-intelxe/lib
OUTPUTOPT = -DNETCDF
LINKOPTS  = -lnetcdf
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
#OPTS = -O0 -warn all -check all -traceback -FR -assume byterecl -fp-model precise -g
OPTS = -O3 -xSSE4.2 -FR -assume byterecl -fp-model precise
#-----------------------------------------------------------------------------
#  Linux, single processor, using g95 compiler
#FC   = g95
#OPTS = -ffree-form -O2
#-----------------------------------------------------------------------------
#  Linux, single processor, using gfortran compiler
#FC   = gfortran44
#OPTS = -O0 -Wall -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbounds-check -fbacktrace -ffree-form -frecord-marker=4 -lm
#-----------------------------------------------------------------------------
#CPP  = cpp -C -P -traditional
#-----------------------------------------------------------------------------
#-- You shouldn't need to change anything below here
#-----------------------------------------------------------------------------

SRC1   = read_nc3.f90
SRC2   = write_nc3.f90

OBJS1 = $(addsuffix .o, $(basename $(SRC1)))
OBJS2 = $(addsuffix .o, $(basename $(SRC2)))

FFLAGS  =  $(OPTS)
AR      = ar cru

.SUFFIXES:
.SUFFIXES:      .f90 .o

all : read_nc3 #write_nc3

read_nc3:		$(OBJS1)
			$(FC) $(OBJS1) $(FFLAGS) $(OUTPUTINC) $(OUTPUTLIB) $(LINKOPTS) -o read_nc3

write_nc3:		$(OBJS2)
			$(FC) $(OBJS2) $(FFLAGS) $(OUTPUTINC) $(OUTPUTLIB) $(LINKOPTS) -o write_nc3

.f90.o:
			$(FC) $(FFLAGS) $(OUTPUTINC) $(OUTPUTLIB) $(LINKOPTS) -c $*.f90

clean:
			rm -f *.o *.f90~ *genmod.* Makefile~ read_nc3 write_nc3

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
