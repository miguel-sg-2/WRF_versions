#!/bin/bash
## WRF installation with parallel process.
# Download and install required library and data files for WRF.
# License: LGPL
# Jamal Khan <jamal.khan@legos.obs-mip.fr>
# Tested in Ubuntu 18.04 LTS
# Link: https://gist.github.com/jamal919/5498b868d34d5ec3920f306aaae7460a

################################################################
# env setting (step 0)
export HOME=`cd;pwd`

#export DIR=$HOME/WRF/Library # old one
export DIR=$HOME/mintrop/WRF_forIdealSims-master/Library # new
export PARENT_DIR=$HOME/WRF_versions
export WRF_DIR=$PARENT_DIR/WRF
#export WPS_DIR=$PARENT_DIR/WPS-4.1

# Use proper gcc
#sudo update-alternatives --config gcc

export CC=gcc
export CXX=g++
export FC=gfortran
export F77=gfortran
export HDF5=$DIR

export CPPFLAGS=-I$DIR/include 
export LDFLAGS=-L$DIR/lib

export PATH=$DIR/bin:$PATH
export NETCDF=$DIR

export LD_LIBRARY_PATH=$DIR/lib:$LD_LIBRARY_PATH
export CPPFLAGS=-I$DIR/include 
export LDFLAGS=-L$DIR/lib
export LIBS="-lnetcdf -lhdf5_hl -lhdf5 -lz" 

export JASPERLIB=$DIR/lib
export JASPERINC=$DIR/include


################################################################


############################ WRF 4.1.5 #################################
## WRF v4.1.5
## Downloaded from git tagged releases
########################################################################
# remove line "-DBUILD_RRTMG_FAST=1 \" from configure.wrf (not necessary)

# compile
cd $WRF_DIR
./clean
./clean -a
./configure # 34, 1 for gfortran and distributed memory
./compile em_real >& compile_real.log &
rm -rf main_real
cp -r main main_real

############################ IDEAL CASES #################################

# call env variables (step 0)
# copy $HOME/WRF/main executables elsewhere, as they will be overwritten
cd $WRF_DIR
./clean
./clean -a
./configure # 34, 1 for gfortran and distributed memory
./compile em_les >& compile_les.log &
rm -rf main_ideal
cp -r main main_ideal

##########################################################################
# Download and install WPS v 4.1
##########################################################################

cd $PARENT_DIR
wget -c https://github.com/wrf-model/WPS/archive/refs/tags/v4.1.tar.gz #https://github.com/wrf-model/WPS/archive/v4.1.tar.gz
tar -xvzf v4.1.tar.gz -C $PARENT_DIR
cd $PARENT_DIR/WPS-4.1
./clean
./configure #3: tutorial; if opt=1, the compilation exits
./compile >& compile.log &

##########################################################################
##################### Static Geography Data         ######################
##########################################################################
# http://www2.mmm.ucar.edu/wrf/users/download/get_sources_wps_geog.html

cd $PARENT_DIR
wget -c https://www2.mmm.ucar.edu/wrf/src/wps_files/geog_high_res_mandatory.tar.gz
tar -xvzf geog_high_res_mandatory.tar.gz -C $PARENT_DIR


## export PATH and LD_LIBRARY_PATH
echo "export PATH=$DIR/bin:$PATH" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=$DIR/lib:$LD_LIBRARY_PATH" >> ~/.bashrc

