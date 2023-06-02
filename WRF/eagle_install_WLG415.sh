#!/bin/bash
################################################################
# WRF-LES-GAD installation on NREL's Eagle supercomputer 
# for WRF v. 4.1.5 based on MSG's guidelines
################################################################

# env setting (step 0)
export HOME=`cd;pwd`

# Load modules 
module purge
module load hdf5/1.10.6/intel-impi
module load intel-mpi/2020.1.217
module use /nopt/nrel/apps/modules/centos74/modulefiles
module load netcdf-c/4.6.2/intel-18.0.3-mpi
module load netcdf-f/4.4.4/intel1803-impi

# Set environment variables
export NETCDF=$NETCDF_FORTRAN
export JASPERINC=/nopt/nrel/apps/wrf/wps_dep_libs/include/
export JASPERLIB=/nopt/nrel/apps/wrf/wps_dep_libs/lib/
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_EM_CORE=1

# Get WRF
#git clone git@github.com:wradunz/WRF-LES-GAD.git
cd WRF-LES-GAD

# Set WRF dir path
export PARENT_DIR=$PWD
export WRF_DIR=$PARENT_DIR/framework/WRF

################################################################


############################ WRF 4.1.5 #################################
## WRF v4.1.5
## Downloaded from git tagged releases
########################################################################

# compile
cd $WRF_DIR
./clean
./clean -a
cp configure.wrf_Eagle configure.wrf
#./configure # 34, 1 for gfortran and distributed memory
./compile em_real >& compile_real.log &
rm -rf main_real
cp -r main main_real

############################ IDEAL CASES #################################

# call env variables (step 0)
# copy $HOME/WRF/main executables elsewhere, as they will be overwritten
cd $WRF_DIR
./clean
./clean -a
cp configure.wrf_Eagle configure.wrf
# configure @ eagle: 67. (dm+sm)   INTEL (ifort/icc): HSW/BDW
./compile em_les >& compile_les.log &
rm -rf main_ideal
cp -r main main_ideal

##########################################################################
# Download and install WPS v 4.1
##########################################################################

cd $PARENT_DIR
wget -c https://github.com/wrf-model/WPS/archive/refs/tags/v4.1.tar.gz #https://github.com/wrf-model/WPS/archive/v4.1.tar.gz
tar -xvzf v4.1.tar.gz -C $PARENT_DIR
cd $WPS_DIR
# configure @ eagle: 67. (dm+sm)   INTEL (ifort/icc): HSW/BDW
./configure #3: tutorial; @ eagle try opt=19
./compile >& compile.log &

######################## Static Geography Data ####################
# http://www2.mmm.ucar.edu/wrf/users/download/get_sources_wps_geog.html
cd $HOME/WRF/Downloads
wget -c https://www2.mmm.ucar.edu/wrf/src/wps_files/geog_high_res_mandatory.tar.gz
tar -xvzf geog_high_res_mandatory.tar.gz -C $HOME/WRF


## export PATH and LD_LIBRARY_PATH
echo "export PATH=$DIR/bin:$PATH" >> ~/.bashrc
echo "export LD_LIBRARY_PATH=$DIR/lib:$LD_LIBRARY_PATH" >> ~/.bashrc

