#!/bin/sh
#============================================================================
# mcsmooth.e
#
# Master script for smoothing count, exposure, and bgk maps in multiple bands 
#
# $1 - energy bands for the smoothing
# $2, $3 - min and max of sigma for the smoothing (def=3,4)
# $4 - if not =0 (def), background maps will be smoothed. If =3, only 
#    background maps will be smoothed, using the existing scale map.
# $5 - the broad-band count file root name (def=c_b). The file is used for 
# calculating the smooth scales
#
# written by wqd, 5/9/03
#============================================================================
#
version=0.1
echo "mcsmooth.e version: " $version 
echo " $1 - energy bands for the smoothing"
echo " $2, $3 - min and max of sigma for the smoothing (def=3,4)"
echo " $4 - if not =0 (def), background maps will be smoothed. If =3, only "
echo "    background maps will be smoothed, using the existing scale map."
echo " $5 - the broad-band count file root name (def=c_b). "
echo " The file is used for calculating the smooth scales"
if [ "$1" = "" ]
then
	bands="1 2 3 4"
else 
	bands=$1
fi
if [ "$2" = "" ]
then
	sigmin=3
else 
	sigmin=$2
fi
if [ "$3" = "" ]
then
	sigmax=4
else 
	sigmax=$3
fi
if [ "$4" = "" ]
then
	bmaps=0
else 
	bmaps=$4
fi
if [ "$5" = "" ]
then
	cbfroot='c_b'
else 
	cbfroot=$5
fi
if [ ${bmaps} != 3 ] ; then 
csmooth infile=${cbfroot}.fits outfile=${cbfroot}_s.fits outsigfile=${cbfroot}.sig \
    outsclfile=${cbfroot}.kernel sigmin=${sigmin} conkerneltype=gauss conmeth=fft \
    clobber=yes sigmax=${sigmax} sclmode=compute sclmin=0.5 sclmax=200 sclmap=none bkgmode=user bkgmap=bgd.fits bkgerr=bgd_err.fits
fi

for band in ${bands}		#main loop for the bands
do        
echo "Band = " ${band}
if [ ${bmaps} != 3 ] ; then 
csmooth infile=t_${band}.fits outfile=t_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=0.5 sclmax=200 
csmooth infile=c_${band}.fits outfile=c_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=0.5 sclmax=200 
fi

if [ ${bmaps} != 0 ] ; then 
csmooth infile=b_${band}.fits outfile=b_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=0.5 sclmax=200 
fi

done

