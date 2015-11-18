#!/bin/sh
#============================================================================
# mcsmooth.e
#
# Master script for smoothing count, exposure, and bgk maps in multiple bands 
#
# $1 - energy bands for the smoothing
# $2, $3 - min and max of sigma for the smoothing (def=3,4)
# $4, $5 - min and max of scale for the smoothing (def=0.5,200)
# $6 - if not =0 (def), background maps will be smoothed. If =3, only 
#    background maps will be smoothed, using the existing scale map.
# $7 - the broad-band count file root name (def=c_b). The file is used for 
# calculating the smooth scales
#
# written by ljt, 6/16/07
#============================================================================
#

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
	sclmin=0.5
else 
	sclmin=$4
fi
if [ "$5" = "" ]
then
	sclmax=200
else 
	sclmax=$5
fi
if [ "$6" = "" ]
then
	bmaps=0
else 
	bmaps=$4
fi
if [ "$7" = "" ]
then
	cbfroot='c_b'
else 
	cbfroot=$5
fi

if [ ${bmaps} != 3 ] ; then 
punlearn csmooth
csmooth infile=${cbfroot}.fits outfile=${cbfroot}_s.fits outsigfile=${cbfroot}.sig \
    outsclfile=${cbfroot}.kernel sigmin=${sigmin} conkerneltype=gauss conmeth=fft \
    clobber=yes sigmax=${sigmax} sclmode=compute sclmin=${sclmin} sclmax=${sclmax} sclmap=none bkgmode=user bkgmap=b_b.fits bkgerr=eb_b.fits
fi

for band in ${bands}		#main loop for the bands
do        
echo "Band = " ${band}
if [ ${bmaps} != 3 ] ; then 
punlearn csmooth
csmooth infile=t_${band}.fits outfile=t_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=${sclmin} sclmax=${sclmax}
punlearn csmooth
csmooth infile=c_${band}.fits outfile=c_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=${sclmin} sclmax=${sclmax}
fi

if [ ${bmaps} != 0 ] ; then 
punlearn csmooth
csmooth infile=b_${band}.fits outfile=b_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=${sclmin} sclmax=${sclmax}
fi

done

