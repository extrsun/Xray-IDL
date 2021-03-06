#!/bin/csh
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
# $7 - if not =0 (def), keyword "bkgmode" will be set to "user" when calculating 
#      smooth scale, and background will use default value ("b_b.fits" for bck, 
#      "b_berr.fits" for bck error); otherwise, using local background.
# $8 - the broad-band count file root name (def=c_b). The file is used for 
# calculating the smooth scales
#
# written by wqd, 5/9/03
#============================================================================
#
set version=0.1
echo "mcsmooth.e version: " $version 
echo " $1 - energy bands for the smoothing"
echo " $2, $3 - min and max of sigma for the smoothing (def=3,4)"
echo " $4, $5 - min and max of scale for the smoothing (def=0.5,200)"
echo " $6 - if not =0 (def), background maps will be smoothed. If =3, only "
echo "    background maps will be smoothed, using the existing scale map."
echo " $7 - the broad-band count file root name (def=c_b). "
echo " The file is used for calculating the smooth scales"
if ("$1" == "") then
  set bands="1 2 3 4"
else 
  set bands="$1"
endif
if ("$2" == "") then
  set sigmin=3
else 
  set sigmin=$2
endif
if ("$3" == "") then
  set sigmax=4
else 
  set sigmax=$3
endif
if ("$4" == "") then
  set sclmin=0.5
else 
  set sclmin=$4
endif
if ("$5" == "") then 
  set sclmax=200
else 
  set sclmax=$5
endif
if ("$6" == "") then
  set bmaps=0
else 
  set bmaps=$6
endif
if ("$7" == "") then
  set bckfg=1
else
  set bckfg=0
endif
if ("$8" == "") then
  set cbfroot='c_b'
else 
  set cbfroot=$8
endif
if (${bmaps} != 3) then 
  if (${bckfg} != 0) then
    csmooth infile=${cbfroot}.fits outfile=${cbfroot}_s.fits outsigfile=${cbfroot}.sig \
        outsclfile=${cbfroot}.kernel sigmin=${sigmin} conkerneltype=gauss conmeth=fft \
        clobber=yes sigmax=${sigmax} sclmode=compute sclmin=${sclmin} sclmax=${sclmax} \
        sclmap=none bkgmode="user" bkgmap="b_b.fits" bkgerr="b_berr.fits"
  else
    csmooth infile=${cbfroot}.fits outfile=${cbfroot}_s.fits outsigfile=${cbfroot}.sig \
        outsclfile=${cbfroot}.kernel sigmin=${sigmin} conkerneltype=gauss conmeth=fft \
        clobber=yes sigmax=${sigmax} sclmode=compute sclmin=${sclmin} sclmax=${sclmax} sclmap=none
  endif
endif

foreach band (${bands})		#main loop for the bands
echo "Band = " ${band}
if (${bmaps} != 3) then 
csmooth infile=t_${band}.fits outfile=t_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=${sclmin} sclmax=${sclmax}
csmooth infile=c_${band}.fits outfile=c_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=${sclmin} sclmax=${sclmax}
endif

if (${bmaps} != 0) then 
csmooth infile=b_${band}.fits outfile=b_${band}_s.fits conkerneltype=gauss conmeth=fft \
    clobber=yes sclmap=${cbfroot}.kernel outsigfile=none outsclfile=none sclmode=user \
    sigmin=${sigmin} sigmax=${sigmax} sclmin=${sclmin} sclmax=${sclmax}
endif

end

