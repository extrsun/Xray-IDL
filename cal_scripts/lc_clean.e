#!/bin/sh
#============================================================================
# lc_clean.e
version=0.0
echo "lc_clean.e version: " $version 
#============================================================================
# Shell script for removing background flares (filter the lightcurve),
# using Maxim(?) routine lc_clean (no CIAO routine?)
#
#Arguments:
#  $1 == events file name 
#  $2 == lc_clean.par file name (def = lc_clean_ccd67.par for ACIS-S)
#	other current choices are: lc_clean_ccd0123.par for ACIS-I
#  $3 == output cleaned events file name (def = _eventsfile_clean.fits)
#  lc_clean routine from is not a ciao program and is from acisbg (Markavich?),
#  assumed to be installed in the directory $LOCALDIR/acisbg/bin.redhat62/
#
# Additional outputs:
# _eventsfile_clean_gti.fits
#
# written by wqd, May 23, 2002
#============================================================================
# set key parameters:
lcpardir=$PUBDIR/cal_scripts/

#lcpardir=''

#Check to see if arguments exist:
echo $1
if [ "$1" == '' ] 
then
  echo "An input events file is need."
  exit -1
else
  ln -sf $1 _eventsfile.fits
fi
if [ "$2" == '' ] 
then
  lcparfname=${lcpardir}lc_clean_ccd67.par
  echo "lc_clean par file name = " $lcparfname
else 
  lcparfname=$2
fi
if [ "$3" == '' ] 
then
  echo "output file name = _eventsfile_clean.fits"
  outfname=_eventsfile_clean.fits 
else 
  outfname=$3
fi
#if [ "$4" == '' ] 
#then
#  echo "alias lc_clean /net/xray/usr/local/acisbg/bin.redhat62/lc_clean"
#  alias lc_clean='/net/xray/usr/local/acisbg/bin.redhat62/lc_clean'
#else
#  alias lc_clean=$4
#fi

fextract "_eventsfile.fits[gti]" _eventsfile_gti.fits clobber=yes
#run lc_clean
#lc_clean $lcparfname
#/net1/hobby/CHANDRA/acisbg06/bin.redhat62/lc_clean $lcparfname
 $PUBDIR/cal_scripts/lc_clean $lcparfname

#A file was created (_eventsfile_clean_gti.fits) that has
#only time intervals that fulfil your criteria for filtering.

## Apply the cleaning to the data using the FTOOLS command "fcopy" 
## (which is similar to the CIAO command "dmcopy") and by using
## the good times interval that was created above:
rm $outfname

fcopy "_eventsfile.fits[events][gtifilter('_eventsfile_clean_gti.fits')]" $outfname
 
#remove the temp files
#rm _eventsfile_gti.fits _eventsfile.fits

exit 0
