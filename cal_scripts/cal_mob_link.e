#!/bin/sh
#============================================================================
# cal_mob_link.e for multiple data sets of the same target
# linking cleaned event and exposure files of the sets to the same directory
#============================================================================
# $1 - target directory  (relative to the global xdata directory) which 
#           contains multiple files (e.g., ../)
# $2 - file directory list (e.g., "2268 2269")
# $3 - file name
#
# written by wqd, Jan 6, 2003
#============================================================================
if [ "$1" = "" ] ; then
	echo "file directory (e.g., /d4/m82/) is needed!"
	exit -1
else
	maindir=$1
fi 
if [ "$2" != "" ] ; then
	files=$2
fi 
if [ "$3" = "" ] ; then
	evtfname=evt2file_new_clean
else
	evtfname=$3
fi 

for file in $files		#main loop for the files
do     
	if [ $file = "./" ] ; then
	    mevtfname=evt2file_new_clean
	    mbevtfname=evt2file_new_clean_b
	else
	    mevtfname=${file}_${evtfname}
	    mbevtfname=${file}_${evtfname}_b
	fi 
	fname=${maindir}${file}/cal/${evtfname}
	bfname=${maindir}${file}/back/${evtfname}_b
	echo "File = " $file
	ln -s ${fname}.fits  ${mevtfname}.fits
	ln -s ${bfname}.fits  ${mbevtfname}.fits
	echo "ln -s ${fname}.fits  ${mevtfname}.fits"
	echo "ln -s ${bfname}.fits  ${mbevtfname}.fits"
	k=1
	for band in $BANDS
	do
	  ln -s ${fname}_i${band}.fits ${mevtfname}_i$k.fits
	  echo "ln -s ${fname}_i${band}.fits ${mevtfname}_i$k.fits"
	  if [ "$INSTR" = "aciss" ]
	      then 
	      ln -s ${fname}_ccd7_i${band}.fits ${mevtfname}_ccd7_i$k.fits
	      echo "ln -s ${fname}_ccd7_i${band}.fits ${mevtfname}_ccd7_i$k.fits"
	  fi 
	  ln -s ${bfname}$k.fits ${mevtfname}_b$k.fits
	  echo "ln -s ${bfname}$k.fits ${mevtfname}_b$k.fits"
	  k=`expr $k + 1`
	done
	ln -s ${fname}_mask.fits ${mevtfname}_i0.fits
done
echo done
