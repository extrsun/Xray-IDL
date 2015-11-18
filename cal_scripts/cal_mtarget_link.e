#!/bin/sh
#============================================================================
# cal_main for multiple data files
#============================================================================
# $1 - file directory list (e.g., "2268 2269")
#
# written by wqd, Dec. 24, 2002
#============================================================================
if [ "$1" = "" ] ; then
	echo "file directory list (e.g., "2268 2269") is needed!"
	exit -1
else
	files=$1
fi 
if [ "$2" = "" ] ; then
	echo "file directory (e.g., /d4/m82) is needed!"
	exit -1
else
	maindir=$2
fi 
if [ "$3" = "" ] ; then
	events_root=evt2file
else
	events_root=$3
fi 
if [ "$4" = "" ] ; then
	evtdir=../cal/
else
	evtdir=$4
fi 
evtfname=${events_root}_new_clean

for file in $files		#main loop for the files
do        
	echo "File = " $file  
	mkdir ${MAINDIR}/${file}/xdata
	cd $MAINDIR/${file}/xdata/
	pwd
	ln -s ${evtdir}${evtfname}.fits  ${evtfname}.fits
	echo "ln -s ${evtdir}${evtfname}.fits  ${evtfname}.fits"
	k=1
	for band in $BANDS
	do
	 ln -s ${evtdir}${evtfname}_i${band}.fits   ${evtfname}_i$k.fits
	 echo "ln -s ${evtdir}${evtfname}_i${band}.fits ${evtfname}_i${k}.fits"
	 k=`expr $k + 1`
	done
done
echo done
