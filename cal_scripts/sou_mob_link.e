#!/bin/sh
#============================================================================
# sou_mob_link.e for multiple data sets of the same target
# linking source files of the sets to the same directory (e.g., /d4/m82/sou)
#============================================================================
# $1 - relative target directory which contains multiple files (e.g.,/d4/m82/)
# $2 - file directory list (e.g., "2268 2269")
# $3 - file name
#
# written by wqd, June 12, 2003
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
if [ "$4" = "" ] ; then
	tail=_map70BSH_hr
else
	tail=$4
fi 

for file in $files		#main loop for the files
do     
	if [ $file = "./" ] ; then
	    mevtfname=evt2file_new_clean_map70BSH_hr
	else
	    mevtfname=${file}_${evtfname}
	fi 
	fname=${maindir}${file}/sou/${evtfname}${tail}
	echo "File = " $file
	ln -s ${fname} ${mevtfname}
	echo "ln -s ${fname}  ${mevtfname}"
done
echo done
