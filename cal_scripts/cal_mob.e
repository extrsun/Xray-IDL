#!/bin/sh
#============================================================================
# cal_main for multiple data files
#============================================================================
# $1 - file directory list (e.g., "2268 2269")
# $2 - main directory name 
#
# written by wqd, Dec. 24, 2002
#============================================================================
if [ "$1" = "" ] ; then
	echo "file directory list (e.g., "2268 2269") is needed!"
	 exit -1
else
	obs=$1
fi 
if [ "$2" = "" ] ; then
	echo "main directory name  (e.g., "/d4/m82/") is needed!"
	 exit -1
else
	maindir=$2
fi 
events_root=evt2file
for ob in $obs		#main loop for the obs
do        
    echo "Ob = " $ob
    cd ${maindir}${ob}/cal/
    pwd
    $PUBDIR/cal_scripts/evt1to2.e $events_root 
    $PUBDIR/cal_scripts/post_evt2.e ${events_root}_new 
    $PUBDIR/cal_scripts/make_expmap.e  ${events_root}_new_clean asol_file
done

echo done
