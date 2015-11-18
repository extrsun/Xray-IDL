#!/bin/sh
#============================================================================
# cal_main for multiple data files
#============================================================================
# $1 - file directory list (e.g., "2268 2269")
# $2 - main directory name 
# $3 - ccd_id (def ="0,1,2,3")
# $4 - stowed data directory (def= /net1/hobby/CHANDRA/acisbg06/data)
# $5 - stowed data file (def=acis_D_0123567_stowed_evt_150606.fits; June 16, 2006)
# $6 - if not empty, VF cleaning will be done
#
# written by wqd, Dec. 24, 2002
# updated by wqd, June 2, 2007 together with memo_bg, but not tested 
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
if [ "$3" = "" ] ; then
	ccd_id="0,1,2,3"
	echo "assuming ccd_id =" $ccd_id
else
	STOWDIR=$3
fi 
if [ "$4" = "" ] ; then
	STOWDIR=/net1/hobby/CHANDRA/acisbg06/data
	echo "assuming STOWDIR = " $STOWDIR
else
	STOWDIR=$4
fi 
if [ "$5" = "" ] ; then
	STOWFILE=acis_D_0123567_stowed_evt_150606.fits acis_D_0123567_stowed_evt_170404.fits
	echo "assuming STOWFILE = " $STOWFILE
else
	STOWFILE=$5
fi 
eventroot=evt2file_new_clean
beventroot=b${eventroot}

for ob in $obs		#main loop for the obs
do        
    echo "Ob = " $ob
    cd ${maindir}${ob}
    mkdir back 
    cd back
    pwd
#auto find the the aoff file 
ln -s  ../cal/${eventroot}.fits ${eventroot}.fits
find ../ -name 'acis*_aoff1.fits.gz' -exec gunzip {} \;
find ../ -name 'acis*_aoff1.fits' -exec ln -s {} aoff.fits \;

#Assuming all the stowed data and related files are in (make sure that they are the latest) 
#dmcopy "${STOWDIR}${STOWFILE}[grade=0,2,3,4,6][ccd_id=$ccd_id][energy="300:12000"]" ${beventroot}.fits clobber=yes
if [ "$6" = "" ] ; then
       dmcopy "${STOWDIR}${STOWFILE}[grade=0,2,3,4,6][ccd_id=$ccd_id]" ${beventroot}.fits clobber=yes
else
       dmcopy "${STOWDIR}${STOWFILE}[grade=0,2,3,4,6][ccd_id=$ccd_id]" ${beventroot}.fits clobber=yes
       copy ${beventroot}.fits"[events][status==bxxxxxxxx0xxxxxxxxxxxxxxxxxxxxxxx]" ${beventroot}.fits
       echo "Assuming the VF mode cleaning "
	
fi 

#The above file will be modified
ln -s ${PUBDIR}/cal_scripts/make_acisbg.par ./
ln -s ../cal/${eventroot}_gti.fits ./gti.fits
${PUBDIR}/cal_scripts/make_acisbg

done

echo done
