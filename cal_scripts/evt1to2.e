#!/bin/sh
#======================================================================
# evt1to2.e
version=0.1
echo "evt1to2.e version: " $version 
#============================================================================
# script for the level 1 to level 2 event calibration
# 
# Requirements:
# $PUBDIR should have been defined, which contains the cal_scripts directory
#
# This script should be run after completing offset adjustment 
# procedure using the website: 
# http://asc.harvard.edu/cal/ASPECT/fix_offset/fix_offset.cgi
# resulting in a file aspcorr_evt2.fits, used here
#
#*outputs:
# The finished new evt2 file is the original input file with the suffix 
# _new.fits
# 
# wqd, July 4, 2002
# modify to include the CTI correction
# (acis_process_events), wqd, Dec. 5, 2002
# add check keyword chvf for reducing the background in the timed VF mode
# (a significant fraction of the source events may be flagged and removed 
# for moderately bright to bright sources.)
# Remove key words instr etc to make them environment variables, wqd, 1/3/03
# add tgain_corr keyword to acis_process_events avaiable for ciao 3.1, 8/19/2004
#======================================================================
if [ "$1" = "" ]
then
	echo "A file root name (e.g., acisf01972N001_evt2) is needed!"
	 exit -1
else
	events_root=$1
	events_file=${events_root}.fits
fi
chvf=$CHVF

#output file name root (before the background flare cleaning)
events_new_root=${events_root}_new

# set default file names:
events_new_file=${events_new_root}.fits

badpixfile=`ls *bpix*`

# Start the evt1 processing:
echo "starting acis_process_events"
if [ "$chvf" = "yes" ]
then
 echo "combining cti and VF cleaning (assuming no bright piled-up sources):"
 punlearn acis_process_events
 acis_process_events infile=aspcorr_evt2.fits\
 outfile=temp_${events_file} acaofffile=@asol_file apply_cti=$APPLYCTI\
 apply_tgain=yes gainfile=CALDB badpixfile=$badpixfile eventdef=")stdlev1" check_vf_pha=yes\
 doevtgrade=yes stop=none clobber=yes verbose=2 rand_pix_size=0
else 
 punlearn acis_process_events
 acis_process_events infile=aspcorr_evt2.fits apply_tgain=yes \
 outfile=temp_${events_file} acaofffile=@asol_file apply_cti=$APPLYCTI\
 gainfile=CALDB badpixfile=$badpixfile eventdef=")stdlev1"\
 doevtgrade=yes stop=none clobber=yes verbose=2  rand_pix_size=0
fi
echo "selecting grades."
punlearn dmcopy
dmcopy "temp_${events_file}[EVENTS][grade=0,2,3,4,6,status=0]" temp1_${events_file} opt=all clobber=yes
#dmcopy also removes the events with stauts not=0
echo "done"

echo "applying GTI filters"
punlearn dmcopy
dmcopy "temp1_${events_file}[EVENTS][@fltfile.fits]" ${events_new_file} opt=all clobber=yes

echo " The new evt2 file ${events_new_file} is produced!"
