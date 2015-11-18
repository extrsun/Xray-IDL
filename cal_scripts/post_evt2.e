#!/bin/sh
#============================================================================
# post_evt2.e
version=0.0
echo "post_evt2.e version: " $version 
#============================================================================
# Shell script for post-evt2 calibration
#
# Steps:1) destreaks the data and remove extra bad pixels in the ACIS-S
# 	2) removes the background flares
#
# Arguments:
#  $1 == ROOT name of the evt2 events file (e.g., acisf02025N001_evt2_new)
#
# Example: 
# 	post_evt2.e acisf02025N001_evt2_new aciss
#
# wqd, July 4, 2002
# dasmith, Oct 23, 2003 (modified)
#############################################################################
if [ "$1" = "" ] ; then
	echo "A file root name (e.g., acisf01972N001_evt2_new) is needed!"
	 exit -1
else
	events_root=$1
	events_file=$events_root.fits
	if [ -f $events_file ]
	then 
	 echo $events_file "is found"
	else
	 echo $events_file "does not exist!"
	 exit -1
	fi
fi
instr=$INSTR
	
events_root_clean=${events_root}_clean

#############################################################################
#for ACIS-S, destreak ccd8 and remove extra bad pixels
#############################################################################
if [ "$instr" = "aciss" ]; then
	echo "starting the destreak process"
	#assuming that ccd_id=8 (ACIS-S4) is included:
	punlearn destreak
	pset destreak infile=${events_file}
	pset destreak outfile=temp_${events_file}
	pset destreak ccd_id=8
	pset destreak filter=yes
	pset destreak clobber=yes
	pset destreak mode=h
	destreak verbose=2
	events_file=temp_${events_file}

	echo "removing extra bad pixels in ccd6"
	rm temp_${events_file}
	fcopy ${events_file}"[events][!(ccd_id==6&&((chipx==766&&chipy==501)||(chipx==538&&chipy==8)))]" temp_${events_file}
	events_file=temp_${events_file}
fi
#############################################################################
# remove background flares
#############################################################################
# First edit the point source region file by removing all spaces and 
# deleting the first line.

if [ -f source.reg ] ; then 
    grep 'ellipse' source.reg > temp_source.reg
    #append the following
    grep 'circle' source.reg >> temp_source.reg

    sed s/" "//g temp_source.reg > source_edit.reg
    rm temp_source.reg
    echo "cat source_edit.reg"
    cat source_edit.reg

    #first remove the sources:
    echo "removing sources from $events_file"
    dmcopy "${events_file}[events][exclude sky=region(source_edit.reg)]" \
	temp_${events_file} clobber=yes
else
    ln -s ${events_file} temp_${events_file}
fi

echo "constructing light-curve:"
if [ "$instr" = "aciss" ]; then
    dmcopy "temp_${events_file}[events][energy=2500:7000,ccd_id=7]" \
	temp_temp_${events_file} clobber=yes
#dmcopy "${events_file}[events][energy=2500:6000,ccd_id=5]" temp_${events_file} clobber=yes
#dmcopy "${events_file}[events][energy=300:12000,ccd_id=6,8]" temp_${events_file} \
else
    dmcopy "temp_${events_file}[events][energy=300:12000,ccd_id=0,1,2,3]" \
	temp_temp_${events_file} clobber=yes
fi

punlearn dmextract
pset dmextract infile="temp_temp_${events_file}[bin time=::259.28]"
pset dmextract outfile=_eventsfile.lc
pset dmextract opt=ltc1
pset dmextract clobber=yes
pset dmextract mode=h
dmextract

lcfile=_eventsfile.lc
gtifile=_eventsfile_clean_gti.fits 
#gtifile=${events_root_clean}_gti.fits
psfile=lcfile.ps

if [ "$instr" = "aciss" ] ; then
 $PUBDIR/cal_scripts/lc_clean.e temp_$events_file $PUBDIR/cal_scripts/lc_clean_ccd67.par
else
 $PUBDIR/cal_scripts/lc_clean.e temp_$events_file $PUBDIR/cal_scripts/lc_clean_ccd0123.par
fi

echo "applying the new GTI filter to the events file with sources"
punlearn dmcopy
dmcopy "${events_file}[events][@${gtifile}]" ${events_root_clean}.fits clobber=yes

echo "The cleaned evt2 file ${events_root_clean}.fits is created!"
#need to redo the gti to get rid of the filter extension, suitable for make_acisbg
fextract "${events_root_clean}.fits[gti]" ${gtifile} clobber=yes

#############################################################################
# remove temporary files
#############################################################################
mv _eventsfile_clean_gti.fits ${events_root_clean}_gti.fits
rm temp* _*
