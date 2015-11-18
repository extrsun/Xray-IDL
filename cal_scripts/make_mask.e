#!/bin/sh
#============================================================================
# make_mask.e
#============================================================================
# Shell script for creating a mask without effective correction, a simple
# modification of make_expmap.e.
# The mask is useful for calculating backscal of spectral extraction based
# on no-ciao routines
#
# Arguments:
#  $1 == ROOT name of the events file (e.g., evt2file_new_clean)
#        (def =acis*_evt2)
#  $2 == Input list of aspect solution file(s) (e.g., pcad_asol1.lis).
#  $3 == single ccd number (e.g., 7; overriding the default multiple ccd choice)
#
# Example: 
#  make_mask.e  evt2file_new_clean pcad_asol1.lis 
#
# written by wqd, July 6, 2007
#============================================================================
if [ "$1" = "" ]
then
	if [ -f acis*_evt2.fits ]
	then 
	 events_root=`ls acis*_evt2.fits  | cut -d. -f1` 
	else
	 echo "This file acis*_evt2.fits does not exist!"
	 exit -1
	fi
else
	events_root=$1
fi
echo 'events_root = ' $events_root

if [ "$2" = "" ]
then
	asol_file=`ls  pcad*.fits`
	if [ -f $asol_file ]
	then 
	 "Use the aspec solution file ${asol_file}!"
	else
	 echo "This file ${asol_file} does not exist!"
	 exit -1
	fi
else 
    asol_file=$2
fi
if [ "$3" = "" ]
then
	if [ "$INSTR" = "aciss" ]
	then 
	    ccd="6 7 8"	#for loop
	    ccd_id="6,7,8" 	#for dmcopy 
	    xygrid="3072.5:5120.5"  #block=2
	else
	    ccd="0 1 2 3"
	    ccd_id="0,1,2,3"
	    xygrid="2560.5:5632.5" #block=3
	fi
	tail=''
else
	ccd=$3
	ccd_id=$3
	if [ "$INSTR" = "aciss" ]
	then 
	    xygrid="3072.5:5120.5"  #block=2
	else
	    xygrid="2560.5:5632.5" #block=3
	fi
	tail=_ccd$3
fi
#for a subarray observation, modify the following:
xpixelgrid="1:1024"
ypixelgrid="1:1024"
caldir=/net1/hobby/pub/cal_scripts

bpix1_file=`ls *bpix*`

#default input file names:
events_file=${events_root}.fits
events_gti_file=_eventsfile_gti.fits
if [ -f $events_gti_file ]
then 
	echo "The file ${events_gti_file} exists!"
else
	fextract "$events_file[gti]" ${events_gti_file} 
fi
#default output file names:
asphist_root=${events_root}_asphist

echo "ObsID " ${events_root}

binning=1024

#preparing for the loops:
punlearn mkexpmap 
pset mkexpmap normalize=no #multipled by exposure time
pset mkexpmap verbose=0 
#pset mkexpmap xygrid=2560.5:5632.5:#${binning},2560.5:5632.5:#${binning} 
pset mkexpmap xygrid=${xygrid}:#${binning},${xygrid}:#${binning} 
pset mkexpmap useavgaspect=no
pset mkexpmap mode=h     
pset mkexpmap clobber=yes
punlearn dmregrid 
pset dmregrid bin="1:${binning}:1,1:${binning}:1" rotangle=0 npts=1 
pset dmregrid xoffset=0 yoffset=0 rotxcenter=0 rotycenter=0
pset dmregrid verbose=0
pset dmregrid mode=h
pset dmregrid clobber=yes
punlearn dmimgcalc
pset dmimgcalc operation=div
#pset dmimgcalc weight=1.0
#pset dmimgcalc weight2=${average_counts}
pset dmimgcalc verbose=0
pset dmimgcalc mode=h
pset dmimgcalc clobber=yes
punlearn dmcopy
pset dmcopy mode=h

band=0
	pset mkinstmap spectrumfile=${caldir}/weights_$band.txt
	count=0
	for letter in ${ccd}
	do                  
		#making exposure map:
 		pset mkexpmap instmapfile=$PUBDIR/caldb_local/mask/mask_ccd${letter}.fits;
 		pset mkexpmap outfile=temp_expmap_${band}_ccd${letter}.fits;
		pset mkexpmap asphistfile=${asphist_root}_${letter}.fits;
		#plist mkexpmap
 		mkexpmap;        
    		count=`expr $count + 1`  ;  
	done
	#stacking them together
        ls temp_expmap_${band}_ccd?.fits > temp_expmap_${band}.list 
        cat temp_expmap_${band}.list 
        pset dmregrid infile=@temp_expmap_${band}.list 
        pset dmregrid outfile=${events_root}_mask${tail}.fits
        dmregrid 

pset mkexpmap mode=ql;
pset dmregrid mode=ql;
pset dmimgcalc mode=ql
pset dmcopy mode=ql
rm temp*


