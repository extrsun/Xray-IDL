#!/bin/sh
#============================================================================
# make_expmap.e
version=0.0
echo "make_expmap.e version: " $version 
#============================================================================
# Shell script for creating the instrument (exposure) maps in various bands
#
# Steps:1) updating the bad pixel parameter file
# 	2) producing the aspect histogram
#	3) creating the instrument and exposure maps for individual CCDs
#       4) merge the maps 
#	5) creating the corresponding count and flux maps in the same bands
#
# Requirements: 
#	The default spectral weight files are in the directory:
#	$PUBDIR/cal_scripts. The weights_band.txt are created with the 
#       procedure described in memo_weights, where band is an element of 
#       bands (e.g., = 2000:4000). 
#       All the necessary files (*evt*,  pcad*.fits, *bpix*) are in the 
#       current directory
#
#
# Arguments:
#  $1 == instrument choice (i.e., aciss, acisi, or acis_low; def=acisi)
#  $2 == ROOT name of the events file (e.g., acisf02207_000N002_evt2_cti_clean)
#        (def =acis*_evt2)
#  $3 == Input list of aspect solution file(s) (e.g., pcad_asol1.lis).
#  $4 == single ccd number (e.g., 7; overriding the default multiple ccd choice)
#  $5 == ypixelgrid, subarray chipy coordinates (def="384:639" #a 1/4 subarray)
#
# Example: 
#  make_expmap.e acisi acisf02207_000N002_evt2_cti_clean pcad_asol1.lis > make_expmap.log
#tail make_expmap.log
#
# Outputs:
# 
# ${events_root}_c$band.fits
# ${events_root}_i$band.fits
# ${events_root}_f$band.fits
#
# written by wqd, May 25, 2002
#============================================================================
# Set the key variable:
#maindir=.. #master directory

if [ "$1" = "aciss" ]
then
	bands="300:700 700:1500 1500:3000 3000:7000" 
	    #for ACIS-S high lat fields
        xygrid="3072.5:5120.5"  #block=2
elif [ "$1" = "acisi" ]
then
#	bands="500:1400 1400:2000 2000:4000 4000:8000" #for high lat fields
	bands="500:1000 1000:2000 2000:4000 4000:8000" #for high lat fields
	xygrid="2560.5:5632.5" #block=3
elif [ "$1" = "acisi_low" ]
then
	#bands="1000:3000 3000:5000 5000:8000" #for low lat fields (e.g., gcs)
	bands="1000:2000 2000:3500 3500:5000 5000:8000" 
	    #for low lat fields (e.g., gcs)
	xygrid="2560.5:5632.5" #block=3
else
          echo "Please make the instrument choice (aciss, acisi, acis_low)"
          exit -1
fi


if [ "$2" = "" ]
then
	if [ -f acis*_evt2.fits ]
	then 
	 events_root=`ls acis*_evt2.fits  | cut -d. -f1` 
	else
	 echo "This file acis*_evt2.fits does not exist!"
	 exit -1
	fi
else
	events_root=$2
fi
echo 'events_root = ' $events_root

if [ "$3" = "" ]
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
    asol_file=$3
fi
if [ "$4" = "" ]
then
	if [ "$1" = "aciss" ]
	then 
	    ccd="6 7 8"	#for loop
	    ccd_id="6,7,8" 	#for dmcopy 
	else
	    ccd="0 1 2 3"
	    ccd_id="0,1,2,3"
	fi
	tail=""
else
	ccd=$4
	ccd_id=$4
%	tail=_ccd$4
	tail=""
fi
if [ "$5" = "" ]
then
    ypixelgrid="384:639" #a 1/4 subarray for aciss (N157b) ;see Guide p100
#ypixelgrid="256:767" #a 1/2 subarray for aciss
#ypixelgrid="513:1024" #a 1/2 subarray for aciss
#ypixelgrid="1:512" #a 1/2 subarray for aciss
else
    ypixelgrid=$5
fi
#for a subarray observation, modify the following:
xpixelgrid="1:1024"
#xygrid="3072.5:5120.5"  #block=2 aciss
xygrid="3584.5:4608.5"  #block=1
caldir=$PUBDIR/cal_scripts

bpix1_file=`ls *bpix*`
#aoff1_file=`ls $maindir/secondary/*aoff1*`
#asol_file="${maindir}/primary/pcadf114999371N002_asol1.fits"
#aoff1_file=acisf02207_000N002_aoff1_new.fits

#default input file names:
events_file=${events_root}.fits
events_gti_file=${events_root}_gti.fits
if [ -f $events_gti_file ]
then 
	echo "The file $events_gti_file exists!"
else
	fextract "$events_file[gti]" $events_gti_file 
fi
#default output file names:
asphist_file=${events_root}_asphist.fits

echo "ObsID " ${events_root}

binning=1024
echo "All output image files will be binned to factor " ${binning}
echo "Initializing Bad Pixel Parameter File"

# assign the bad pixel parameter file
d=0
while test $d -ne 10
do
   pset ardlib AXAF_ACIS${d}_BADPIX_FILE = "${bpix1_file}[BADPIX${d}]";
   d=`echo "$d+ 1" | bc -l `;
done

echo "Creating Aspect Histograms"

# create the aspect histograms, assuming the same for all chips have the same
# GTI
punlearn asphist 
  pset asphist infile="@$asol_file[@$events_gti_file]";
  pset asphist outfile=$asphist_file;
  pset asphist evtfile=$events_file ; 
  pset asphist dtffile="";
  pset asphist mode=h;
  pset asphist verbose=0;
  pset asphist clobber=yes;
  plist asphist
  asphist;
pset asphist mode=ql

echo "Making Instrument Maps in the bands " $bands

#preparing for the loops:
punlearn mkinstmap 
pset mkinstmap verbose=0 
pset mkinstmap grating=NONE maskfile=NONE mirror=HRMA 
pset mkinstmap ardlibparfile=ardlib.par 
#pset mkinstmap pixelgrid="1:1024:#${binning},1:1024:#${binning}" 
#pset mkinstmap pixelgrid=${xpixelgrid}:#${binning},${ypixelgrid}:#${binning}
pset mkinstmap pixelgrid=${xpixelgrid},${ypixelgrid}
pset mkinstmap mode=h 
pset mkinstmap clobber=yes
pset mkinstmap obsfile="$events_file";
punlearn mkexpmap 
pset mkexpmap normalize=no #multipled by exposure time
pset mkexpmap verbose=0 
#pset mkexpmap xygrid=2560.5:5632.5:#${binning},2560.5:5632.5:#${binning} 
pset mkexpmap xygrid=${xygrid}:#${binning},${xygrid}:#${binning} 
pset mkexpmap useavgaspect=no
pset mkexpmap mode=h     
pset mkexpmap clobber=yes
pset mkexpmap asphistfile=$asphist_file;
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

for band in $bands		#main loop for the bands
do        
	echo "Band = " $band
	pset mkinstmap spectrumfile=${caldir}/weights_$band.txt
	count=0
	for letter in $ccd
	do                  
		#making instrument map:                   
 		pset mkinstmap outfile=temp_instmap_${band}_ccd$letter.fits;
 		pset mkinstmap detsubsys=ACIS-$letter;
 		#plist mkinstmap;
 		mkinstmap;
		#making exposure map:
 		pset mkexpmap instmapfile=temp_instmap_${band}_ccd$letter.fits;
 		pset mkexpmap outfile=temp_expmap_${band}_ccd$letter.fits;
		#plist mkexpmap
 		mkexpmap;        
    		echo "CCD = $letter";
    		count=`expr $count + 1`  ;  
	done
	#stacking them together
        ls temp_expmap_${band}_ccd?.fits > temp_expmap_$band.list 
        cat temp_expmap_$band.list 
        pset dmregrid infile=@temp_expmap_$band.list 
        pset dmregrid outfile=${events_root}_i$band$tail.fits
	echo ${events_root}_i$band$tail.fits
        dmregrid 
	if [ $CMAP = 1 ]
	then      
	    # create image, binned same as exposure map
	    pset dmcopy infile="${events_file}[energy=$band][ccd_id=$ccd_id][bin x=${xygrid}:#${binning},y=${xygrid}:#${binning}]"
	    pset dmcopy outfile=${events_root}_c$band$tail.fits
	    pset dmcopy clobber=yes
	    dmcopy
        
	    #\\ create fluxed image for double checking that the epxosure 
	    #\\ map and original image align properly
	    pset dmimgcalc  infile=${events_root}_c$band$tail.fits
	    pset dmimgcalc infile2=${events_root}_i$band$tail.fits
	    pset dmimgcalc outfile=${events_root}_f$band$tail.fits
	    dmimgcalc

	    aconvolve infile=${events_root}_f$band$tail.fits  \
		outfile=${events_root}_fg$band$tail.fits  \
		kernelspec="lib:gaus(2,5,1,3,3)" method=fft clobber=yes
		#smoothed with a Gaussian of 1sigma=3 pixels (the last two numbers)
	fi
done
pset mkexpmap mode=ql;
pset mkinstmap mode=ql;
pset dmregrid mode=ql;
pset dmimgcalc mode=ql
pset dmcopy mode=ql
%rm temp*


