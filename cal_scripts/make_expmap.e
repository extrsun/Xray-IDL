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
#       environment variable BANDS is defined
#	The default spectral weight files are in the directory:
#	$PUBDIR/cal_scripts. The weights_band.txt are created with the 
#       procedure described in memo_weights, where band is an element of 
#       bands (e.g., = 2000:4000). 
#       All the necessary files (*evt*,  pcad*.fits, *bpix*) are in the 
#       current directory
#
#
# Arguments:
#  $1 == ROOT name of the events file (e.g., evt2file_new_clean)
#        (def =acis*_evt2)
#  $2 == Input list of aspect solution file(s) (e.g., pcad_asol1.lis).
#  $3 == single ccd number (e.g., 7; overriding the default multiple ccd choice)
#
# Example: 
#  make_expmap.e  evt2file_new_clean pcad_asol1.lis > make_expmap.log
#tail make_expmap.log
#
# Outputs:
# 
# ${events_root}_c$band.fits
# ${events_root}_i$band.fits
# ${events_root}_f$band.fits
#
# written by wqd, May 25, 2002
# remove the low energy degradation correction since ciao now does this auto..
# wqd, June 28, 2004
# modified to include an output for ccd7 only, July 10, 2007
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
	tail=""
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

# create the aspect histograms
for letter in ${ccd}
  do
  punlearn asphist 
  pset asphist infile="@${asol_file}[@${events_file}[ccd_id=${letter}]]";
  pset asphist outfile=${asphist_root}_${letter}.fits;
  pset asphist evtfile=${events_file}; 
  pset asphist dtffile="";
  pset asphist mode=h;
  pset asphist verbose=0;
  pset asphist clobber=yes;
  plist asphist
  asphist;
done
pset asphist mode=ql

echo "Making Instrument Maps in the bands " $BANDS

#preparing for the loops:
punlearn mkinstmap 
pset mkinstmap verbose=0 
pset mkinstmap grating=NONE maskfile=NONE mirror=HRMA 
pset mkinstmap ardlibparfile=ardlib.par 
pset mkinstmap pixelgrid=${xpixelgrid}:#${binning},${ypixelgrid}:#${binning}
pset mkinstmap pbkfile=pbkfile.fits
pset mkinstmap dafile=CALDB
pset mkinstmap mode=h 
pset mkinstmap clobber=yes
pset mkinstmap obsfile="${events_file}";
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

d=0
index=0
for band in $BANDS		#main loop for the bands
do        
	echo "Band = " ${band}
	pset mkinstmap spectrumfile=${caldir}/weights_$band.txt
	count=0
	for letter in ${ccd}
	do                  
		#making instrument map:                   
 		pset mkinstmap outfile=temp_instmap_${band}_ccd${letter}.fits;#
 		pset mkinstmap detsubsys=ACIS-${letter};
# 		pset mkinstmap detsubsys="ACIS-${letter};CONTAM=NO";
 		#plist mkinstmap;
 		mkinstmap;

		#making exposure map:
 		pset mkexpmap instmapfile=temp_instmap_${band}_ccd${letter}.fits;
 		pset mkexpmap outfile=temp_expmap_${band}_ccd${letter}.fits;
		pset mkexpmap asphistfile=${asphist_root}_${letter}.fits;
		#plist mkexpmap
 		mkexpmap;        
    		echo "CCD = ${letter}";
# for CCD=7 create an exposure map for the chip alone, added on July 12,07
		if [ "$letter" = "7" ]
		    then
		    pset dmregrid infile=temp_expmap_${band}_ccd${letter}.fits
		    pset dmregrid outfile=${events_root}_ccd${letter}_i${band}${tail}.fits
		    dmregrid 
	fi    

    		count=`expr $count + 1`  ;  
	done
#-------------------------------------------------------------------
#from Hui Dong for making a mask:
        if [ $index = 0 ]
        then
           echo " creating the mask file "
           for letter in ${ccd}
    	   do                  
                echo ${letter}
                echo "image=readfits('temp_instmap_${band}_ccd${letter}.fits',hdr)" > script.idl
                echo "posi=where(image gt 0,nsel)" >> script.idl
                echo "if nsel ne 0 then image(posi)=1" >> script.idl
                echo "writefits,'mask_ccd${letter}.fits',image,hdr" >> script.idl
                echo "exit" >> script.idl

                csh $PUBDIR/xrayidl/xidl_setup_b
                
 		pset mkexpmap instmapfile=mask_ccd${letter}.fits;
 		pset mkexpmap outfile=temp_expmap_ccd${letter}0.fits;
		pset mkexpmap asphistfile=${asphist_root}_${letter}.fits;
		#plist mkexpmap
 		mkexpmap;
           done        
                    
           ls temp_expmap_ccd?0.fits > temp_expmap.list
           cat temp_expmap.list
           pset dmregrid infile=@temp_expmap.list
	   pset dmregrid outfile=${events_root}_mask.fits
       
           dmregrid
	   rm mask_ccd?.fits script.idl
        fi
#-------------------------------------------------------------------
        if [ $CMAP != 2 ]
	then      
        index=`expr $index + 1`

	d=`expr $d + 1`
	#stacking them together
        ls temp_expmap_${band}_ccd?.fits > temp_expmap_${band}.list 
        cat temp_expmap_${band}.list 
        pset dmregrid infile=@temp_expmap_${band}.list 
        pset dmregrid outfile=${events_root}_i${band}${tail}.fits
        dmregrid 
	fi
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
rm temp* evt2file_new_clean_asphist_?.fits


