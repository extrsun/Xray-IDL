#! /bin/csh
#-------------------------------------------------------------
# calculate the background subtracted weight map
# written by D. Smith and wqd, Jan 27, 2003
#-------------------------------------------------------------
if ( "$1" == "" ) then
	echo "spec_weight.e band spec_root event_root datadir"
	echo "The band (e.g., "500:3000")"
	echo "spec_root=spec"
	echo "event_root=evt2file_new_clean_sd"
	echo "datadir=./"
	exit -1
else
	set band=$1
endif 

if ( "$2" == "" )  then
	set spec_root=spec
else
	set spec_root=$2
endif 

if ( "$3" == "" )  then
	set event_root=evt2file_new_clean_sd
else
	set event_root=$3
endif 

if ( "$4" == "" )  then
	set datadir=./
else
	set datadir=$4
endif 
echo $datadir
echo $event_root

#set bands="1000:2000"
set block=8

#
# Create source and background images in detector coordinates
#
dmcopy "${datadir}${event_root}.fits[energy=${band},sky=region(${spec_root}.reg)][bin detx=::${block},dety=::${block}]" ${spec_root}_sim.fits clobber=yes 
dmcopy "${datadir}b${event_root}.fits[energy=${band},sky=region(${spec_root}.reg)][bin detx=::${block},dety=::${block}]" ${spec_root}_bim.fits clobber=yes

#
# Exposures
#
dmkeypar ${spec_root}_sim.fits EXPOSURE
set sexp = `pget dmkeypar rval`
dmkeypar ${spec_root}_bim.fits EXPOSURE
set bexp = `pget dmkeypar rval`
echo 'exposures for the source and backg: ' $sexp $bexp
if ( -e "mul.c" ) then
	echo "mul.c exists!"
	exit
else if ( -e "a.out" ) then
	echo "a.out exists!"
	exit
else
cat << EOF > mul.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

main( int argc, char *argv[] )
{
  double Number1, Number2;

  Number1 = atof( argv[1] );
  Number2 = atof( argv[2] );

  /* Print Number1 divided by Number2 */
  printf( "%lg\n", Number1 / Number2 );

  return EXIT_SUCCESS;
}
EOF
gcc -lm -o a.out mul.c
set result = `./a.out $sexp $bexp`
rm mul.c a.out
endif

#
# Create a background subtracted image.  Multiply background by e.g.
# 80540.27/550000 = 0.1464 prior to subtraction.  Need to copy DATE
# keywords from first file as input into mkwarf (the default won't
# do this).
#
if ( -e "dmimgcalc_header_lookup.txt" ) then
	echo "dmimgcalc_header_lookup.txt exists!"
	exit
else
cat << EOF > dmimgcalc_header_lookup.txt
MISSION		Merge-Merged;Force-AXAF		
TELESCOP	Merge-Merged;Force-Unknown	
OBJECT		Merge-Merged;Force-Unknown	
OBSERVER	Merge-Merged;Force-Unknown	
TITLE		Merge-Merged;Force-Unknown	
OBS_ID		Merge-Merged;Force-Unknown	
INSTRUME	Merge-Merged;Force-Unknown	
DETNAM		Merge-Merged;Force-Unknown	
GRATING		Merge-Merged;Force-Unknown	
OBS_MODE	Merge-Merged;Force-Unknown	
DATAMODE	Merge-Merged;Force-Unknown	
ORIGIN		WarnFirst;Force-ASC		
CONTENT		Force				
HDUNAME		Force				
RA_NOM		WarnOmit-0.0003			
DEC_NOM		WarnOmit-0.0003			
ROLL_NOM	WarnOmit-1.0			
SIM_X		WarnOmit-0.001			
SIM_Y		WarnOmit-0.001			
SIM_Z		WarnOmit-0.001			
FOC_LEN		WarnOmit-1.0			
EQUINOX		WarnPrefer-2000.0		
RADECSYS	WarnPrefer-ICRS			
DATACLAS	WarnPrefer-Observed		
TIMVERSN	WarnFirst			
CLOCKAPP	WarnFirst			
MJDREF		fail				
TIERRELA	Max				
TIERABSO	max				
TIMEDEL		maX				
DATE		Force			
DATE-OBS	Force			
DATE-END	Force			
TSTART		Force			
TSTOP		Force			
ONTIME		Force			
LIVETIME	Force			
DTCOR		Force			
TIMEUNIT	Fail;Default-s			
TIMESYS		Fail;Default-UTC		
TIMEPIXR	Fail;Default-0.5		
EOF

#
# Run dmimgcalc
#
punlearn dmimgcalc
pset dmimgcalc weight2=${result}
dmimgcalc infile=${spec_root}_sim.fits \
    infile2=${spec_root}_bim.fits \
    outfile=${spec_root}.wmap \
    operation=sub \
    weight=1 \
    lookupTab=dmimgcalc_header_lookup.txt \
    clobber=yes

#
# Cleanup files
#
rm dmimgcalc_header_lookup.txt
endif

#
# Done
#
exit
