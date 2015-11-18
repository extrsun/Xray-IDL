#!/bin/csh
#
#-------------------------------------------------------------
# change the exposure time in the background spectrum 
# written by D. Smith, Mar 20, 2003
#-------------------------------------------------------------
if ( "$1" == "" ) then
	echo "spec_expos.e spec_root"
	echo "spec_root=spec"
	exit -1
else
	set spec_root=$1
endif

dmkeypar ${spec_root}_sou_g.pi BACKSCAL
set sback = `pget dmkeypar rval` 
dmkeypar ${spec_root}_b_sou_g.pi BACKSCAL
set bback = `pget dmkeypar rval` 
echo 'BACKSCAL values for the source and reference file are: ' $sback $bback

if ( -e "div.c" ) then
    echo "div.c exists!"
    exit
else if ( -e "a.out" ) then
    echo "a.out exists!"
    exit
else
cat << EOF > div.c
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
gcc -lm -o a.out div.c
set scale = `./a.out $bback $sback`
echo $scale

#Scale BACKSCAL keyword in reference dataset
echo $bback $scale
set result = `./a.out $bback $scale`

echo $result
dmhedit ${spec_root}_b_sou_g.pi filelist=none operation=add key=BACKSCAL \
    value=$result
 
#Get BACKSCAL keywords from the blank-sky reference dataset
dmkeypar b${spec_root}_b.pi BACKSCAL
set bback = `pget dmkeypar rval`
echo $bback

#Scale BACKSCAL keywords
echo $bback $scale
set result = `./a.out $bback $scale`

echo $result
dmhedit b${spec_root}_b.pi filelist=none operation=add key=BACKSCAL \
    value=$result

#get exposure from the real dataset
dmkeypar ${spec_root}_b_sou_g.pi EXPOSURE
set bexp = `pget dmkeypar rval`
echo $bexp

#Scale EXPOSURE keyword in reference file
echo $bexp $scale
set result = `echo "scale=2; $bexp * $scale" | bc -l` 

echo $result
dmhedit ${spec_root}_b_sou_g.pi filelist=none operation=add key=EXPOSURE \
    value=$result
 
#Get exposures from the blank-sky dataset
dmkeypar b${spec_root}_b.pi EXPOSURE
set bexp = `pget dmkeypar rval`
echo $bexp

#Scale EXPOSURE keyword in blank-sky datasets
echo $bexp $scale
set result = `echo "scale=2; $bexp * $scale" | bc -l` 

echo $result
dmhedit b${spec_root}_b.pi filelist=none operation=add key=EXPOSURE \
    value=$result

# delete unwanted files:
rm -f div.c a.out
endif

exit
