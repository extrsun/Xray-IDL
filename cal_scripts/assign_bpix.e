#!/bin/sh
#============================================================================
#
if [ "$1" = "" ]
then
	bpix1_file=`ls *bpix*`
else
	bpix1_file=$1
fi
# assign the bad pixel parameter file
d=0
while test $d -ne 10
do
   pset ardlib AXAF_ACIS${d}_BADPIX_FILE = "${bpix1_file}[BADPIX${d}]";
   d=`echo "$d+ 1" | bc -l `;
done
