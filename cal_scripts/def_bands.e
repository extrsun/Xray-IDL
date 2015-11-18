#!/bin/sh
#============================================================================
if [ "$INSTR" = "aciss" ]
then
	bands="300:700 700:1500 1500:3000 3000:7000" 
	    #for ACIS-S high lat fields
elif [ "$INSTR" = "acisi" ]
then
#	bands="500:1400 1400:2000 2000:4000 4000:8000" #for high lat fields
	bands="500:1000 1000:2000 2000:4000 4000:8000" #for high lat fields
elif [ "$INSTR" = "acisi_low" ]
then
	#bands="1000:3000 3000:5000 5000:8000" #for low lat fields (e.g., gcs)
	#bands="1000:2000 2000:3500 3500:5000 5000:8000" 
	bands="1000:2500 2500:4000 4000:6000 6000:9000"
	    #for low lat fields (e.g., gcs)
else
          echo "Please make the instrument choice (aciss, acisi, acis_low)"
          exit -1
fi
echo $bands


