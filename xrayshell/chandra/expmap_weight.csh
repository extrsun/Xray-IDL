#! /bin/csh
#-------------------------------------------------------------
# calculate the weights for the exposure maps
# written by D. Smith, Aug 01, 2003
#-------------------------------------------------------------
if ( "$1" == "" ) then
    echo "expmap_weight.csh event_root chiptype instr"
    echo "event_root=evt2file_new_clean"
    echo "chiptype=aciss"
    echo "instr=aciss"
    exit -1
else
    set event_root=$1
endif 
#
if ( "$2" == "" )  then
    set chiptype=aciss
else
    set chiptype=$2
endif 
#
if ( "$3" == "" )  then
    set instr=aciss
else
    set instr=$3
endif 
#
dmkeypar ${event_root}.fits TSTART
set time = `pget dmkeypar rval`
set days = `echo "scale = 4 ;($time - 49091521.0) / ( 24.0 * 3600.0)" | bc -l`
#
ln -sf $PUBDIR/xrayshell/chandra/${chiptype}.fak ${chiptype}.fak
ln -sf $PUBDIR/xrayshell/chandra/${chiptype}.rmf ${chiptype}.rmf
ln -sf $PUBDIR/xrayshell/chandra/${chiptype}.arf ${chiptype}.arf
#
if ( "$instr" == "aciss" ) then
    set elo = `echo 0.3 0.7 1.5 3.0`
    set eelo = `echo 0.4 0.8 1.6 3.1`
    set ehi = `echo 0.7 1.5 3.0 7.0`
    set eehi = `echo 0.6 1.4 2.9 6.9`
else if ( "$instr" == "acisi" ) then
    set elo = `echo 0.5 1.0 2.0 4.0`
    set eelo = `echo 0.6 1.1 2.1 4.1`
    set ehi = `echo 1.0 2.0 4.0 8.0`
    set eehi = `echo 0.9 1.9 3.9 7.9`
else if ( "$instr" == "acisi_low" ) then
    set elo = `echo 1.0 2.5 4.0 6.0`
    set eelo = `echo 1.1 2.6 4.1 6.1`
    set ehi = `echo 2.5 4.0 6.0 9.0`
    set eehi = `echo 2.4 3.9 5.9 8.9`
else
    echo "Please make the instrument choice (aciss, acisi, acis_low)"
    exit -1
endif
#
#setenv LMODDIR /net/xray/software/acisabs
#setenv LD_LIBRARY_PATH "/net/xray/software/acisabs:${LD_LIBRARY_PATH}"
#
cat << EOF > xxx.xcm
data 1:1 ${chiptype}.fak
model acisabs * wabs( po )
   ${days}     -1.0000       0.0000       0.0000       10000.       10000.
  7.22000E-03 -1.00000E-05   0.0000       0.0000       1.0000       1.0000
  0.58200     -0.10000       0.0000       0.0000       1.0000       1.0000
   620.00      -1.0000       1.0000       1.0000       10000.       10000.
   10.000      -1.0000       0.0000       0.0000       50.000       50.000
   20.000      -1.0000       1.0000       1.0000       50.000       50.000
   2.0000      -1.0000       0.0000       0.0000       50.000       50.000
   1.0000      -1.0000       0.0000       0.0000       50.000       50.000
  3.00000E-02  1.00000E-03   0.0000       0.0000      1.00000E+05  1.00000E+06
   1.7000      1.00000E-02  -3.0000      -2.0000       9.0000       10.000
   1.0000      1.00000E-02   0.0000       0.0000      1.00000E+24  1.00000E+24
notice 1:1-**
ignore 1:0.0-$eelo[1] $eehi[1]-**
notice 1:$elo[1]-$ehi[1]
log xxx.log
show rates
notice 1:1-**
ignore 1:0.0-$eelo[2] $eehi[2]-**
notice 1:$elo[2]-$ehi[2]
show rates
notice 1:1-**
ignore 1:0.0-$eelo[3] $eehi[3]-**
notice 1:$elo[3]-$ehi[3]
show rates
notice 1:1-**
ignore 1:0.0-$eelo[4] $eehi[4]-**
notice 1:$elo[4]-$ehi[4]
show rates
delcomp 1
notice 1:1-**
ignore 1:0.0-$eelo[1] $eehi[1]-**
notice 1:$elo[1]-$ehi[1]
show rates
notice 1:1-**
ignore 1:0.0-$eelo[2] $eehi[2]-**
notice 1:$elo[2]-$ehi[2]
show rates
notice 1:1-**
ignore 1:0.0-$eelo[3] $eehi[3]-**
notice 1:$elo[3]-$ehi[3]
show rates
notice 1:1-**
ignore 1:0.0-$eelo[4] $eehi[4]-**
notice 1:$elo[4]-$ehi[4]
show rates
log none
quit
y
EOF
#
xspec - xxx
#
set xxx = `grep predicted xxx.log`
#
# Band 1
set b1 = `echo "scale = 4 ;$xxx[5] / $xxx[25]" | bc -l`
# Band 2 
set b2 = `echo "scale = 4 ;$xxx[10] / $xxx[30]" | bc -l`
# Band 3
set b3 = `echo "scale = 4 ;$xxx[15] / $xxx[35]" | bc -l`
# Band 4
set b4 = `echo "scale = 4 ;$xxx[20] / $xxx[40]" | bc -l`
#
echo ${b1} ${b2} ${b3} ${b4}
if ( ${chiptype} == "aciss" ) then
    setenv BI_FRAC "${b1} ${b2} ${b3} ${b4}"
else if ( ${chiptype} == "acisi" ) then
    setenv FI_FRAC "${b1} ${b2} ${b3} ${b4}"
else
    echo "Please make the instrument choice (aciss, acisi)"
    exit -1
endif
#
exit
#
