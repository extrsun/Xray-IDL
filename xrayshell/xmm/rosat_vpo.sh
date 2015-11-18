#/bin/csh
#============================================================================
#  phoctrayp.sh
#============================================================================
#Shell script for pulling count vs N_H data from XSPEC's Raymond-Smith
#with Wisconsin absorption model.  The data is a model of background
#flux that is attenuated by the intervening interstellar medium.  This
#script can be run by typing the command:
#
#  phoctrayp.sh [#1] [#2] [#3] [#4]
#
#where [#1] is the lower limit of the energy band to use, and [#2] is
#the upper limit of the energy band to use.  Thus letting [#1] = 13
#and [#2] = 18 in the above command line will run the XSPEC model with
#the command, "ignore **-12,19-**".  The default settings for [#1] and
#[#2] are 3 and 10 respectively.  The third variable is the temperature
#in kT (keV); the default value if the argument is not given is 0.08625
#keV or 1.e6 K.  The final parameter is the name of the output file, with
#the default value of "phoctrayp.dat".
#
#Since the arguments are read in order, it is okay to leave out the
#latter arguments from a command, but not the earlier ones.  Thus, the
#ONLY acceptable ways to run this script are:
#
#  phoctrayp.sh [#1] [#2] [#3] [#4]
#  phoctrayp.sh [#1] [#2] [#3]
#  phoctrayp.sh [#1] [#2]
#  phoctrayp.sh [#1]
#  phoctrayp.sh
#
#This Raymond-smith with Wisconsin absorption model (constructed in XSPEC
#by using the command "model wabs raymond") will have the following para-
#meters:
#
#  wabs         nH 10^22        varied
#  raymond      kT(keV)         argument [#3]
#  raymond      Abundanc        1.0000
#  raymond      Redshift            0.
#  raymond      norm            1.0000
#
#This shell script first creates a batch script which will run XSPEC,
#and output is directed to log files of the form "phoct_log.rayp".
#The data from the log files are stripped and placed into the final
#data file with a name given by argument [#4].  It is this data file
#then that is used by the IDL routine "phoctlint.pro" for interpolating
#counts from a given column density vector and photon index.
#
#Arguments:
#  $1 == ignore from ** to this channel in XSPEC model; default = 2;
#  $2 == ignore from this channel to ** in XSPEC model; default = 11;
#  $3 == temperature in keV; default = 0.08625 keV (or 1.e6 K);
#  $4 == name of output file; default = "phoctrayp.dat".

#Check to see if arguments exist:
if ($1 == '') then
  set lo = 2
#  set lo = 1
else
#  set lo = `nawk 'BEGIN{print i-1.;exit}' i=$1`
   set lo = $1
endif
if ($2 == '') then
  set hi = 11
#  set hi = 1
else
#  set hi = `nawk 'BEGIN{print i+1.;exit}' i=$2`
  set hi = $2
endif
if ($3 == '') then
  set temp = 0.002
else
  set temp = $3
endif
if ($4 == '') then
  set fdat = "shrayp.dat"
else
  set fdat = $4
endif
if ($5 == '') then
  set abund = 0.5
else
  set abund = $5
endif
if ($6 == '') then
  set fluxlo = 1
else
  set fluxlo = $6
endif
if ($7 == '') then
  set fluxhi = 11
else
  set fluxhi = $7
endif
if ($8 == '') then
  set fname = "rhri.fak"
else
  set fname = $8
endif
echo $lo $hi $temp $fdat
#Create batch files:
#set abund = $5
set f_inst = "sh_inst.rayp"
echo "data " $fname > $f_inst
echo "chatter 0" >> $f_inst
echo "model var po" >> $f_inst
foreach i (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
  echo `` >> $f_inst
end
echo "ignore **-"$lo" "$hi"-**" >> $f_inst
#echo "newpar 1 1.0000" >> $f_inst
echo "newpar 2 = 1" >> $f_inst
echo "newpar 3 = 1 $abund" >> $f_inst
echo "newpar 4 = 1 $abund" >> $f_inst
echo "newpar 5 = 1 $abund" >> $f_inst
echo "newpar 6 = 1 $abund" >> $f_inst
echo "newpar 7 = 1 $abund" >> $f_inst
echo "newpar 8 = 1 $abund" >> $f_inst
echo "newpar 9 = 1 $abund" >> $f_inst
echo "newpar 10 = 1 $abund" >> $f_inst
echo "newpar 11 = 1 $abund" >> $f_inst
echo "newpar 12 = 1 $abund" >> $f_inst
echo "newpar 13 = 1 $abund" >> $f_inst
echo "newpar 14 = 1 $abund" >> $f_inst
echo "newpar 15 = 1 $abund" >> $f_inst
echo "newpar 16 = 1 $abund" >> $f_inst
echo "newpar 17 = 1 $abund" >> $f_inst
echo "newpar 18 = 1 $abund" >> $f_inst
echo "newpar 19  $temp 0" >> $f_inst
#echo "newpar 20  $abund 0" >> $f_inst
echo "chatter 10" >> $f_inst
foreach i (0.0010 0.0020 0.0030 0.0040 0.0050 0.0070 0.0090 \
    0.0095 0.0100 0.0110 0.0120 0.0130 0.0140 0.0150 0.0160 0.0170 0.0180 \
    0.0190 0.0200 0.0210 0.0220 0.0230 0.0240 0.0250 0.0260 0.0270 0.0280 \
    0.0290 0.0300 0.0320 0.0340 0.0360 0.0380 0.0400 0.0450 0.0569 0.0600 \
    0.0800 0.1000 0.1200 0.1259 0.1400 0.1600 0.1799 0.2000 0.2200 0.2400 \
    0.2608 0.2800 0.3000 0.3200 0.3364 0.3400 0.3500 0.3600 0.3700 0.3800 \
    0.3900 0.4000 0.4120 0.4200 0.4364 0.4400 0.4642 0.4800 0.4964 0.5000 \
    0.5200 0.5344 0.5400 0.5600 0.5805 0.6096 0.6387 0.6776 0.7165 0.7741 \
    0.8317 0.9378 1.0439 1.1    1.3    1.5    1.8    2.2    3.     4. \
    6.     10.    14     20     27     38     55     75     100)
  echo "newpar 1 "$i >> $f_inst
  echo "show" >> $f_inst
end
# get flux
  echo "newpar 1 0." >> $f_inst
  echo "flux" $fluxlo $fluxhi >> $f_inst

echo "quit" >> $f_inst
echo "y" >> $f_inst
#Run XSPEC using the just created batch files.
set f_log = "sh_log.rayp"
#hs
xspec < $f_inst > $f_log
#Pull out column densities, N_H and place them into temporary files:
set cuta = "_cut.a"
set cutb = "_cut.b"
#sed -n -e /"2    2       powerlaw"/p $f_log | cut -c 45-57 > $f_log$cuta
#sed -n -e /" 1    1    1       varabs"/p $f_log | cut -c 45-57 > $f_log$cuta
sed -n -e /"  1    1    1   varabs"/p $f_log | cut -c 49-60 > $f_log$cuta
#Pull out counts and place them into temporary files:
#sed -n -e /After/p $f_log | cut -c 61-71 > $f_log$cutb
sed -n -e /"Model predicted"/p $f_log | cut -c 25-38 > $f_log$cutb
	
#Paste together data from the temporary files:
echo $temp > $fdat
paste  $f_log$cuta $f_log$cutb  >> $fdat

# put the flux at the end 
#sed -n -e /"Model flux"/p $f_log | cut -c 34-45 >> $fdat
sed -n -e /"Model flux"/p $f_log | cut -c 41-51 >> $fdat

#Remove extraneous data files and end program:
#rm -f sh_inst* sh_log*
exit 0

