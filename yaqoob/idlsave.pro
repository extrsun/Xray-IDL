; IDL Version 3.0.0 (ultrix mipsel)
; Journal File for yaqoob@legacy.gsfc.nasa.gov
; Working directory: /home2/yaqoob/images
; Date: Sun Jun 27 18:17:42 1993
 
help
slist=s1c3bpls
blist=s1c3bplb
$ls *.gti
help
tmflt
; TMFLT, qlist, nfiles 
; Filters a photon list using up to 10 pre-made ascii GTI files 
; The new qlist contains events only from time-sections which 
; are good and common to ALL files. EACH file must not contain 
; overlapping time intervals but the different files may do 
tmflt,blist,1
; size of photon list on entry         3756
; Enter ascii time intervals file name 
; n4151_s1c3b_std.gti
;       1  1.24947e+07  1.24966e+07
;       2  1.25004e+07  1.25021e+07
;       3  1.25027e+07  1.25027e+07
;       4  1.24434e+07  1.24434e+07
;       5  1.24440e+07  1.24440e+07
;       6  1.24475e+07  1.24476e+07
;       7  1.24476e+07  1.24484e+07
;       8  1.24490e+07  1.24490e+07
;       9  1.24491e+07  1.24493e+07
;      10  1.24548e+07  1.24548e+07
;      11  1.24550e+07  1.24552e+07
;      12  1.24594e+07  1.24595e+07
;      13  1.24595e+07  1.24609e+07
;      14  1.24609e+07  1.24611e+07
;      15  1.24653e+07  1.24655e+07
;      16  1.24667e+07  1.24671e+07
;      17  1.24926e+07  1.24947e+07
;      18  1.24966e+07  1.25004e+07
;      19  1.25027e+07  1.25035e+07
;      20  1.25046e+07  1.25060e+07
;      21  1.25060e+07  1.25060e+07
;      22  1.25066e+07  1.25066e+07
;      23  1.25067e+07  1.25085e+07
;      24  1.25085e+07  1.25086e+07
;      25  1.25086e+07  1.25094e+07
;      26  1.25102e+07  1.25108e+07
;      27  1.25109e+07  1.25111e+07
;      28  1.25111e+07  1.25112e+07
;      29  1.25112e+07  1.25111e+07
;      30  1.25112e+07  1.25112e+07
;      31  1.25112e+07  1.25112e+07
;      32  1.25112e+07  1.25112e+07
;      33  1.25112e+07  1.25112e+07
;      34  1.25112e+07  1.25112e+07
;      35  1.25117e+07  1.25117e+07
;      36  1.25118e+07  1.25118e+07
;      37  1.25118e+07  1.25119e+07
;      38  1.25119e+07  1.25142e+07
;      39  1.25148e+07  1.25148e+07
;      40  1.25156e+07  1.25160e+07
;      41  1.25035e+07  1.25035e+07
;      42  1.25094e+07  1.25094e+07
; new photon list size         3735
tmflt,blist,1
; size of photon list on entry         3735
; Enter ascii time intervals file name 
; n4151_s1c3b_all.gti
;       1  1.24947e+07  1.24966e+07
;       2  1.25004e+07  1.25021e+07
;       3  1.25027e+07  1.25027e+07
;       4  1.24434e+07  1.24434e+07
;       5  1.24440e+07  1.24440e+07
;       6  1.24475e+07  1.24476e+07
;       7  1.24476e+07  1.24484e+07
;       8  1.24490e+07  1.24490e+07
;       9  1.24491e+07  1.24493e+07
;      10  1.24548e+07  1.24548e+07
;      11  1.24550e+07  1.24552e+07
;      12  1.24594e+07  1.24595e+07
;      13  1.24595e+07  1.24609e+07
;      14  1.24609e+07  1.24611e+07
;      15  1.24653e+07  1.24655e+07
;      16  1.24667e+07  1.24671e+07
;      17  1.24926e+07  1.24947e+07
;      18  1.24966e+07  1.25004e+07
;      19  1.25027e+07  1.25035e+07
;      20  1.25046e+07  1.25060e+07
;      21  1.25060e+07  1.25060e+07
;      22  1.25066e+07  1.25066e+07
;      23  1.25067e+07  1.25085e+07
;      24  1.25085e+07  1.25086e+07
;      25  1.25086e+07  1.25094e+07
;      26  1.25102e+07  1.25108e+07
;      27  1.25109e+07  1.25111e+07
;      28  1.25111e+07  1.25112e+07
;      29  1.25112e+07  1.25111e+07
;      30  1.25112e+07  1.25112e+07
;      31  1.25112e+07  1.25112e+07
;      32  1.25112e+07  1.25112e+07
;      33  1.25112e+07  1.25112e+07
;      34  1.25112e+07  1.25112e+07
;      35  1.25117e+07  1.25117e+07
;      36  1.25118e+07  1.25118e+07
;      37  1.25118e+07  1.25119e+07
;      38  1.25119e+07  1.25142e+07
;      39  1.25148e+07  1.25148e+07
;      40  1.25156e+07  1.25160e+07
;      41  1.25035e+07  1.25035e+07
;      42  1.25094e+07  1.25094e+07
; new photon list size         3735
remhp,blist
;       1    3187
;       2     219
;       3     119
;       4      82
;       5      50
;       6      33
;       7      24
;       8      15
;       9       5
;      10       1
;      11       1
;      12       1
;      13       1
;      14       1
;      15       1
;      16       1
;      17       1
;      18       1
;      19       1
;Enter desired cutoff: 
; 10
;Ready to modify plist? y/(n): 
; n
mktmflt
; MKTMFLT, tname=tname, qlist 
; Create GTI ascii file using light curve from qlist 
mktmflt,blist
; Enter name of time intervals output file 
; n4151_s1c3b_bgd.gti
; PHA values are in the range 
;       0    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,2048
; Enter time binsize in seconds 
; 16.
; Number of bins =         4540
;Try different binning ?
; y
; PHA values are in the range 
;       0    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 16.
; Number of bins =         4540
;Try different binning ?
; n
; Enter new min x and max x (0,0 to stop)
; 45000,55000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       49192.992       51770.004
;       12492560.       12495137.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; Enter new min x and max x (0,0 to stop)
; 52000,62000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       53507.535       57715.777
;       12496875.       12501083.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       59331.832       60166.973
;       12502699.       12503534.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; Enter new min x and max x (0,0 to stop)
; 60000,75000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       61236.363       62635.500
;       12504603.       12506002.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       65254.801       66035.719
;       12508622.       12509403.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       66881.695       68004.258
;       12510249.       12511371.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       68492.336       69191.898
;       12511859.       12512559.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       72136.586       72706.000
;       12515504.       12516073.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; Enter new min x and max x (0,0 to stop)
; 70000,80000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; i =        8
view
; VIEW, plist, tname=tname 
; Given a photon list (plist) and GTI ascii file (tname) VIEW the 
; light curve and spectrum. Routine then allows fine adjustment of 
; the GTI file until you are completely satisfied (!) 
view,blist
; Enter GTI file name (or none) 
; n4151_s1c3b_bgd.gti
; PHA values are in the range 
;       0    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 16.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; Number of bins =         1462
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Enter the number of the time region to adjust (0 to stop) 
; 4
; Current start and stop time is        12504603.       12506002.
; Enter new start and stop times (0 for same) 
; 0,12505800
; Number of bins =         1462
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Enter the number of the time region to adjust (0 to stop) 
; 0
; Finished fudging - rewriting GTI file n4151_s1c3b_bgd.gti
;       1       12492560.       12495137.
;       2       12496875.       12501083.
;       3       12502699.       12503534.
;       4       12504603.       12505800.
;       5       12508622.       12509403.
;       6       12510249.       12511371.
;       7       12511859.       12512559.
;       8       12515504.       12516073.
; Make PHA file from spectrum? (must be 512 or 1024) 
; n
view
; VIEW, plist, tname=tname 
; Given a photon list (plist) and GTI ascii file (tname) VIEW the 
; light curve and spectrum. Routine then allows fine adjustment of 
; the GTI file until you are completely satisfied (!) 
view,blist
; Enter GTI file name (or none) 
; n4151_s1c3_bgd.gti
; PHA values are in the range 
;       0    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,200
; Enter time binsize in seconds 
; 16.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; % OPENR: Error opening file: n4151_s1c3_bgd.gti.
;   No such file or directory

retall
close,10
close,1
view,blist
; Enter GTI file name (or none) 
; n4151_s1c3b_bgd.gti
; PHA values are in the range 
;       0    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,200
; Enter time binsize in seconds 
; 16.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; Number of bins =         1461
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Enter the number of the time region to adjust (0 to stop) 
; 0
; Finished fudging - rewriting GTI file n4151_s1c3b_bgd.gti
;       1       12492560.       12495137.
;       2       12496875.       12501083.
;       3       12502699.       12503534.
;       4       12504603.       12505800.
;       5       12508622.       12509403.
;       6       12510249.       12511371.
;       7       12511859.       12512559.
;       8       12515504.       12516073.
; Make PHA file from spectrum? (must be 512 or 1024) 
; n
$vi n4151_s1c3b_bgd.gti
help,blist
tmflt,blist,1
; size of photon list on entry         3735
; Enter ascii time intervals file name 
; n4151_s1c3b_bgd.gti
;       1  1.24926e+07  1.24951e+07
;       2  1.24969e+07  1.25011e+07
;       3  1.25027e+07  1.25035e+07
;       4  1.25046e+07  1.25060e+07
;       5  1.25086e+07  1.25094e+07
;       6  1.25102e+07  1.25114e+07
;       7  1.25119e+07  1.25126e+07
;       8  1.25155e+07  1.25161e+07
; new photon list size         1400
remhp,blist
;       1    1350
;       2      29
;       3      13
;       4       5
;       5       1
;       6       1
;       7       1
;       8       1
;       9       1
;      10       1
;      11       1
;      12       1
;      13       1
;      14       1
;      15       1
;      16       1
;      17       1
;      18       1
;      19       1
;Enter desired cutoff: 
; 4
;Ready to modify plist? y/(n): 
; y
help,blist
view,blist
; Enter GTI file name (or none) 
; none
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,200
; Enter time binsize in seconds 
; 16.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; Number of bins =         1461
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Make PHA file from spectrum? (must be 512 or 1024) 
; n
remhp,blist
;       1    1345
;       2      24
;       3       8
;       4       1
;       5       1
;       6       1
;       7       1
;       8       1
;       9       1
;      10       1
;      11       1
;      12       1
;      13       1
;      14       1
;      15       1
;      16       1
;      17       1
;      18       1
;      19       1
;Enter desired cutoff: 
; 10
;Ready to modify plist? y/(n): 
; n
$vi n4151_s1c3b_bgd.gti
tmflt,blist,1
; size of photon list on entry         1377
; Enter ascii time intervals file name 
; n4151_s1c3b_bgd.gti
;       1  1.24926e+07  1.24951e+07
;       2  1.24969e+07  1.25011e+07
;       3  1.25027e+07  1.25035e+07
;       4  1.25046e+07  1.25058e+07
;       5  1.25086e+07  1.25094e+07
;       6  1.25102e+07  1.25114e+07
;       7  1.25119e+07  1.25126e+07
;       8  1.25155e+07  1.25161e+07
; new photon list size         1340
view,blist
; Enter GTI file name (or none) 
; none
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 16.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; Number of bins =         1462
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Make PHA file from spectrum? (must be 512 or 1024) 
; n
remhp,blist
;       1    1315
;       2      21
;       3       4
;       4       1
;       5       1
;       6       1
;       7       1
;       8       1
;       9       1
;      10       1
;      11       1
;      12       1
;      13       1
;      14       1
;      15       1
;      16       1
;      17       1
;      18       1
;      19       1
;Enter desired cutoff: 
; 3
;Ready to modify plist? y/(n): 
; y
help,blist
view,blist
; Enter GTI file name (or none) 
; none
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 32
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; Number of bins =          731
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Make PHA file from spectrum? (must be 512 or 1024) 
; n
makeqdp
; MAKEQDP,bscale,slist,blist,bin=bin,fname=fname,outname=outname[rootname]
; Make a multi-pha range qdp file containing light curves from 
; two photon lists (slist and blist). The idea is that each panel
; of the plot will consist of a source (slist) and background 
; lightcurve (blist) in a particular PHA range. The PHA ranges 
; are read from a file fname which contains inclusive lower and 
; upper PHA boundaries. Not more than 6 panels recommended per plot
; BSCALE is the background area scaling factor 
; Creates output files outname.qdp and outname.pco 
; Simply type <IDL>$qdp outname > to plot 
makeqdp,1.,blist
; Enter filename containing PHA boundaries 
; phbnd.dat
; Enter output QDP filename 
; n4151_s1c3b_b00
; Enter number of light curves required for each photon list 
; 5
; Enter bin size in seconds 
; 128.
; PHA boundaries: 
; Min and Max photon times:        12492615.       12516007.
; tmin tmax nbins      0.00000       23391.923       183.74940
$qdp n4151_s1c3b_b00
$vi phatim.dat
view,blist
; Enter GTI file name (or none) 
; phatim.dat
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 32.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; % READF: End of file encountered. Unit: 11
;          File: phatim.dat
close,11
$vi phatim.dat
retall
view,blist
; Enter GTI file name (or none) 
; phatim.dat
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 32.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; % Attempt to subscript PLIST with <LONG     (          -1)> is out of range.
retall
close,11
$copy n4151_s1c3b_bgd.gti phatim.dat
$cp n4151_s1c3b_bgd.gti phatim.dat
$vi phatim.dat
view,blist
; Enter GTI file name (or none) 
; phatim.dat
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 32.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; Number of bins =           77
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Enter the number of the time region to adjust (0 to stop) 
; 0
; Finished fudging - rewriting GTI file phatim.dat
;       1       12492560.       12495137.
; Make PHA file from spectrum? (must be 512 or 1024) 
; n
mktmflt
; MKTMFLT, tname=tname, qlist 
; Create GTI ascii file using light curve from qlist 
mktmflt,blist
; Enter name of time intervals output file 
; n4151_s1c3b_bgd.gti2
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 32
; Number of bins =          731
;Try different binning ?
; y
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,200
; Enter time binsize in seconds 
; 16.
; Number of bins =         1461
;Try different binning ?
; y
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,150
; Enter time binsize in seconds 
; 32
; Number of bins =          731
;Try different binning ?
; n
; Enter new min x and max x (0,0 to stop)
; 0,0
; i =        0
; % PLOT: Expression must be an array in this context: UCTS.
retall
view,blist
; Enter GTI file name (or none) 
; none
; PHA values are in the range 
;     100    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,200
; Enter time binsize in seconds 
; 32
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; Number of bins =          731
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Make PHA file from spectrum? (must be 512 or 1024) 
; y
; exposure time =       1328.00
;Enter filename (w/o extension): 
; n4151_s1c3b_bgd01
$ls *.idl
save,filename='n4151_s1c3B.idl'
tmflt,slist,1
; size of photon list on entry         8951
; Enter ascii time intervals file name 
; n4151_s1c3b_std.gti
;       1  1.24947e+07  1.24966e+07
;       2  1.25004e+07  1.25021e+07
;       3  1.25027e+07  1.25027e+07
;       4  1.24434e+07  1.24434e+07
;       5  1.24440e+07  1.24440e+07
;       6  1.24475e+07  1.24476e+07
;       7  1.24476e+07  1.24484e+07
;       8  1.24490e+07  1.24490e+07
;       9  1.24491e+07  1.24493e+07
;      10  1.24548e+07  1.24548e+07
;      11  1.24550e+07  1.24552e+07
;      12  1.24594e+07  1.24595e+07
;      13  1.24595e+07  1.24609e+07
;      14  1.24609e+07  1.24611e+07
;      15  1.24653e+07  1.24655e+07
;      16  1.24667e+07  1.24671e+07
;      17  1.24926e+07  1.24947e+07
;      18  1.24966e+07  1.25004e+07
;      19  1.25027e+07  1.25035e+07
;      20  1.25046e+07  1.25060e+07
;      21  1.25060e+07  1.25060e+07
;      22  1.25066e+07  1.25066e+07
;      23  1.25067e+07  1.25085e+07
;      24  1.25085e+07  1.25086e+07
;      25  1.25086e+07  1.25094e+07
;      26  1.25102e+07  1.25108e+07
;      27  1.25109e+07  1.25111e+07
;      28  1.25111e+07  1.25112e+07
;      29  1.25112e+07  1.25111e+07
;      30  1.25112e+07  1.25112e+07
;      31  1.25112e+07  1.25112e+07
;      32  1.25112e+07  1.25112e+07
;      33  1.25112e+07  1.25112e+07
;      34  1.25112e+07  1.25112e+07
;      35  1.25117e+07  1.25117e+07
;      36  1.25118e+07  1.25118e+07
;      37  1.25118e+07  1.25119e+07
;      38  1.25119e+07  1.25142e+07
;      39  1.25148e+07  1.25148e+07
;      40  1.25156e+07  1.25160e+07
;      41  1.25035e+07  1.25035e+07
;      42  1.25094e+07  1.25094e+07
; new photon list size         8795
tmflt,slist,1
; size of photon list on entry         8795
; Enter ascii time intervals file name 
; n4151_s1c3b_all.gti
;       1  1.24947e+07  1.24966e+07
;       2  1.25004e+07  1.25021e+07
;       3  1.25027e+07  1.25027e+07
;       4  1.24434e+07  1.24434e+07
;       5  1.24440e+07  1.24440e+07
;       6  1.24475e+07  1.24476e+07
;       7  1.24476e+07  1.24484e+07
;       8  1.24490e+07  1.24490e+07
;       9  1.24491e+07  1.24493e+07
;      10  1.24548e+07  1.24548e+07
;      11  1.24550e+07  1.24552e+07
;      12  1.24594e+07  1.24595e+07
;      13  1.24595e+07  1.24609e+07
;      14  1.24609e+07  1.24611e+07
;      15  1.24653e+07  1.24655e+07
;      16  1.24667e+07  1.24671e+07
;      17  1.24926e+07  1.24947e+07
;      18  1.24966e+07  1.25004e+07
;      19  1.25027e+07  1.25035e+07
;      20  1.25046e+07  1.25060e+07
;      21  1.25060e+07  1.25060e+07
;      22  1.25066e+07  1.25066e+07
;      23  1.25067e+07  1.25085e+07
;      24  1.25085e+07  1.25086e+07
;      25  1.25086e+07  1.25094e+07
;      26  1.25102e+07  1.25108e+07
;      27  1.25109e+07  1.25111e+07
;      28  1.25111e+07  1.25112e+07
;      29  1.25112e+07  1.25111e+07
;      30  1.25112e+07  1.25112e+07
;      31  1.25112e+07  1.25112e+07
;      32  1.25112e+07  1.25112e+07
;      33  1.25112e+07  1.25112e+07
;      34  1.25112e+07  1.25112e+07
;      35  1.25117e+07  1.25117e+07
;      36  1.25118e+07  1.25118e+07
;      37  1.25118e+07  1.25119e+07
;      38  1.25119e+07  1.25142e+07
;      39  1.25148e+07  1.25148e+07
;      40  1.25156e+07  1.25160e+07
;      41  1.25035e+07  1.25035e+07
;      42  1.25094e+07  1.25094e+07
; new photon list size         8795
mktmflt,slist
; Enter name of time intervals output file 
; n4151_s1c3b_src.gti
; PHA values are in the range 
;       0    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 16.
; Number of bins =         4503
;Try different binning ?
; n
; Enter new min x and max x (0,0 to stop)
; 45000,55000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       48555.250       49336.160
;       12492528.       12493309.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       49479.328       50676.727
;       12493452.       12494650.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; Enter new min x and max x (0,0 to stop)
; 50000,60000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       52668.051       56247.234
;       12496641.       12500220.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       58763.500       59555.258
;       12502736.       12503528.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; Enter new min x and max x (0,0 to stop)
; 60000,65000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       60672.422       60883.914
;       12504645.       12504857.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       60986.953       61393.680
;       12504960.       12505367.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       61523.828       62039.016
;       12505497.       12506012.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; Enter new min x and max x (0,0 to stop)
; 60000,70000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       65173.480       65455.477
;       12509146.       12509428.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       66323.156       67071.523
;       12510296.       12511044.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; Enter new min x and max x (0,0 to stop)
; 65000,75000
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
;       71600.805       72056.336
;       12515574.       12516029.       4
; Use cursor to mark tstart and tend for this interval 
; Click near top to re-scale X-axis, below X-axis to finish 
; Click anywhere else to take next time-interval 
; i =       10
view,slist
; Enter GTI file name (or none) 
; n4151_s1c3b_src.gti
; PHA values are in the range 
;       0    2047
; Enter lower and upper inclusive PHA values for lightcurve 
; 0,500
; Enter time binsize in seconds 
; 32.
; Enter: 	0 = gis data 
;         1 = sis bright mode 
;         2 = sis faint mode 
; 1
; Number of bins =          731
; Number of bins in spectrum (must be power of 2 =< 1024) 
; 512
; sbsiz =        2
; Enter the number of the time region to adjust (0 to stop) 
; 0
; Finished fudging - rewriting GTI file n4151_s1c3b_src.gti
;       1       12492528.       12493309.
;       2       12493452.       12494650.
;       3       12496641.       12500220.
;       4       12502736.       12503528.
;       5       12504645.       12504857.
;       6       12504960.       12505367.
;       7       12505497.       12506012.
;       8       12509146.       12509428.
;       9       12510296.       12511044.
;      10       12515574.       12516029.
; Make PHA file from spectrum? (must be 512 or 1024) 
; y
; exposure time =       6808.00
;Enter filename (w/o extension): 
; n4151_s1c3b_src01
tmflt,slist,1
; size of photon list on entry         8795
; Enter ascii time intervals file name 
; n4151_s1c3b_src.gti
;       1  1.24925e+07  1.24933e+07
;       2  1.24935e+07  1.24946e+07
;       3  1.24966e+07  1.25002e+07
;       4  1.25027e+07  1.25035e+07
;       5  1.25046e+07  1.25049e+07
;       6  1.25050e+07  1.25054e+07
;       7  1.25055e+07  1.25060e+07
;       8  1.25091e+07  1.25094e+07
;       9  1.25103e+07  1.25110e+07
;      10  1.25156e+07  1.25160e+07
; new photon list size         6808
remhp,slist
;       1    4642
;       2    1341
;       3     471
;       4     200
;       5     101
;       6      35
;       7      13
;       8       4
;       9       1
;      10       1
;      11       1
;      12       1
;      13       1
;      14       1
;      15       1
;      16       1
;      17       1
;      18       1
;      19       1
;Enter desired cutoff: 
; 10
;Ready to modify plist? y/(n): 
; n
help
makeqdp,geo_s1c3b.bscl,slist,blist
; Enter filename containing PHA boundaries 
; phbnd.dat
; Enter output QDP filename 
; n4151_s1c3b_sb00
; Enter number of light curves required for each photon list 
; 5
; Enter bin size in seconds 
; 128.
; PHA boundaries: 
; Min and Max photon times:        12492615.       12516007.
; tmin tmax nbins      0.00000       23391.923       183.74940
$qdp n4151_s1c3b_sb00
$ls *.pha
$ls n4151*.pha
$ls *.idl
save,filename='n4151_s1c3B.idl'
