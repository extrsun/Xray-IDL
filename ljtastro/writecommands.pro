pro writecommands

if 0 then begin
;for extract spectra
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/extractspectra.xcm'
files='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/regions2/*.reg'
thesefiles=findfile(files,count=numfiles)

print,numfiles

openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,'punlearn specextract'
printf,lun1,'specextract infile="acisf02762_clean1.fits[sky=region('+strmid(thesefiles[i],57,31)+')]" outroot='+strmid(thesefiles[i],57,27)
printf,lun1,''
endfor

close,lun1
free_lun,lun1
endif


if 0 then begin
;for rebin spectra
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/rebinspectra.xcm'
files='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/spectra/X*src1.pi'
thesefiles=findfile(files,count=numfiles)

print,numfiles

openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,'punlearn dmgroup'
printf,lun1,'dmgroup infile='+strmid(thesefiles[i],56,35)+' outfile='+strmid(thesefiles[i],56,28)+$
'SNR3.pi grouptype=SNR xcolumn=pi grouptypeval=3 binspec="" ycolumn=counts'
printf,lun1,''
endfor

close,lun1
free_lun,lun1
endif


if 0 then begin
;for giving background spectra
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/givebgspectra.xcm'
files='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/spectra/*src1.pi'
thesefiles=findfile(files,count=numfiles)
backgroundspectrum='src_bkg1.pi'

openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,'punlearn dmhedit'
printf,lun1,'dmhedit infile='+strmid(thesefiles[i],56,42)+' filelist="" operation=add key=BACKFILE value='+$
backgroundspectrum
printf,lun1,''
endfor

close,lun1
free_lun,lun1
endif


if 0 then begin
;for spectra fitting(2vmekal+power)
commandsname='/home/ljt/ljt/data/SNR/Kepler/spectra/spectra/fitspectra.xcm'
files='/home/ljt/ljt/data/SNR/Kepler/spectra/spectra/X*SNR4.pi'
thesefiles=findfile(files,count=numfiles)
backgroundspectrum='Kepler_bg_sou.pi'

print,numfiles

openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,'data '+strmid(thesefiles[i],46,35)
printf,lun1,'setpl en'
printf,lun1,'ignore 0.0-0.3 10.0-**'
printf,lun1,'mo wa (vmekal+vmekal+power)'
printf,lun1,'/*'
printf,lun1,'freeze 40 41'
printf,lun1,'new 41 0'
printf,lun1,'new 2 0.2'
printf,lun1,'new 21 0.7'
printf,lun1,'thaw 12 13 16 18 32 33 35 37'
printf,lun1,'reno'
printf,lun1,'fit 10000'
printf,lun1,'thaw 40 41'
printf,lun1,'new 41 5e-7'
printf,lun1,'reno'
printf,lun1,'fit 10000'
printf,lun1,'pl ldata delchi'
printf,lun1,'save all '+strmid(thesefiles[i],46,32)+'_2vmekalpower.xcm'
printf,lun1,''
endfor

close,lun1
free_lun,lun1
endif


if 0 then begin
;for spectra fitting(vpshock+power)
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/fitspectra.xcm'
files='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/spectra/*SNR3.pi'
thesefiles=findfile(files,count=numfiles)


print,numfiles

openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,'data '+strmid(thesefiles[i],56,35)
;printf,lun1,'setpl en'
;printf,lun1,'setpl add'
printf,lun1,'ignore 0.0-0.3 8.0-**'
printf,lun1,'mo wa (vpshock)'
printf,lun1,'/*'
printf,lun1,'new 5 0.3'
printf,lun1,'new 6 0.3'
printf,lun1,'new 7 0.3'
printf,lun1,'new 8 0.3'
printf,lun1,'new 9 0.3'
printf,lun1,'new 10 0.3'
printf,lun1,'new 11 0.3'
printf,lun1,'new 12 0.3'
printf,lun1,'new 13 0.3'
printf,lun1,'new 14 0.3'
printf,lun1,'new 15 0.3'
printf,lun1,'new 1 0.2'
printf,lun1,'new 18 0.00093'
printf,lun1,'thaw 7 8 9 10 11 14'
printf,lun1,'reno'
printf,lun1,'fit 10000'
printf,lun1,'error 1 2 7 8 9 10 11 14 17 19'
printf,lun1,'pl ldata delchi'
printf,lun1,'save all '+strmid(thesefiles[i],56,32)+'_0.3-8.0keV.xcm'
printf,lun1,'iplot'
printf,lun1,'hardcopy '+strmid(thesefiles[i],56,32)+'_0.3-8.0keV.ps/ps'
printf,lun1,'quit'
printf,lun1,'flux 0.3 8.0'
printf,lun1,'new 7 0'
printf,lun1,'flux 0.3 8.0'
printf,lun1,'new 8 0'
printf,lun1,'flux 0.3 8.0'
printf,lun1,'new 9 0'
printf,lun1,'flux 0.3 8.0'
printf,lun1,'new 10 0'
printf,lun1,'flux 0.3 8.0'
printf,lun1,'new 11 0'
printf,lun1,'flux 0.3 8.0'
printf,lun1,'new 14 0'
printf,lun1,'flux 0.3 8.0'
printf,lun1,''
endfor

close,lun1
free_lun,lun1
endif


if 0 then begin
;for spectra fitting(1.5-5keV)(nH+power+2 gauss)
commandsname='/home/ljt/ljt/data/SNR/Kepler/spectra/spectra/fitspectra4.xcm'
files='/home/ljt/ljt/data/SNR/Kepler/spectra/spectra/X*SNR3.pi'
thesefiles=findfile(files,count=numfiles)
backgroundspectrum='Kepler_bg_sou.pi'

print,numfiles

number=0
openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
counts=fix(strmid(thesefiles[i],68,5))
xcoor=fix(strmid(thesefiles[i],48,4))
ycoor=fix(strmid(thesefiles[i],54,4))
if xcoor ge 4162 then begin
if counts ge 1000 or counts eq 0 then begin
printf,lun1,'data '+strmid(thesefiles[i],46,35)
printf,lun1,'setpl en'
printf,lun1,'ignore 0.0-1.5 5.0-**'
printf,lun1,'mo wa (power+gauss+gauss)'
printf,lun1,'/*'
printf,lun1,'new 4 1.9'
printf,lun1,'new 5 0.02'
printf,lun1,'new 7 2.4'
printf,lun1,'reno'
printf,lun1,'fit 10000'
printf,lun1,'pl ldata delchi'
printf,lun1,'save all '+strmid(thesefiles[i],46,32)+'_power2gauss.xcm'
printf,lun1,''
number=number+1
endif 
endif
endfor

print,number

close,lun1
free_lun,lun1
endif



if 0 then begin
;for getting the reduced Chi-square value.
commandsname='/home/ljt/ljt/data/SNR/Kepler/spectra/backgroundspectra/getchi.xcm'
number=681
beginvalue=1.2

openw,lun1,commandsname,/get_lun
for i=0,number-1 do begin
range=beginvalue+0.01*i
printf,lun1,'ignore energy 0.0:'+string(range)
printf,lun1,'goodness'
endfor

close,lun1
free_lun,lun1
endif



if 0 then begin
;for writing the spectral region for radial profile.
file='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/radialprofile/radial.reg'
outputname='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/radialprofile/'
number=60
headstr='ljt'
str1='ljt'


openr,lun2,file,/get_lun

readf,lun2,headstr
for i=0,number/3-1 do begin
for j=0,2 do begin
 readf,lun2,str1
 regionname='Degree'+strtrim(string(j),2)+'Radius'+strtrim(string(100+i),2)
 outputregionname=outputname+regionname+'.reg'
 openw,lun1,outputregionname,/get_lun
   printf,lun1,headstr
   printf,lun1,str1
 close,lun1
 free_lun,lun1
endfor
endfor

close,lun2
free_lun,lun2

endif



if 0 then begin
;for Tomography.
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/tomography/bin8_4-4.5_4.5-6_index-4.4--2/tomography_tom12.xcm'
lowerindex=-0.8
upperindex=-0.6
number=20
width=(upperindex-lowerindex)/float(number)

openw,lun1,commandsname,/get_lun
for i=0,number-1 do begin
index=lowerindex+width*i
multinumber=(5.25/4.25)^index
printf,lun1,'punlearn dmimgcalc'
printf,lun1,'dmimgcalc Kepler_4-4.5keV_bin8_sigmin3_multi2.fits none Kepler_4-4.5keV_bin16_multi2_multi'+strtrim(multinumber,2)+$
'.fits add weight='+strtrim(multinumber,2)
printf,lun1,'dmimgcalc Kepler_4.5-6keV_bin8_sigmin3_multi0.67.fits Kepler_4-4.5keV_bin16_multi2_multi'+strtrim(multinumber,2)+'.fits '+$
'Tomography_index'+strmid(strtrim(index,2),0,5)+'.fits sub'
printf,lun1,'dmcopy "Tomography_index'+strmid(strtrim(index,2),0,5)+'.fits[sky=region(total.reg)]" Tomography_index'+$
strmid(strtrim(index,2),0,5)+'_total.fits'
printf,lun1,''
endfor

close,lun1
free_lun,lun1
endif





if 0 then begin
;for dmcopy.
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/tomography/bin8_4-4.5_4.5-6_index-4.4--2/dmcopyregion.xcm'
files='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/tomography/bin8_4-4.5_4.5-6_index-4.4--2/tom12/Tomography*total.fits'
thesefiles=findfile(files,count=numfiles)

print,numfiles

openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,'punlearn dmcopy'
printf,lun1,'dmcopy "'+strmid(thesefiles[i],98,32)+'[sky=region(tom12.reg)]" '+strmid(thesefiles[i],98,21)+'_tom12.fits'
printf,lun1,''
endfor

close,lun1
free_lun,lun1

endif




if 0 then begin
;for source counts.
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/tomography/bin8_4-4.5_4.5-6_index-4.4--2/TomRegCts2.xcm'
files='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/tomography/bin8_4-4.5_4.5-6_index-4.4--2/tom*.reg'
thesefiles=findfile(files,count=numfiles)

print,numfiles

beginnumber=92
numberlong=9

openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,strmid(thesefiles[i],beginnumber,numberlong)
printf,lun1,'dmlist "Kepler_source.fits[sky=region('+strmid(thesefiles[i],beginnumber,numberlong)+')]" counts'
;printf,lun1,'dmlist "Kepler_source.fits[sky=region('+strmid(thesefiles[i],78,38)+')][energy=500:800]" counts'
;printf,lun1,'dmlist "Kepler_source.fits[sky=region('+strmid(thesefiles[i],78,38)+')][energy=1000:1150]" counts'
;printf,lun1,'dmlist "Kepler_source.fits[sky=region('+strmid(thesefiles[i],78,38)+')][energy=800:1000]" counts'
;printf,lun1,'dmlist "Kepler_source.fits[sky=region('+strmid(thesefiles[i],78,38)+')][energy=1410:1680]" counts'
;printf,lun1,'dmlist "Kepler_source.fits[sky=region('+strmid(thesefiles[i],78,38)+')][energy=1950:2100]" counts'
;printf,lun1,'dmlist "Kepler_source.fits[sky=region('+strmid(thesefiles[i],78,38)+')][energy=1710:1950]" counts'
printf,lun1,''
endfor

close,lun1
free_lun,lun1

endif




if 0 then begin
;for nt-tau figure.
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/Fe.xcm'
number=50
temperature=double(10^(findgen(number)*0.06-1.))
;temperature=[0.50,0.60,0.70,0.80,0.90,1.00,2.00,3.00,4.00,5.00,6.00,7.00,8.00,9.00,10.0]

openw,lun1,commandsname,/get_lun
printf,lun1,'data src_SNR5.pi'
printf,lun1,'ignore 0.0-0.3 2.0-**'
printf,lun1,'mo wa (vpshock)'
printf,lun1,'/*'
printf,lun1,'new 5 0.3'
printf,lun1,'new 6 0.3'
printf,lun1,'new 7 0.3'
printf,lun1,'new 8 0.3'
printf,lun1,'new 9 0.3'
printf,lun1,'new 10 0.3'
printf,lun1,'new 11 0.3'
printf,lun1,'new 12 0.3'
printf,lun1,'new 13 0.3'
printf,lun1,'new 14 0.3'
printf,lun1,'new 15 0.3'
printf,lun1,'new 1 0.2'
printf,lun1,'new 18 0.00093'
printf,lun1,'thaw 14'
printf,lun1,'reno'
printf,lun1,'fit 10000'
printf,lun1,'freeze 2'
for i=0,number-1 do begin
if temperature[i] gt 0.2 and temperature[i] lt 80 then begin
printf,lun1,'new 2 '+strtrim(string(temperature[i]),2)
printf,lun1,'reno'
printf,lun1,'fit 1000'
printf,lun1,'save all Fe'+strtrim(string(temperature[i]),2)+'_0.30-2.00.xcm'
printf,lun1,''
endif
endfor

close,lun1
free_lun,lun1

endif





if 0 then begin
;for flux calculating.
commandsname='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/aidfile/calcflux3b.xcm'
files='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/spectra/*SNR3.pi'
thesefiles=findfile(files,count=numfiles)


print,numfiles

openw,lun1,commandsname,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,'data '+strmid(thesefiles[i],64,35)
printf,lun1,'@'+strmid(thesefiles[i],64,32)+'_0.4-2.68keV.xcm'
printf,lun1,'flux 0.4 2.68'
printf,lun1,'new 7 0'
printf,lun1,'flux 0.4 2.68'
;printf,lun1,'new 8 0'
;printf,lun1,'flux 0.4 2.68'
printf,lun1,'new 9 0'
printf,lun1,'flux 0.4 2.68'
printf,lun1,'new 10 0'
printf,lun1,'flux 0.4 2.68'
printf,lun1,'new 11 0'
printf,lun1,'flux 0.4 2.68'
printf,lun1,'new 14 0'
printf,lun1,'flux 0.4 2.68'
printf,lun1,'new 5 0'
printf,lun1,'new 6 0'
printf,lun1,'new 8 0'
printf,lun1,'new 12 0'
printf,lun1,'new 13 0'
printf,lun1,'new 15 0'
printf,lun1,'flux 0.4 2.68'
printf,lun1,''
endfor

close,lun1
free_lun,lun1
endif


end
