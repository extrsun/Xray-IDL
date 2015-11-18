pro multimanal,namekey,qdpfile=qdpfile,rmap=rmap,binwidths=binwidths,sigma=sigma,frac,fracerr,tmpfile=tmpfile
;Tahir Yaqoob June 1997
if n_params(0) eq 0 then begin
 print,'multimanal,namekey,qdpfile=qdpfile,rmap=rmap,binwidths=binwidths,sigma=sigma,frac,fracerr,tmpfile=tmpfile'
 print,'Run TIMANAL on all event files in current directory with name'
 print,'including the string NAMEKEY'
 print,'**INPUTS**'
 print,'NAMEKEY		- searches and uses all files in the current'
 print,'		- directory with the string NAMKEY in the name'
 print,'QDPFILE		- Name of output QDP file'
 print,' *OPTIONAL INPUTS* '
 print,'RMAP		- integer array with numbers indicating the'
 print,'		- following options:'
 print,'       0. SIS+GIS 0.5-10 keV for SIS, 0.7-10 keV for GIS'
 print,'       1. SIS(0.5-2 keV)+GIS(0.7-2.) keV'
 print,'       2. SIS(2-10 keV)+GIS(2-10) keV'
 print,'       3. SIS+GIS up to 4 keV'
 print,'       4. SIS+GIS up to 5 keV'
 print,'       5. SIS+GIS up to 6 keV'
 print,'       6. SIS+GIS up to 7 keV'
 print,'		- RMAP may have up to 7 elements'
 print,'		  default is RMAP=[0,1,2,3,4,5,6]'
 print,' *OPTIONAL INPUTS* '
 print,'BINWIDTHS	- integer array of binwidths  '
 print,'		  May have any number of elements'
 print,'		  Default is'
 print,'                  128 bins from 1 to 128 s'
 print,'		   64 bins from 130 to 258 s'
 print,'		   64 bins from 260 to 512 s'
 print,'		   35 bins from 544 to 1632 s'
 print,' ---NOTE--- Only GIS is used for binwidths less than 32 s' 
 retall
end
;routine will compute the following EPFs
; 	0. SIS+GIS 0.5-10 keV for SIS, 0.7-10 keV for GIS
;	1. SIS(0.5-2 keV)+GIS(0.7-2.) keV
;	2. SIS(2-10 keV)+GIS(2-10) keV
;	3. SIS+GIS up to 4 keV
;	4. SIS+GIS up to 5 keV
;	5. SIS+GIS up to 6 keV
;       6. SIS+GIS up to 7 keV
;now set up the pi-ranges and qdp comments for these
if n_elements(rmap) eq 0 then rmap=indgen(7)
nrange=(size(rmap))(1)
nmax=100l
qcom=strarr(nmax)
qcom(0)='SIS+GIS 0.5-10 keV for SIS, 0.7-10 keV for GIS'
qcom(1)='SIS(0.5-2 keV)+GIS(0.7-2.) keV'
qcom(2)='SIS(2-10 keV)+GIS(2-10) keV'
qcom(3)='SIS+GIS up to 4 keV'
qcom(4)='SIS+GIS up to 5 keV'
qcom(5)='SIS+GIS up to 6 keV'
qcom(6)='SIS+GIS up to 7 keV'
pi_l=fltarr(nmax,2) & pi_h=fltarr(nmax,2)
pi_l(0:nmax-1l,0)=140. & pi_l(0:nmax-1l,1)=60.     
pi_l(2,0)=565. & pi_l(2,1)=171.
pi_h(0,0)=1725. & pi_h(1,0)=564. & pi_h(2,0)=1725. & pi_h(3,0)=1074.
pi_h(4,0)=1214. & pi_h(5,0)=1354. & pi_h(6,0)=1494.
pi_h(0,1)=850. & pi_h(1,1)=170. & pi_h(2,1)=850. & pi_h(3,1)=340.
pi_h(4,1)=425. & pi_h(5,1)=510. & pi_h(6,1)=595. 
if n_elements(binwidths) eq 0 then begin
 binwidths=fltarr(91)
 binwidths(0:15)=1.+findgen(16)
 binwidths(16:23)=18.+2.*findgen(8)
 binwidths(24:55)=32.+16.*findgen(32)
 binwidths(56:90)=544.+32.*findgen(35)
endif
dummy=0.0
;find where the binwidths are less than 32s. Don't use less than 32s for SIS
wgis=where((binwidths lt 32.),nwgis)
openw,3,qdpfile
openw,4,'tempfile.dat'
printf,3,'READ SERR 1 2'
printf,3,'skip single'
printf,3,'r 0 1000 0 1'
printf,3,'mark 17 on 1..',nrange
printf,3,'la f '
printf,3,'la x BINWIDTH (s)'
printf,3,'la y Excess Pair Fraction (EPF)'
if n_elements(sigma) eq 0 then sigma=1.
evtfile=findfile('*'+strtrim(namekey,2)+'*.evt',count=count)
print,'Number of event files: ',count
nb=(size(binwidths))(1)
;for i=0l,1l do begin
for ii=0l,nrange-1l do begin
 i=rmap(ii)
 totfrac= fltarr(nb) & totfracerr=totfrac 
 ngis=0.0 & nsis=0.0 
for j=0l,count-1l do begin
 if n_elements(tmpfile) gt 0 then qdpname='timanal_temp'+strtrim(string(j+1),2)+'.qdp'
 fname=strtrim(evtfile(j),2) 
 qreadasca,fname,plist,h,gti,tab
;find out if this is SIS or GIS
 ikey=sxpar(h,'instrume')
 if strmid(ikey,0,3) eq 'SIS' then begin
	inst=0 
	nsis=nsis+1.0
 endif
 if strmid(ikey,0,3) eq 'GIS' then begin
	inst=1
	ngis=ngis+1.0
 endif 
;setup the pi channels
 pilo=pi_l(i,inst) & pihi=pi_h(i,inst)
 timanal,plist,gti,pilo,pihi,binwidths,sigma,frac,fracerr,qdpname=qdpname
;if SIS and binwidths are too small, replace with zeros
 if inst eq 0 and nwgis gt 0 then begin
  frac(wgis)=0.0 & fracerr(wgis)=0.0
 endif
 totfrac=totfrac+frac
 totfracerr=totfracerr+fracerr*fracerr
endfor
nfiles=nsis+ngis+fltarr(nb)
if nwgis gt 0 then nfiles(wgis)=ngis
for jj=0l,nb-1l do printf,4,binwidths(jj),nfiles(jj)
totfracerr=sqrt(totfracerr)/nfiles
totfrac=totfrac/nfiles
printf,3,'!'+qcom(i)
printf,3,'!SIS PI channel range ',pi_l(i,0),pi_h(i,0)
printf,3,'!GIS PI channel range ',pi_l(i,1),pi_h(i,1)
for k=0l,nb-1l do printf,3,binwidths(k),dummy,totfrac(k),totfracerr(k)
printf,3,'NO NO NO NO'
endfor
close,3
close,4
return
end
