pro mkascasmry,sbit,sext,dir=dir,rootname=rootname,xcen,ycen,radsrc,theta,phi,expt,ctspsec
if n_params(0) eq 0 then begin
print,'mkascasmry,sbit,sext,dir=dir,rootname=rootname,xcen,ycen,radsrc,theta,phi,expt,ctspsec'
 print,'After running automated IDL/XSELECT script run this program'
 print,'to produce a concise summary of the data products '
 print,'The spectral and region filenames are expected to have '
 print,'ROOTNAME in common.'
 print,'**INPUTS **'
 print,'SBIT	- string which identifies source files e.g. src '
 print,'SEXT	- string which identifies the spectral extenstion e.g. pha'
 print,'DIR	- directory containing spectral and SAOimage region files'
 print,'ROOTNAME - common rootname for all spectral and reg files'
 print,' **OUTPUTS** '
 print,'XCEN,YCEN - source extraction region centers '
 print,'RADSRC	  - source extraction radius '
 print,'EXPT	  - exposure time'
 print,' XIMAGE SCRIPT - the routine also writes a file ximage.script'
 print,' 	    which allows the user to examine the WMAPs of all'
 print,'	    the spectral files to make sure all the region'
 print,'	    selection went ok'
 retall
end
if n_elements(dir) eq 0 then dir=''
;set up conversion factors for det coords to arcmin
plate_scale=0.98220003
cfac=fltarr(4)
cfac(0:1)=0.027*plate_scale
cfac(2:3)=0.25*plate_scale
;first find the source extraction region files
regnames=findfile(dir+rootname+'*'+sbit+'*.reg',count=nreg)
print,'NREG = ',nreg
;forprint,regnames
;now find the source spectral files 
specnames=findfile(dir+rootname+'*'+sbit+'.'+sext,count=nspec)
bbit='bgd'
bgdnames=findfile(dir+rootname+'*'+bbit+'.'+sext,count=nspec)
print,'NSPEC = ',nspec
;forprint,specnames
if nreg ne nspec then begin
 print,'Number of spectral files does not match number of '
 print,'region files ** ABORTING **'
 return
end   
xcen=fltarr(nspec) & ycen=fltarr(nspec) & radsrc=fltarr(nspec)
srccts=xcen & expt=xcen  & theta=xcen & phi=xcen & det=xcen 
ctspsec=xcen
openw,1,rootname+'_regsummary.txt'
openw,2,rootname+'_specsummary.txt'
openw,5,rootname+'_offaxis.qdp'
printf,5,'SKIP SINGLE'
printf,5,'!1st group is SIS x y
printf,5,'!2nd group is GIS x y
printf,5,'!3rd group is XRT theta and phi
printf,1,'SUMMARY OF SOURCE EXTRACTION REGIONS '
printf,1,' '
str1='SEQ      INST   XCEN    YCEN   RADIUS   THETA    PHI'
printf,1,str1 
printf,1,' '
printf,2,'SUMMARY OF SOURCE EXTRACTION SPECTRA '
printf,2
str1='SEQ      INST CCD        START        STOP         EXPOSURE'
str1=str1+'   COUNTS      CTS/S'
printf,2,str1
printf,2,' '
openw,4,rootname+'ximage.script'
;for j=0l,2l do begin
for j=0l,nspec-1l do begin
;interpret saoimage region files; assumes the first line is the
;source circle extraction parameters
 openr,3,regnames(j)
 dummy=' '
rdf: readf,3,dummy
 if strmid(dummy,0,1) eq '#' then goto, rdf
 pstr1=str_sep(dummy,'(')  & reads,pstr1(1),xc,yc,rs
 xcen(j)=xc & ycen(j)=yc & radsrc(j)=rs
; now get parameters from spectra
 tab=readfits(specnames(j),h)
 w1=where(strmid(h,0,5) eq 'TSTAR')
 reads,(str_sep((h(w1))(0),'='))(1),tstart
 w1=where(strmid(h,0,5) eq 'TSTOP')
 reads,(str_sep((h(w1))(0),'='))(1),tstop
 get_asca_date,tstart,mo,day,year,hr,min,sec,cdate1,ctime1
 get_asca_date,tstop,mo,day,year,hr,min,sec,cdate2,ctime2
 w1=where(strmid(h,0,5) eq 'EXPOS')
 reads,(str_sep((h(w1))(0),'='))(1),texp
 expt(j)=texp
 instr=sxpar(h,'INSTRUME')
 seqno=strtrim(sxpar(h,'SEQNUM'),2)
 reads,strmid(instr,3,1),inst & det(j)=inst
 ccdid='     '
 w1=where(strmid(h,0,6) eq 'OPTIC1')
 reads,(str_sep((h(w1))(0),'='))(1),optx 
 w1=where(strmid(h,0,6) eq 'OPTIC2')
 reads,(str_sep((h(w1))(0),'='))(1),opty
;now compute the off-axis theta and phi corresponding to the extraction
;region center (which should be the source centroid)
 xdist=xc-optx & ydist=yc-opty
 theta(j)=cfac(inst)*sqrt(xdist*xdist+ydist*ydist)
 if xdist ne 0. then phi(j)=atan(ydist/xdist)*180./!pi
 if phi(j) lt 0. then phi(j)=phi(j)+360.
; fm1='(A8,1X,A4,1X,3(F7.2,1X),F8.5,1X,F6.2)'
 fm1='(A8,1X,A4,1X,3(F7.2,1X),F8.5,1X,F6.2,2(F7.2,1X))'
 printf,1,format=fm1,seqno,instr,xc,yc,rs,theta(j),phi(j),optx,opty
 if strmid(instr,0,4) eq 'SIS0' then ccdid=sxpar(h,'S0CCDPOW')
 if strmid(instr,0,4) eq 'SIS1' then ccdid=sxpar(h,'S1CCDPOW') 
 tab=readfits(specnames(j),h,ext=1)
 w1=where(strmid(h,0,5) eq 'TOTCT')
 reads,(str_sep((h(w1))(0),'='))(1),totct
 fm2='(A8,1X,A4,1X,A4,1X,2(A8,1X,A5,1X),F10.2,1X,F10.2,1X,F8.2)'
 printf,2,format=fm2,seqno,instr,ccdid,cdate1,ctime1,cdate2,ctime2,$
texp,totct,totct/texp
 ctspsec(j)=totct/texp
 close,3
;now write the ximage script
 printf,4,'read/fits ',specnames(j)
 printf,4,'display'
 printf,4,'read/fits ',bgdnames(j)
 printf,4,'display'
endfor
close,1
close,2
close,4
;write out position file
for k=0l,nspec-1l do begin
 if det(k) le 1 then printf,5,xcen(k),ycen(k)
endfor
printf,5,'NO NO '
for k=0l,nspec-1l do begin
 if det(k) gt 1 then printf,5,xcen(k),ycen(k)
endfor
printf,5,'NO NO '
for k=0l,nspec-1l do begin
 printf,5,theta(k),phi(k)
endfor
printf,5,'NO NO'
close,5
return
end
