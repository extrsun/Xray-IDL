pro readxtelc,fname,rootname,nlqc,exp,binwid
if n_params(0) eq 0 then begin
 print,'readxtelc,fname,rootname,nlqc,exp,binwid'
 print,'Read a FITS lightcurve file and write results to QDP file'
 print,'FNAME 	- Name of input FITS file '
 print,'ROOTNAME - rootname of output QDP file ' 
 print,'NLQC - Number of light curves to write '
 print,'EXP - only write bins with fractional exposure > exp '
 print,'BINWID - bin width in seconds '
 print,'If NLQC = 30 then 5 output files will be written , 1 for ' 
 print,'each XTE PCU and the names will be rootname_p{0..4}.qdp ' 
 retall
endif
sfx=strarr(30) & pcus=strarr(5)
pcus=['0','1','2','3','4']
sfx=['','2','3','4','5','6','7','8','9','10','11','12','13','14' $
,'15','16','17','18','19','20','21','22','23','24','25','26','27', $
'28','29','30']
tableget,fname,time,'time',1,xte=1
ntim=(size(time))(1)
cts=fltarr(ntim,nlqc) & fracexp=cts & ctserr=cts
tempcts=fltarr(ntim)
for k=0l,nlqc-1l do begin 
 pname1='RATE'+sfx(k)
 pname2='FRACEXP'+sfx(k)
 pname3='ERROR'+sfx(k)
tableget,fname,tempcts,pname1,1,xte=1
 cts(0:ntim-1,k)=tempcts
tableget,fname,tempcts,pname2,1,xte=1
 fracexp(0:ntim-1,k)=tempcts
tableget,fname,tempcts,pname3,1,xte=1
 ctserr(0:ntim-1,k)=tempcts
endfor
if nlqc ne 30 then begin
 openw,1,rootname+'.qdp'
 printf,1,'READ SERR 1 2 '
 printf,1,'SKIP SINGLE '
 printf,1,'LA x Time '
 printf,1,'LA y cts/s '
 printf,1,'LA OT Lightcurves (pcux[L1 R1 L2 R2 L3 R3]) [x:0..4])'
 for k=0l,nlqc-1l do begin
  for j=0l,ntim-1l do begin
  if fracexp(j,k) ge exp then begin
   hw=0.5*fracexp(j,k)*binwid
   printf,1,time(j),hw,cts(j,k),ctserr(j,k)
  endif
  endfor
 printf,1,'NO NO NO NO'
 endfor
 close,1
endif
if nlqc eq 30 then begin
 for i=0,4 do begin
  openw,1,rootname+'_pcu'+pcus(i)+'.qdp'
  printf,1,'READ SERR 1 2'
  printf,1,'SKIP SINGLE '
  printf,1,'PLOT VERTICAL '
  printf,1,'LA X Time '
  printf,1,'LA OT PCU'+pcus(i)
  printf,1,'win 1'
  printf,1,'LA Y L1 cts/s '
  printf,1,'win 2'
  printf,1,'la y R1 cts/s '
  printf,1,'win 3'
  printf,1,'LA Y L2 cts/s '
  printf,1,'win 4'
  printf,1,'la y R2 cts/s '
  printf,1,'win 5'
  printf,1,'LA Y L3 cts/s '
  printf,1,'win 6'
  printf,1,'la y R3 cts/s '
  for k=0l,5 do begin 
   kqc=6*i+k
   for j=0l,ntim-1l do begin
   hw=0.5*fracexp(j,kqc)*binwid
   printf,1,time(j),hw,cts(j,kqc),ctserr(j,kqc)
   endfor
   printf,1,'NO NO NO NO '
  endfor
  close,1
 endfor
endif
return
end 
