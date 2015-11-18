pro combxtelc,time,terr,cts,ctserr,index,detstr,cstr,qdpname=qdpname
;T. Yaqoob December 1996
if n_params(0) eq 0 then begin
 print,'combxtelc,time,terr,cts,ctserr,index,detstr,cstr,qdpname=qdpname'
 print,'Combine XTE light curves from different PCA detectors'
 print,'First Run subxtelc.pro to subract the background and produce'
 print,'IDL arrays cts(*,30) and ctserr(*,30)'
 print,'TIME - time array from SUBXTELC'
 print,'TERR - time bin halfwidth from SUBXTELC'
 print,'CTS  - Background-subtracted counts '
 print,'CTSERR - error on above '
 print,'INDEX - integer array containing indices (0-29) for combining'
 print,'	into one lightcurve. eg. index=[0,1,6,7] etc'
 print,'DETSTR - string specifying detector combination [plot label]'
 print,'CSTR   - string as part of combined lightcurve name'
 print,'QDPNAME - RootName of QDP file to write results to '
 print,'	The file will be called QDPNAME_cstr.qdp'
 print,' 	The routine will also write 5 more qdp files showing '
 print,'	lightcurves for all detectors seperately and will be '
 print,'	called QDPNAME_pcu0.qdp etc '
 retall
end
ntim=(size(time))(1)
ninx=(size(index))(1)
tcts=fltarr(ntim)
tctserr=tcts
if n_elements(qdpname) eq 0 then begin
 qdpname=' '
 read,'Enter Rootname of output QDP files ',qdpname
endif
;set up filenames 
fname=qdpname+'_'+cstr+'.qdp'
fnames=strarr(5)
print,'Individual Filenames will be: '
for i=0,4 do begin 
fnames(i)=qdpname+'_'+'pcu'+strtrim(i,2)+'.qdp'
print,fnames(i)
endfor
openw,1,fname
printf,1,'READ SERR 1 2'
printf,1,'SKIP SINGLE'
printf,1,'LA X Time (s)'
printf,1,'LA Y cts/s '
printf,1,'LA OT XTE PCA Combined detctors : ',detstr
for k=0l,ntim-1l do begin
 tcts(k)=0.0 & tctserr(k)=0.0
 for j=0l,ninx-1l do begin 
  tcts(k)=tcts(k)+cts(k,index(j))
  tctserr(k)=tctserr(k)+ctserr(k,index(j))*ctserr(k,index(j))
 endfor
 if tctserr(k) gt 0. then tctserr(k)=sqrt(tctserr(k)) else $
 tcsterr(k)=0.0
 printf,1,time(k),terr(k),tcts(k),tctserr(k)
endfor
close,1
;now write the individual files
;find count-rate limits
ymax=max(cts+ctserr)*1.2
ymin=-0.5
xmin=min(time) & xmax=max(time)
for j=0,4 do begin
openw,1,fnames(j)
printf,1,'READ SERR 1 2'
printf,1,'SKIP SINGLE '
printf,1,'PLOT VERTICAL'
printf,1,'win 1'
printf,1,'loc 0.1 0.6 0.5 0.85'
printf,1,'la nx off'
printf,1,'la y L1 c/s'
printf,1,'la ot ','PCU'+strtrim(j,2)
printf,1,'r',xmin,xmax,ymin,ymax 
printf,1,'win 2'
printf,1,'loc 0.5 0.6 0.9 0.85'
printf,1,'la nx off'
printf,1,'la y R1 c/s'
printf,1,'r',xmin,xmax,ymin,ymax 
printf,1,'win 3'
printf,1,'loc 0.1 0.35 0.5 0.6'
printf,1,'la nx off'
printf,1,'la y L2 c/s'
printf,1,'r',xmin,xmax,ymin,ymax 
printf,1,'win 4'
printf,1,'loc 0.5 0.35 0.9 0.6'
printf,1,'la nx off'
printf,1,'la y R2 c/s'
printf,1,'r',xmin,xmax,ymin,ymax 
printf,1,'win 5'
printf,1,'loc 0.1 0.1 0.5 0.35'
printf,1,'la x Time(s)'
printf,1,'la y L3 c/s '
printf,1,'r',xmin,xmax,ymin,ymax 
printf,1,'win 6'
printf,1,'loc 0.5 0.1 0.9 0.35'
printf,1,'la x Time(s)'
printf,1,'la y R3 c/s'
printf,1,'r',xmin,xmax,ymin,ymax 
for k=0,5 do begin
 ix = 6*j + k
 for i=0,ntim-1 do begin
  printf,1,time(i),terr(i),cts(i,ix),ctserr(i,ix)
 endfor
 printf,1,'NO NO NO NO '
endfor
close,1
endfor
return
end
