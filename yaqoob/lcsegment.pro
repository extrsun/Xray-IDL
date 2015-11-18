pro lcsegment,qdpname,ncrit,critconf
if n_params(0) eq 0 then begin
 print,'lcsegment,qdpname,ncrit,critconf'
 print,'Take a QDP lightcurve with equal bin sizes and compute'
 print,'The number of contiguos segments and the chi-sq against'
 print,'constant hypothesis for each segment'
 print,'NCRIT is a critical minimum data points a segement must '
 print,'have.'
 print,'CRITCONF is the minium percentage confidence level for '
 print,'the chi-sq test for a segement shows variability'
 retall
end
readcol,qdpname,time,tw,cts,ctserr
npts=(size(cts))(1)
print,'# of data points : ',npts
tbin=2.*tw(0)
print,'Bin Width : ',tbin
nseg=lonarr(npts)
data=fltarr(npts)
err=fltarr(npts)
iseg=0l
idata=0l
finish=0
npass=0l
openw,1,'seg_'+qdpname
for k=0l,npts-1l do begin
  data(idata)=cts(k) & err(idata)=ctserr(k)
 if k lt (npts-1l) then begin
  if (time(k+1)-time(k)) gt 1.01*tbin then finish=1
 endif
 if k eq npts-1l then finish=1
 if finish gt 0 then begin
  print,'Finihsed Segment ',iseg
  print,'Found ',idata+1l,' data points in this segment'
  if idata gt 0 then begin
   chilc,data(0:idata),err(0:idata),chisq,pchi
   print,'Chi-sq and Confidence level for this segment: ',chisq,pchi
  endif
  cseg=' '
  if n_elements(pchi) gt 0 then begin
  if idata ge ncrit and pchi ge critconf then begin
   cseg='*' 
   npass=npass+1l
  endif 
  endif
  if idata gt 0 then printf,1,strtrim(cseg,2),iseg+1,idata+1l,chisq,pchi else $
   printf,1,strtrim(cseg,2),iseg+1,idata+1l
  iseg=iseg+1l & idata=0l
  finish=0 
 endif else begin
  idata=idata+1l
 endelse
endfor
printf,1,'Number of segments for which there are at least ',ncrit  
printf,1,'bins and the confidence level for variability is at least '
printf,1,critconf,' percent is ',npass
close,1
return
end 
