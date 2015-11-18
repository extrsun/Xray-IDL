pro asca4instbin3,infile,outfile,bintot,wgt=wgt,e2,bintot2,indiv=indiv
if n_params(0) eq 0 then begin
 print,'asca4instbin3,infile,outfile,bintot,wgt=wgt'
 print,'Take a QDP file for data+model for 4 ASCA instruments '
 print,'(2 SIS and 2 GIS in that order) and produce a combined plot for 4 
 print,'instruments with binning bintot '
 print,'If wgt is set, then bintot is weighted by 1/sigma^2'
 print,'If e2, bintot2 are given, then binsize=bintot2 for E>e2'
 print,'/indiv - do not combine instruments after binning'
 retall
end
readcol,infile,e,eerr,d,derr,m
;set up bins for combined 4 inst plot
emin=min(e) & emax=max(e)
if n_elements(e2) eq 0 then e2=emax
if n_elements(bintot2) eq 0 then bintot2=bintot
ntbins1=fix((e2-emin)/bintot)
ntbins2=fix((emax-e2)/bintot2)
ntbins = ntbins1+ntbins2
if ntbins2 eq 0 then begin
 etl=emin+findgen(ntbins)*bintot
 eth=etl+bintot
endif else begin
 etl=[emin+findgen(ntbins1)*bintot,e2+findgen(ntbins2)*bintot2]
 eth=[etl(0:ntbins1-1)+bintot,etl(ntbins1:ntbins1+ntbins2-1)+bintot2]
;make sure there isn't a gap
 eth(ntbins1-1)=etl(ntbins1)
endelse
etcen=0.5*(etl+eth)
; bin each instr. separately first
totrat=fltarr(ntbins,4) & totrate=totrat
;now find out where the instrument boundaries are
ilo=lonarr(4) & ihi=lonarr(4)
ntot=(size(e))(1)
ilo(0)=0l & ihi(3)=ntot-1l
ix=0
for k=1l,ntot-1l do begin
 if e(k) lt e(k-1) then begin  
   ihi(ix)=k-1
   ilo(ix+1)=k
   ix=ix+1
 endif
endfor
forprint,ilo,ihi

openw,3,outfile
printf,3,'read serr 1 2'
printf,3,'skip single'
printf,3,'marker 17 on 1 2'
if keyword_set(wgt) then printf,3,'# Weighting by 1/sigma^2'
for j=0l,ntbins-1l do begin
 for k=0,3 do begin
  i1=ilo(k) & i2=ihi(k)
  wj=where(e(i1:i2) ge etl(j) and e(i1:i2) lt eth(j) and derr(i1:i2) gt 0.,$
   nwj)
  if nwj gt 0 then begin
   desq1=(1./derr(wj+i1))^2
   if not keyword_set(wgt) then desq1(*)=1.0
   totmod = total(m(wj+i1)*desq1)
   totrat(j,k)=total(d(wj+i1)*desq1)/totmod
   totrate(j,k)=sqrt(total((derr(wj+i1)*desq1)^2))/totmod
  endif
 endfor
 if not keyword_set(indiv) then printf,3,etcen(j),0.5*(eth(j)-etl(j)),$
   avg(totrat(j,*)),sqrt(total(totrate(j,*)^2))/4
endfor
if keyword_set(indiv) then begin
 for k=0,3 do begin
  for j=0,ntbins-1 do printf,3,etcen(j),0.5*(eth(j)-etl(j)),$
    totrat(j,k),totrate(j,k)
  printf,3,'no no no no'
 endfor
endif
close,3
end
   



