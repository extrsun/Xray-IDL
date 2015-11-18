pro asca4instbin,infile,outfile,e1,bin1,e2,bin2,bintot
;Author T. Yaqoob
;DATE- sometime in 1995
if n_params(0) eq 0 then begin
 print,'asca4instbin,infile,outfile,e1,bin1,e2,bin2,bintot'
 print,'Take a QDP file for data/model ratios for 4 ASCA instruments '
 print,'(2 SIS and 2 GIS in that order) and bin it up using ASCABIN '
 print,'so that the bin widths are approx equal to the instrument resolutions'
 print,'Will also produce a combined plot for 4 instruments '
 print,'with binning bintot '
 retall
end
readcol,infile,e,eerr,r,rerr
;set up bins for combined 4 inst plot
emin=min(e) & emax=max(e)
ntbins=fix((emax-emin)/bintot)
etl=emin+findgen(ntbins)*bintot
eth=etl+bintot
etcen=0.5*(etl+eth)
totrat=fltarr(ntbins) & totrate=totrat
openw,3,'tot'+outfile
printf,3,'read serr 1 2'
printf,3,'marker 17 on 1 2'
for j=0l,ntbins-1l do begin
 wj=where((e ge etl(j) and e lt eth(j) and rerr gt 0.),nwj)
 if nwj gt 0 then begin
   rat=r(wj)
   resq1=1./rerr(wj)/rerr(wj)
   totrat(j)=total(rat*resq1)/total(resq1)
   totrate(j)=sqrt(1./total(resq1))
   printf,3,etcen(j),0.5*bintot,totrat(j),totrate(j)
 endif
endfor
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
openw,2,outfile
printf,2,'READ SERR 1 2'
printf,2,'skip single'
printf,2,'r 0.3 11 0.5 1.7'
printf,2,'log x'
for j=0,3 do begin
 i1=ilo(j) & i2=ihi(j)
 if j le 1 then inst=0 else inst=1
 ascabin,inst,e1,bin1,e2,bin2,e(i1:i2),eerr(i1:i2),r(i1:i2),rerr(i1:i2),$
 eout,eouterr,rout,routerr,iout
 for k=0l,iout-1l do printf,2,eout(k),eouterr(k),rout(k),routerr(k)
 printf,2,'NO NO NO NO'
endfor
close,2
close,3
end
