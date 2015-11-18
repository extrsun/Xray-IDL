pro plot_ctshk,plist,gti,fnames,hkname,binsize,phlo,phhi,pi,qname=qname
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,'plot_ctshk,plist,gti,fnames,hkname,binsize,phlo,phhi,pi,qname=qname'
 print,'Make a plot of count rate from a photon list vs HK param'
 retall
endif
if n_elements(qname) eq 0 then begin
 qname=' '
 read,' Enter name of QDP file to output results ',qname
endif
nfiles=(size(fnames))(1)
;read in arrays from mkfilter files
for k=0,nfiles-1 do begin
 tab=readfits(fnames(k),h,ext=1)
 timtmp=tbget(h,tab,'TIME')
 hktmp=tbget(h,tab,hkname)  
 if k eq 0 then begin
   hktime=timtmp & hkval=hktmp
 endif
 if k gt 0 then begin
   hktime=[hktime,timtmp] & hkval=[hkval,hktmp]
 endif
endfor
;make a light curve for the data
if pi eq 0 then $
wph=where((plist.pha ge phlo and plist.pha le phhi),nph)
if pi eq 1 then $ 
wph=where((plist.pi ge phlo and plist.pi le phhi),nph)
if nph le 0 then begin
 print,' No events in that PHA/PI range '
 retall
end
MKCURVE,plist.time,gti,binsize,nbins,tcen,tbounds,teff,ctspsec,ctserr
;now bin up the HK data like the light curve, taking the average value
;of the HK parameter
hkbin=fltarr(nbins)
openw,1,qname
printf,1,'READ SERR 1 2'
printf,1,'LA X ',hkname
printf,1,'LA Y cts/s'
printf,1,'LA T BINSIZE = ',binsize,' seconds'
printf,1,'MARKER 17 ON 1 2'
ymax=1.2*max(ctspsec+ctserr)
printf,1,'R Y 0',ymax
dum=0.
for k=0l,nbins-1l do begin
 whk=where((hktime ge tbounds(k,0) and hktime le tbounds(k,1)),nhk)
 if nhk gt 0 then hkbin(k)=total(hkval(whk))/float(nhk)
 printf,1,hkbin(k),dum,ctspsec(k),ctserr(k)
endfor
hmin=0.9*min(hkbin)
hmax=1.1*max(hkbin)
printf,1,'R X ',hmin,hmax
ploterr,hkbin,ctspsec,ctserr,psym=1 
for k=0l,nbins-1l do printf,1,hkbin(k),dum,ctspsec(k),ctserr(k)
close,1
return
end
