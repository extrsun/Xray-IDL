pro chkdrift,plist,gti,phlo,phhi,pi,binsize,postyp,gistyp,qname=qname
if n_params(0) eq 0 then begin
 print,'chkdrift,plist,gti,phlo,phhi,pi,binsize,postyp,gistyp,qname=qname'
 print,'check photon list to see whether spectral variations'
 print,'are due to variation in the source off-axis angle'
 print,'INPUTS: '
 print,'GTI(*,2) time intervals for light curve '
 print,'PHLO PHHI high and low PHA/PI channels '
 print,'PI =0 for PHA =1 for PI'
 print,'BINSIZE bin width in seconds for light curve '
 print,' Currently should only be used for GIS'
 retall
endif
if n_elements(qname) eq 0 then begin
 qname=' '
 read,'Enter rootname of QDP file to dump results to ',qname
endif
if pi eq 0 then slist=plist(where(plist.pha gt phlo and plist.pha le phhi))
if pi gt 0 then slist=plist(where(plist.pi gt phlo and plist.pi le phhi))
tm=slist.time
time=tm(sort(tm)) 
;convert to DETX and DETY 
dir='/home2/yaqoob/giscal/'
GTGISMAP, postyp, gistyp, dir, gmap,cal,detx,dety,deltax,deltay,coeff
GISLINXY,coeff,cal,detx,dety,deltax,deltay,slist.x,slist.y,xl,yl,xlp,ylp
slist.x=xl & slist.y=yl
if gistyp eq 2 then begin
; xopt=1.3 & yopt=0.8
  xopt=1.0 & yopt=0.49
endif
if gistyp eq 3 then begin
; xopt=-2.3 & yopt=1.0
  xopt=-2.41 & yopt=1.36
endif
MKCURVE,time,gti,binsize,nbins,tcen,tbounds,teff,ctspsec,ctserr
;find average source postion from (0,0) for every time bin
rpos=fltarr(nbins)
for k=0l,nbins-1 do begin
t1=tbounds(k,0) & t2=tbounds(k,1)
meancntr,slist,t1,t2,xx,yy
;convert to mm
xmm=(xx-129.)*0.25 & ymm=(yy-129.)*.25
x0=(xmm-xopt) & y0=(ymm-yopt)
rpos(k)=x0*x0+y0*y0 & if rpos(k) gt 0 then rpos(k)=sqrt(rpos(k))
endfor
window,1
ploterr,rpos,ctspsec,ctserr,psym=1
openw,1,qname+'.qdp'
yvals=ctspsec+ctserr
ymax=1.2*max(yvals)
printf,1,'READ SERR 1 2'
printf,1,'LA X DISTANCE FROM OPTICAL AXIS (MM) FOR SENSOR ',gistyp
printf,1,'LA Y cts/s in the PHA/PI RANGE ',phlo,phhi
printf,1,'LA T BINSIZE = ',binsize,' s'
printf,1,'R Y 0 ',ymax
printf,1,'MARKER 17 ON 2'
for k=0l,nbins-1 do begin
 printf,1,rpos(k),0.,ctspsec(k),ctserr(k)
endfor
close,1
return
end
