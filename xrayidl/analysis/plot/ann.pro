pro ann,image_c,image_b,image_t,r1,r2,angle,flux,fluxerr,a1=a1,a2=a2,ad=ad $
,cpy=cpy,cpx=cpx,aexlow=aexlow,aexhigh=aexhigh
;-
; calculate the angular flux distribution around a user specified center
; and fit the distribution with a constant flux.
; The program can also be used to calculate the significance and flux
; of an enhancement in the distribution.
; 
; the angle is defined to be anti-clockwise with angle=0 for the norminal 
;	x-axis
;
;*INPUTS:
; image_c,image_b,image_t - count, background, and exposure images
; r1,r2 - the inner and outer radii of the annulus within which data are used
; a1,a2 - the lower and upper boundaries of the angle intervals
; ad - the number of angles to be calculated
; cpx, cpy - the pixel center 
; aexlow,aexhigh - the lower and upper angular boundaries within which
;		the data will not be excluded in the calculation of the 
;		distribution and a deviation of the data from the distribution
;		will be calculated.
;*OUTPUTS:
; angle - vector containing the angles
; flux - the surface brightness defined as (image_c-image_b)/image_t
; fluxe - the standard deviation of the flux
;
; writen by WQD Jan 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - ann,image_c,image_b,image_t,r1,r2,angle'
print,',flux,fluxerr,a1=a1,a2=a2,ad=ad,cpy=cpy,cpx=cpx,'
print,'aexlow=aexlow,aexhigh=aexhigh'
return
endif

; calculate the surface brightness distribution as a function of the angle

sz=size(image_t)
dimx=sz(1)
dimy=sz(2)
if n_elements(a1) eq 0 then a1=0
if n_elements(a2) eq 0 then a2=360.
if n_elements(ad) eq 0 then ad=40
if n_elements(aexlow) eq 0 then aexlow=0.
if n_elements(aexhigh) eq 0 then aexhigh=0.
get_angle,dis,a,dimx,dimy,cpx=cpx,cpy=cpy
sel=where(image_t gt 0. and dis ge r1 and dis le r2 and a ge a1 and a le a2, nsel)
count=image_c(sel)
back=image_b(sel)
time=image_t(sel)
a=a(sel)
flux=fltarr(ad)
bcount=fltarr(ad)
mexp=fltarr(ad)
nb=mexp  
adel=(a2-a1)/ad
angle=findgen(ad)*adel+(a1+0.5*adel)

for k=0,ad-1 do begin
	alow=a1+k*adel
	ahigh=alow+adel
	sel=where(a ge alow and a lt ahigh,nsel)
	if nsel ne 0 then begin
	  flux(k)=total(count(sel))
	  bcount(k)=total(back(sel))
	  mexp(k)=total(time(sel))
	  nb(k)=n_elements(time(sel))
	endif
endfor
if !debug eq 1 then stop
bcount=bcount > 1.
fluxerr=sqrt(flux > bcount)/mexp
flux=(flux-bcount)/mexp
exsel=where (angle gt aexlow and angle lt aexhigh, nexsel)
if !debug eq 1 then stop
if nexsel ne 0 then begin
	sflux=total(flux(exsel))
	sfluxerr=total(fluxerr(exsel)*fluxerr(exsel))
	sbin=total(nb(exsel))
	remove,exsel,angle,flux,fluxerr,nb
endif
if !debug eq 1 then stop

mflux=total(flux/(fluxerr*fluxerr))
mfluxerr=total(1./(fluxerr*fluxerr))
mflux=mflux/mfluxerr
mfluxerr=sqrt(1./mfluxerr)
print,'mflux,mfluxerr = ',mflux,mfluxerr
chi=total((flux-mflux)*(flux-mflux)/(fluxerr*fluxerr))
errflux=total((flux-mflux)*(flux-mflux))
nb=n_elements(flux)
print,'chi,nb = ',chi,nb
errflux=sqrt(errflux/(nb-1))
print,'errflux = ',errflux
;
; estimate the deviation
if nexsel ne 0 then begin
sflux=(sflux-mflux*nexsel)*sbin
sfluxerr=sqrt(sfluxerr+errflux*errflux*nexsel)*sbin
print,'sflux, sfluxerr,s/n,nexsel,sbin = ', $
	sflux,sfluxerr,sflux/sfluxerr,nexsel,sbin
endif
end