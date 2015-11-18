pro psf_hri_frac,theta,rr,frac=frac,s2v=s2v
;+
; calculate the flux encircled radius of the RHRI PSF for off-axis sources
; following the formula given at 
; http://heasarc.gsfc.nasa.gov/docs/rosat/newsletters/hripsf10.html
;
; theta - vector containing the off-axis angle in units of arcmin
; rr - output radius in units of arcsec
; frac= input fraction of the flux encircled
; s2v - the gaussian size of the second component, which varies with offaxis 
;	angle
;
; written by wqd, 11/17/98
;-
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - psf_hri_frac,theta,rr,frac=frac'
return
endif
if n_elements(frac) eq 0 then frac=0.5
nfrac=n_elements(frac)
s1 = 2.18 & A1 = 0.96 & s2 = 4.04  & A2 = 0.18 & s3 = 31.7 & A3 = 0.00090
xx=findgen(100)+2.
sxx=xx^2
nbin=n_elements(theta)
rr=fltarr(nbin,nfrac)
s2v=fltarr(nbin)
for k=0,nbin-1 do begin
	s2 = 3.3 + 0.019*theta(k) -0.016*theta(k)^2 + 0.0044*theta(k)^3
	comp1=2*!pi*a1*s1^2*(1.-exp(-sxx/(2.*s1^2)))
	comp2=2*!pi*a2*s2^2*(1.-exp(-sxx/(2.*s2^2)))
	comp3=2*!pi*a3*s3^2*(1.-exp(-xx/s3))
	dnorm=2*!pi*(a1*s1^2+a2*s2^2+a3*s3^2)
	tt=(comp1+comp2+comp3)/dnorm
	linterp,tt,xx,frac,rrs
	rr(k,*)=rrs
	s2v(k)=s2
endfor
return
end