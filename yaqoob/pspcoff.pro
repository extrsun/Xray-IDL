function pspcoff, r, theta, E, spec
; calculates the PSPC off-axis point spread function assuming a given energy
; r is angular distance from "center" in arcmin
; theta is the off-axis angle (arcmin)
; E is an array of energies in keV and
; SPEC are the correspnding photon counts 
; The final PSF is weighted by this pulse height distribution
; If you just want the PSF at one energy you can set spec=fltarr(1)+1.
; and E = your value. 
; coded from CALDB MEMO CAL/ROS/93-015  Turner etal.
; G. Rawley and T. Yaqoob
;
;calculate p(r,E,theta) in 3 terms
;
;p1 is the Lorentzian mirror scattering term
;
r=r(0)
nen=n_elements(e)
p1=fltarr(nen)
if nen eq 1 then begin
	E=E(0) & spec=spec(0)
endif
rbreak = 861.9 / (60. * E)
;Fscatt = 0.041*E^1.43
;following line seems to have changed from the original memo
Fscatt = 0.075*E^1.43
rscatt = 79.9 / (60. * E)
alpha = 2.119 + 0.212*E
Ascatt = Fscatt / (!pi * (rscatt/2.)^2 * $
  (alog(1 + (2.*rbreak/rscatt)^2) + $
   2.*rbreak^2/(alpha-2)/((rscatt/2.)^2+rbreak^2)) * (1.+(4.*rbreak/rscatt)^2))

wlt=where((r lt rbreak),nwlt)
wge=where((r ge rbreak),nwge)
if nwlt gt 0 then p1(wlt)= Ascatt(wlt) / (1 + (2.*r/rscatt(wlt))^2)
if nwge gt 0 then p1(wge)= (r / rbreak(wge))^(-alpha(wge))*Ascatt(wge) / $ 
 (1.+(2.*rbreak(wge)/rscatt(wge))^2) 
;if (r lt rbreak)then begin 
;  p1 = Ascatt / (1 + (2.*r/rscatt)^2)
;endif else begin
;  p1rb = Ascatt / (1 + (2.*rbreak/rscatt)^2)
;  p1 = p1rb * (r / rbreak)^(-alpha)
;endelse
;
; p2 is the exponential function
;
Fexps = fltarr(nen,2) & Fexp=fltarr(nen)
Fexps(0:nen-1,0) = 10^(-1.635 + 0.639 * E + 0.052 * E^2) * exp(-0.5*(theta/12.)^2)
;NOTE: The following line may change in Jane's next release
;Fexps(1) = 1.0 - 0.059 * E^1.43
;indeed it has
Fexps(0:nen-1,1) = 1.0-Fscatt
for j=0,nen-1 do Fexp(j) = min([Fexps(j:j,0),Fexps(j:j,1)])
rt = 1. / 60. * sqrt(50.61/ E^1.472 + 6.80 * E^5.62)
Aexp = Fexp / (2. * !pi * rt^2)
p2 = Aexp * exp(-r / rt)
;
; p3 is the Gaussian
;
Fint = 1 - Fscatt - Fexp
rint = 1. / 60. * sqrt(108.7 / E^0.888 + 1.121 * E^6.)
mint = 1./60. * sqrt(0.129 * theta^2.848)
gint = sqrt(rint^2 + mint^2)
Aint = Fint / (2. * !pi * gint^2)
p3 = Aint * exp(-0.5 * (r / gint)^2)
;
;sum them up
;
psf = p1 + p2 + p3
pspcoff=total(psf*spec)/total(spec)
;pspcoff=0.0
;if nen gt 1 then begin
;for j=0,nen-1 do pspcoff = pspcoff+psf(j)*float(spec(j))
;pspcoff=pspcoff/total(spec)
;endif
;if nen eq 1 then pspcoff=psf
return, pspcoff
end
