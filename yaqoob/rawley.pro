function rawley, r, theta, E
; calculates the PSPC off-axis point spread function assuming a given energy
; r is angular distance from "center" in arcmin
; theta is the off-axis angle (arcmin)
;
;set E = 1 keV (not bad for WR stars)
;
;E = 1.
;
;calculate p(r,E,theta) in 3 terms
;
;p1 is the Lorentzian mirror scattering term
;
rbreak = 861.9 / (60. * E)
Fscatt = 0.041*E^1.43
rscatt = 79.9 / (60. * E)
alpha = 2.119 + 0.212*E
Ascatt = Fscatt / (!pi * (rscatt/2.)^2 * $
  (alog(1 + (2.*rbreak/rscatt)^2) + $
   2.*rbreak^2/(alpha-2)/((rscatt/2.)^2+rbreak^2)) * (1.+(4.*rbreak/rscatt)^2))

if (r lt rbreak)then begin 
  p1 = Ascatt / (1 + (2.*r/rscatt)^2)
endif else begin
  p1rb = Ascatt / (1 + (2.*rbreak/rscatt)^2)
  p1 = p1rb * (r / rbreak)^(-alpha)
endelse
;
; p2 is the exponential function
;
Fexps = fltarr(2)
Fexps(0) = 10^(-1.635 + 0.639 * E + 0.052 * E^2) * exp(-0.5*(theta/12.)^2)
;NOTE: The following line may change in Jane's next release
Fexps(1) = 1.0 - 0.059 * E^1.43
Fexp = min(Fexps)
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
p3 = Aint * exp(-0.5 * (r / rint)^2)
;
;sum them up
;
psf = p1 + p2 + p3
return, psf
end
