pro syn_lum,edot,rs,alpha,lx,emin=emin,emax=emax,eb=eb,gamma=gamma,vp=vp,ns=ns
;-
; calculate the luminosity of a pulsar wind nebula, using the one-zone
; model of R. Chevalier (ApJ, 2000).
;
;* Limits: The index of particle distribution has to be > 2
;	i.e., alpha > 1 for the eq. (9) case (non bow shock)
; 	      alpha > 0.5 for the eq. (12) case (bow shock)
; Near these limits, the results are also not very good, because the
; upper bound of the particular distribution needs to be considered.
;
; edot - the total energy input from the pulsar in units of 10^38 ergs/s
; rs - the pulsar wind shock radius in units of 10^17 cm
; alpha - energy index of X-ray spectrum
; lx - output luminosity in units of 10^38 ergs/s
; emin, emax - the lower and upper energy limits (keV) of the luminosity
; eb - the magentic energy fraction, the particle energy fraction is
;	assumed to be 1-eb
; gamma - Lorentz factor of the wind in units of 10^6
; if ns is specified, the modified eq (12), replacing t=rs/vp by t=ns/vp
; ns - the size of the nebula in units of 10^17 cm
; vp - the proper motion velocity in units of the speed of light, def=1/3.
;
; IDL kind of does not like the use of vectors here.
;*Example:
; syn_lum,4.8,3.,pp,lx2,ns=7. ;for N157B
;
; writen by wqd, Oct. 20, 2000
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- syn_lum,edot,rs,alpha,lx,emin=emin,emax=emax,eb=eb,gamma=gamma,vp=vp,ns=ns'
print,'* Limits: The index of particle distribution has to be > 2'
print,'i.e., alpha > 1 for the eq. (9) case (non bow shock)'
print,' alpha > 0.5 for the eq. (12) case (bow shock)'
return
endif

if n_elements(gamma) eq 0 then gamma=3.
if n_elements(emina) eq 0 then emin=0.2
if n_elements(emax) eq 0 then emax=4.
if n_elements(eb) eq 0 then eb=0.5
if n_elements(ns) eq 0 then begin
	pp=2*alpha
	fac=2.68e-4
	aa=0.5*((pp-2.)/(pp-1.))^(pp-1)*(eb*fac*gamma^4 $
		/rs^2*edot)^((pp-2.)*0.25)*(1.-eb)^(pp-1)
	;if pp eq 2 then lx=aa*edot*alog(emax/emin) else $
	lx=aa*edot*2./(2.-pp)*(emax^((2.-pp)*0.5)-emin^((2.-pp)*0.5))
endif else begin
	pp=2*alpha+1
	if n_elements(vp) eq 0 then vp=1./3.
	fac=2.68e-4
	aa=16.5*(pp-2.)^(pp-1)/(pp-1.)^(pp-2)*(1.-eb)^(pp-1)*ns/gamma^3 $
		*(eb*fac*gamma^4/rs^2*edot)^((pp+1)*0.25)
	if pp eq 3 then lx=aa*edot*alog(emax/emin) else $
	lx=aa*edot*2./(3.-pp)*(emax^((3.-pp)*0.5)-emin^((3.-pp)*0.5))
endelse
print,'Lx = ',lx
return
end
