pro get_psyserr,oaam,psyserr,nooapsyserr=nooapsyserr
;+
; get the systematic error (arcsec) in the absolute astrometry of an obs.
; to be called by ml_anal.pro
;
; oaam - offaxis angle in arcmin
; psyserr - output systematic error in arcsec
;           nooapsyserr - if set, no offaxis dependence will be
;                         included for the calculation of the sys
;                         error.
;
; writen by WQD, June 22, 2003
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  get_psyserr,oaam,psyserr,nooapsyserr=nooapsyserr'
return
endif

psyserr=0.2
if not keyword_set(nooapsyserr) then $
	psyserr=psyserr+1.4*(oaam/8.)^2 ; a rough fit to the
	; the Orion x-o offsets as a function of axis angles (Feigelson) 
	; plate scale uncertainty may also be considered to be included.

return
end
