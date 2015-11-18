pro vatest,smina,sfv,gsp,vastat,bsp=bsp,smax=smax
;+
; perform v_e/va test, a version extended of the Avni and Bahcall (1980) 
; statistic for varying detection sensitivity across a field 
; smina - vector or array containing minimum flux at each pixel of the field
; sfv	- vector containing the source fluxes
; gsp - vector contains the parameters of the galaxy source flux distribution
;	used by int_bs procedure 
; bsp - vector contains the parameters of the background source 
;	flux distribution used by int_bs procedure 
; vastat - output vector containing the v_e/v_a statistics and its error.
; written by wqd, Nov 2, 1998
;-
if n_params() lt 1 then begin
	print,'CALLING SEQUENCE - vatest,smina,sfv,gsp,vastat,bsp=bsp,smax=smax'
	return
endif
; first the total volume of the survey or image.
int_bs,smina,nbsa,bsp=bsp
int_bs,smina,ngsa,bsp=gsp,smax=smax
nsa=ngsa+nbsa
va=total(nsa)

; then the enclosed volume
nbin=n_elements(smina)
ns=n_elements(sfv)
ve=fltarr(ns)
for k=0,ns-1 do begin
	sel=where(smina gt sfv(k),nsel)
	if nsel ne 0 then ve(k)=total(nsa(sel))
	if nsel ne nbin then begin
		int_bs,sfv(k),nbsc,bsp=bsp
		int_bs,sfv(k),ngsc,bsp=gsp,smax=smax
		ve(k)=ve(k)+(nbin-nsel)*(ngsc+nbsc)
	endif else stop,'source flux is below smina (ie. nsel eq nbin)'
endfor
vastat=[total(ve)/(va*ns),1./sqrt(12.*ns)]
print,'vastat, err = ',vastat 
return
end