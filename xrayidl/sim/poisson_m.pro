;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; poisson
;
;*PURPOSE:
; Generate a realization of the poisson randum numbers for an array 
;  of any dimension 
;
;*CALLING SEQUENCE:
; poisson,expect,ran,seed
;
;*PARAMETERS:
;*INPUTS:
; expect - an array containing expected values (counts)
; seed - the seed value for random number generations 
;
;*OUTPUTS:
; ran - an array of the same size as expect, containing the generated randum 
;      values (counts) 
;
;*PROCEDURE:
; First, randum values between 0 and 1 are generated with RANDUMU as the 
; realization of the integrated probabilities. Then these values are
; compared with the integrated probabilities calculated with
; the poisson process and the expected values to determine ran
;
;*RESTRICTIONS:
; none
;
;*NOTES:
; none
;
;*SUBROUTINES CALLED:
; remove
;
;*MODIFICATION HISTORY:
; writen Sept 24 1992 (WQD)
;
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro poisson_m,expect,ran,seed,nsim=nsim
;
if N_params() eq 0 then begin
print,'CALLING SEQUENCE -  poisson,expect,ran,seed,nsim=nsim'
return
endif
;
if n_elements(nsim) eq 0 then nsim=1
sz=size(expect)
nbin=sz(sz(0)+2)
ran=intarr(nbin,nsim)
for k=0,nsim-1 do begin
	ran(*,k)=poidev(expect,seed=seed)
endfor
if sz(0) eq 2 then ran=reform(ran,sz(1),sz(2),nsim)
;
end
