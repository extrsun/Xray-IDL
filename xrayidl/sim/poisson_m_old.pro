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
sz=n_elements(expect)
ran=intarr(sz,nsim)
;ran=fix(expect)*0
nexpect=fltarr(sz,nsim)
index_conv,lindgen(sz*nsim),[sz,nsim],index
nexpect(*)=expect(index(0,*))
loc=lindgen(n_elements(nexpect)) 	;containing locations of the bins
;
;only elements with expected values greater than 0 need the calculations

ran=intarr(sz,nsim)
sel=where(nexpect ge 80,nsel)
if nsel ne 0 then begin
	print,'for counts >= 80, use randomn(seed,D1,...)'
	mn=nexpect(sel)
	ran(sel)=(randomn(seed,nsel)*sqrt(mn)+mn) > 0
endif 
sel=where(nexpect gt 0. and nexpect lt 50,nbin)
if nbin eq 0 and nsel eq 0 then begin
	print,'All expected values equal to 0'
	return
endif else if nbin eq 0 then goto,finish
loc=loc(sel)
expect_buf=double(nexpect(loc))
sigran=randomu(seed,nbin)
;
tnsel=0
prob=exp(-expect_buf)
sig=prob

for n=1,10000 do begin	
	sel=where(sig-sigran ge 0.,nsel)
	if nsel ne 0 then begin
		tnsel=tnsel+nsel
		ran(loc(sel))=n-1 ;ran values determined for these elements
		if tnsel eq nbin then goto,finish else $
		  ;remove those elements with subscripts contained in sel
		  remove,sel,expect_buf,prob,sig,sigran,loc
	endif
	prob=prob*expect_buf/double(n) ;these values could be very small
	sig=sig+prob                ;the double precission is needed
endfor 
;
finish:
sz=size(expect)
if sz(0) eq 2 then ran=reform(ran,sz(1),sz(2),nsim)
;
end
