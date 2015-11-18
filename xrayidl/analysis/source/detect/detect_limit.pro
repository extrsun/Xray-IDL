pro detect_limit,time,dis,fb,fs,ston=ston,sfrac=sfrac,stob=stob,rs=rs
if n_elements(ston) eq 0 then ston=4
;-
; fb - background flux in units of cts/s arcmin^2
;+
; get aperture sizes
if n_elements(rs) eq 0 then begin
if !instr eq 'p' then begin
	if n_elements(sfrac) eq 0 then sfrac=0.85
	detect_params,dis,rs,blow=blow,bhigh=bhigh,perclimit=sfrac $
		,gsigma=sigma,gfrac=gfrac
;	if n_elements(gaus) ne 0 then sigma=gsigma
	sigma=sigma/!size_pixel ;in units of pixels
endif else begin
	if !instr eq 'h' then begin
	 if n_elements(sfrac) eq 0 then begin
		sfrac=0.5
		stop,'assuming sfrac = ',sfrac, ' Is this is OK?'
	 endif
		theta=findgen(40)/2.
		psf_hri_frac,theta,offs,frac=sfrac
		linterp,theta,offs,dis,rs
	endif else stop,'which instrument is this?'
endelse
endif
aa=ston^2/time
bb=fb*!pi*(rs/60.)^2
if keyword_set(stob) eq 0 then fs=0.5*(aa+sqrt(aa^2+4.*aa*bb))/sfrac $
else begin
;	fs=sqrt(aa*bb)/sfrac
	poisson_inv,bb*time,ston,fs
	fs=(fs-bb*time)/time/sfrac
endelse 
return
end