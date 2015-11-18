pro get_stat,im,flux,eflux,filter=filter
;-
; calculate the statistics of an image (in selected region)
; im - input image (e.g., flux)
; flux, eflux - output mean flux and its one sigam error
; filter - an image used to select pixels in im (only pixel with
;	positive filter values will be used in the calculation
; writen by wqd, Oct 2, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_stat,im,flux,eflux,filter=filter'
return
endif
if n_elements(filter) ne 0 then imv=im(where(filter gt 0.)) $
 else imv=im
nbin=n_elements(imv) 
if nbin eq 0 then stop,'No bin is involved'
flux=avg(imv)
dif=imv-flux
if abs(flux) le 1.e-15 then begin
	dif=dif*1.e20
	eflux=sqrt(avg(dif*dif))
	eflux=eflux*1.e-20
endif else eflux=sqrt(avg(dif*dif))
print,'dispersion and number of bins included  = ',eflux, nbin
print,'mean flux and its one sigma error = ',flux,eflux/sqrt(nbin-1)
end