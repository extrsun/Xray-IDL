pro get_snr,cs,nbin_s,cb,nbin_b,csn,snr,snb=snb,sfraco=sfraco,bfrac=bfrac $
,bmap=bmap,ebmap=ebmap,fac_ssr=fac_ssr,ecsn=ecsn
;-
; calculate net source counts and SNR of a source, following Primini
; in ADASS meeting II, p. 428
;
; writen by wqd, April 15, 1995
;+
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - get_snr,cs,nbin_s,cb,nbin_b,csn,snr,snb=snb'
print,',sfraco=sfraco,bfrac=bfrac,bmap=bmap,fac_ssr=fac_ssr'
return
endif
if n_elements(sfraco) eq 0. then sfrac=0.9 else sfrac=sfraco
if n_elements(fac_ssr) ne 0 then begin
    if fac_ssr ne 1 then sfrac=1. ;no correction for the scattered fraction.
endif
if n_elements(bmap) ne 0 then begin
	csn=(cs-cb)
        if n_elements(ebmap) eq 0 then ecsn=sqrt(cs > 1.e-4) $ 
;ecsn=sqrt(cs > cb) $ ;this can cause problem in summing the errors in
;a broad band
         else ecsn=sqrt(cs + (ebmap > 1.e-10)^2)
        snr=csn/(ecsn > 1) 
	csn=csn/sfrac
        ecsn=ecsn/sfrac
endif else begin
	if n_elements(bfrac) eq 0. then bfrac=(1-sfrac)*0.5 
	; 50% in the annulus is an approximation
;	if n_elements(bfrac) eq 0. then bfrac=0. ;for map detection
		
	bnorm=nbin_b/float(nbin_s)
	csn=cs*bnorm-cb ; subtract the background
	snr=csn/sqrt(cs*bnorm^2+cb)
	csn=csn/(sfrac*bnorm-bfrac)
        ecsn=csn/snr
endelse
if !debug eq 1 then stop
return
end
