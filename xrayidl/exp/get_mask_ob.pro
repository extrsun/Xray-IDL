pro get_mask_ob,mask,mask_cor,h,filen=filen,dim=dim
;-
; obtain a normalized image with ribs removed in the detector's coordinate
; and casted according to the aspect solution of the instrument.
; the input file is created by make_expmap.pro (def file name is rp*_exp00.fits
; writen by WQD 5/16/93
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  get_mask_ob,mask,mask_cor,h'
return
endif
if n_elements(filen) eq 0 then filen=!data_dir+!seq_no+'_exp00.fits'
mask=readfits(filen,h)
mask=image_cut(mask,dim/2.,/pixel)
mask=mask/max(mask)
if n_params() gt 1 then begin
	sel=where(mask gt 0.,nsel)
	mask_cor=total(mask(sel))/nsel
	print,'mast correction = ',mask_cor
endif
end