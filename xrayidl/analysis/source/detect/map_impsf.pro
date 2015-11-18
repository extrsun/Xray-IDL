pro map_impsf,impsf,aimpsf,bv=bv,tbw=tbw
;+
; create merge psf and exposure maps from instrument maps
;
;*INPUTS:
; aimpsf - stacked psf images
;*OUTPUTS:
; impsf - weighted broad band psf image
;*OPTIONAL Inputs:
; bv - selecting vector containing band numbers (def =[1,2,3,4])
; tbw - vector containing the count flux weights in the bands
;	which depends on the spectral model used for creating the instrument
;	maps. (def =!tbw defined in sou_det_params)
;	if tbw=0, output no tb.
;
; written by wqd, 4/5/2006
;
;-
if n_params() eq 0 then begin
print,'Calling Seq. - map_impsf,aimpsf,impsf,bv=bv,tbw=tbw'
return
endif
if n_elements(bv) eq 0 then bv=[1,2,3,4]
nbt=n_elements(bv)
if n_elements(tbw) eq 0 then tbw=!tbw else $
	if tbw(0) eq 0 then return ;return normalized maps
impsf=aimpsf(*,*,bv-1)
if nbt gt 1 then begin
    tbnorm=tbw(bv-1)/total(tbw(bv-1))
    for k=0,nbt-1 do impsf(*,*,k)=impsf(*,*,k)*tbnorm(k)
    impsf=total(impsf,3)
endif
if !debug eq 3 then stop
return
end
