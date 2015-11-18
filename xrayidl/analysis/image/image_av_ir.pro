pro image_av_ir,image_x,image_ir,header,image_t,factor=factor,levels=levels $
,fmin=fmin,fmax=fmax,radius=radius,ssub=ssub
; plot the smoothed X-ray image with an overlaid IRAS image.
; the header of the image_ir needs to be modified to get correct coordinates
; image_x -- input x-ray image which has a dimension smaller than that of
;            the image_ir. the default of the factor=0.25
; radius -- the plot image_x radius (in units of 15'')
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- image_av_ir,image_x,image_ir,header,image_t'
print,',factor=factor,levels=levels,fmin=fmin,fmax=fmax,ssub=ssub'
return
endif 
;
if n_elements(factor) eq 0 then factor=0.25
if n_elements(levels) eq 0 then levels=[2.2,2.5,3.,3.5,4.,5.,6.]
if n_elements(radius) eq 0 then radius=230
;
; shrink the size of the image_ir. This has an advantage for using 
; extrapolation in GREY.
image_irc=image_comp(image_ir,factor)
; change the bin size
cdelt=sxpar(header,'cdelt*')
cdelt=cdelt/factor
headerc=header
sxaddpar,headerc,'cdelt1',cdelt(0)
sxaddpar,headerc,'cdelt2',cdelt(1)
; change the dimension of the image
naxis=sxpar(header,'naxis*')
dimc=nint(radius*factor)*2
sxaddpar,headerc,'naxis1',dimc
sxaddpar,headerc,'naxis2',dimc
;

hsubx=(naxis(0)*factor-dimc)/2 
hsuby=(naxis(1)*factor-dimc)/2 
;
; change the pixel reference center
crpix=sxpar(header,'crpix*') 
;crpix may not at the real center of the fitx image
sxaddpar,headerc,'crpix1',crpix(0)*factor-hsubx
sxaddpar,headerc,'crpix2',crpix(1)*factor-hsuby
;
; get rectagular IRAS image with the size same as the image_x
image_irc=image_irc(hsubx:naxis(0)*factor-hsubx-1,hsuby:naxis(1)*factor-hsuby-1)
; or a circular IRAS image
;image_irc=image_cut(image_irc,(radius*factor),/pixel)
image_xc=image_cut(image_x,radius*factor,/pixel)
if n_elements(fmin) eq 0 then begin
	fmin=min(image_x) & fmax=max(image_x)
endif

; with image_t provided, we can estimate the fmin and fmax in the x-ray image
if n_elements(image_t) ne 0 then begin
	image_tc=image_comp(image_t,factor)
	image_tc=image_cut(image_tc,(radius*factor),/pixel)
	if n_elements(ssub) ne 0 then image_xc(where(image_tc le 0.))=0.
	fmin=min(image_xc(where(image_tc gt 0.)))
	fmax=max(image_xc(where(image_tc gt 0.)))
endif
print,'fmin,fmax = ',fmin,fmax
; finally let's plot
cont_grey,image_irc,headerc,bscale(image_xc,fmin*1.1,fmax*0.9),type=1 $
,levels=levels
end