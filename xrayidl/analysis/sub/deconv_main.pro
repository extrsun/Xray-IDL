pro deconv_main,image_c,image_t,psf,deconvo,image_bo=image_bo,filter=filter $
,nit=nit,errlimit=errlimit,conv=conv,image_in=image_in
;-
; Deconvolution of an observed image, give the instrument point spread
; function (spatially invariant psf). A background level is used to
; suppress the enhancement of noise in the general sky background.
;
;*INPUTS:
; image_c, image_t - count and exposure images
; psf - point spread function of the instrument
; image_bo - desired background image (def = 0.)
; filter - image used to select regions for estimating the general 
;	sky background. Only regions with positive values are selected.
;	def = image_t
; nit - maximum number of iteration (def =500)
; errlimit - the limit of the relative error of total counts between the
; 	count image and the deconvled image.
;*OUTPUTS:
; deconvo - the deconvolved flux image (in units of cts/s pixel) with
;		the input background (i.e. image_bo) subtracted.
; conv - convolved flux image 
;*NOTES:
; Maximum Likehood formulation assume Poisson noise statistics (Richardson &
; Lucy method).	Here, an image vignetting correction and a background
; noise suppression is added. A description of a similar algorithm can be 
; found in the article by Rick White in ST RESTORATION, P11 (1993 summer).
; Written by WQD, Sept 4, 1993.
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - deconv_main,image_c,image_t,psf,deconvo'
print,',image_bo=image_bo,filter=filter,nit=nit,errlimit=errlimit'
print,',conv=conv,image_in=image_in'
return
endif
if n_elements(image_bo) eq 0 then image_bo=0.
if n_elements(filter) eq 0 then filter=image_t
if n_elements(nit) eq 0 then nit=500
if n_elements(errlimit) eq 0 then errlimit=5.e-2
if n_elements(image_c) ne n_elements(image_t) then stop,'stop: the numbers of elements in the image_c and image_t do not match'
; estimate the general background level in the field
sel=where(filter gt 0.,nsel)
if nsel eq 0 then stop,'stop: no pixel with non zero exposure'
flux=imdiv(image_c-image_bo,image_t)
mflux=avg(flux(sel)) 
image_b=image_bo+image_t*(mflux*0.9) ;arbitray value
print,'background flux= ',mflux
frac=0.5 ; a factor used to control the fraction of a new deconvlved to be
	; be used in each iteration
fac=3. ;a factor used in reducing the assumed background during the iteration
errdlimit=0.01

; calculate some parameters to be used in the iterations
tcounts=total(image_c)
tcntr=tcounts/total(image_t)
image_teff=imdiv(1.,convolve( image_t, psf,FT=psf_ft,/ROT,NO=noft ))
image_mult=image_t*image_c

if n_elements(image_in) ne 0 then begin
	deconv=image_in
endif else begin
;start with an assumed high internal background (that is not subject to the 
; PSF and the vignetting) in order to suppress the enhancement of the noise
; in the general background regions.
deconv=image_t*0.+mflux*0.1
endelse
;
; start the interation
deconvo=deconv
for k=1,nit do begin
;	get_deconv, image_c,image_t,image_b, psf, deconv $ ; FT_PSF=psf_ft 
;	,const=const,conv=conv
;
	conv = image_t*convolve(deconv, psf, FT=psf_ft, NO=noft )
	image=imdiv(image_mult,conv+image_b) 
	deconv = deconv*image_teff*convolve(image, psf,FT=psf_ft,/ROT,NO=noft)
	; Richardson &  Lucy algorithm

	deconv=(1-frac)*deconvo+frac*deconv
	; calculate the relative errors
	err=total(conv+image_b)/tcounts-1. ;relative difference between the
	; the total numbers of counts 
	errd=avg(imdiv(abs(deconvo-deconv),deconvo)) ;relative difference
	; in two consective runs
	print,'interation, count and flux difference (err, errd) = ',k,err,errd
	if !debug eq 1 then stop,'stop after a new run. type .c to continue. ortype nit=0 to exit' $
		else if !debug eq 0 and err gt 0. then begin
		;when the total number of counts in the model image exceeds
		;the real number of counts, something needs to be done.
		print,'Now the number of counts in the convolved image exceeds'
		print,'that in the original image. You may now type !debug=1
		print,'to monitor each run. The background value will 
		print,'be reduced to make the excess negative. The default'
		print,'factor of the reduction is fac= ',fac
		print,' You may give an alternative value here.'
		print,'You may also show the deconvled image (deconv).'
		read,' If you want to exit, type: nit=0.'
		stop,' To continue, type: .c'
		endif
		if nit eq 0 then goto,done
	if err gt 0. then image_b=(image_b-(fac*err*tcntr)*image_t) > 0.
	; hopefully this fac value will eventually reduce the err value to
	; become negative.
	deconvo=deconv
	if abs(err) lt errlimit and errd lt errdlimit then goto,done
endfor
done:
print,' Converged with the number of iteration = ',k
backdif=avg(imdiv(image_b-image_bo,image_t))
print,'The difference between the current and input background fluxes '
print,backdif,'is added from the output deconv.'
deconvo=deconv+backdif ;put the residual of the assumed background back 
conv = convolve(deconvo, psf, FT=psf_ft, NO=noft )
stop,'stop at the end of the program'
end