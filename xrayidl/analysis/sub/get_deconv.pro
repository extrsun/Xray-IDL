pro get_deconv, image_c,image_t,image_b, psf, deconv, FT_PSF=psf_ft, $
NO_FT=noft,GAUSSIAN=gaussian, CONSTRAINT=constraint,conv=conv
;+
; NAME:
;	MAX_LIKELIHOOD
; PURPOSE:
;	Deconvolution of an observed image (or spectrum), given the
;	instrument point spread response function (spatially invariant psf).
;	Performs iteration based on the Maximum Likelihood solution for
;	the restoration of a blurred image (or spectrum) with additive noise.
;	Maximum Likelihood formulation can assume Poisson noise statistics
;	or Gaussian additive noise, yielding two types of iteration.
;
; CALLING SEQUENCE:
;	for i=1,Niter do begin
;		Max_Likelihood, image_c, psf, deconv, FT_PSF=psf_ft
;
; INPUTS:
;	image_c = observed image or spectrum, should be mostly positive,
;				with mean sky (background) near zero.
;	psf = Point Spread Function of the instrument,
;				(response to a point source, must sum to unity).
;	deconv = result of previous call to Max_Likelihood,
;		(on first call, initial guess default = average of image_c).
;
; OUTPUTS:
;	deconv = deconvolution result of one more iteration by Max_Likelihood.
;
; KEYWORDS:
;      /GAUSSIAN causes max-likelihood iteration for Gaussian additive noise
;		to be used,  otherwise the default is Poisson statistics.
;	CONSTRAINT = used only when /GAUSS specified, and then the image is
;			constrained to be > CONSTRAINT, otherwise image > 0.
;	FT_PSF = passes (out/in) the Fourier transform of the PSF,
;		so that it can be reused for the next time procedure is called,
;      /NO_FT overrides the use of FFT, using the IDL function convol() instead.
;
; EXTERNAL CALLS:
;	function convolve( image, psf ) for convolutions using FFT or otherwise.
;
; METHOD:
;	Maximum Likelihood solution is a fixed point of an iterative eq.
;	(derived by setting partial derivatives of Likelihood to zero).
;	Poisson noise case was derived by Richardson(1972) & Lucy(1974).
;	Gaussian noise case is similar with subtraction instead of division.
;
; HISTORY:
;	written 1992 Frank Varosi STX @ NASA/GSFC
;-
if N_elements( constraint ) eq 0 then constraint = 0.
if n_elements(image_b) eq 0 then image_b=0.
image_teff=imdiv(1.,convolve( image_t, psf,FT=psf_ft,/ROT,NO=noft ))

	if N_elements( deconv ) NE N_elements( image_c ) then begin
		deconv = image_c
		deconv(*) = total( image_c-image_b )/N_elements( image_c )
	endif
	conv = image_t*(convolve( deconv, psf, FT=psf_ft, NO=noft )> constraint)
	  

	if keyword_set( gaussian ) then begin
		deconv = ( deconv + convolve( image_c - conv, psf, $
		/ROTATE_PSF,   FT=psf_ft, NO=noft ) ) > constraint
	endif else begin
		image=imdiv(image_t*image_c,conv+image_b) 
		deconv = deconv*image_teff $
		*convolve( image, psf,FT=psf_ft,/ROT,NO=noft )
	endelse
if !debug eq 2 then stop
;deconv=deconv > constraint
return
end
