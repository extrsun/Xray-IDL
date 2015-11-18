function convlv, image, psf, PSFFT=psfft, buffer=buffer
;+
; NAME:
;	CONVLV
; PURPOSE:
;	Convolution of image with a centered Point Spread Function (PSF)
;
; CALLING SEQUENCE:
;	imconv = convolve( image1, psf, PSFFT = psfft )
;
; INPUTS:
;	image = the image to be convolved with psf
;	psf = the centered Point Spread Function, 
;	(size < or = to size of image).
;
; OPTIONAL INPUT KEYWORDS:
;	PSFFT = passes the Fourier transform of the PSF,
;		so that it can be re-used for the next time function is called.
;	buffer = if set, a region of size(psf)/2 will be set up as a buffer
;		zone to minimize the wrapped-around effect of the response.
;
; METHOD:  
;	When using FFT, PSF is expanded to size of image, as discussed in
;	Numerical Recipes, Press et al. (p411).
;
; HISTORY:	
;	written by wqd, 7/14/1996 
;-
	sp = size( psf) 
		;as a double check of the psf before padding with zeros
		;even if psfft is provided
	if (sp(0) NE 2) then begin
		message,"must supply PSF matrix (2nd arg.)",/INFO
		return, image
	endif else sp2 = sp/2 
	
	sim = size( image )  
	if keyword_set(buffer) ne 0 then begin
		;set up a buffer zone for the wrapped-around region
		nsim=sim+sp2
		nimage=fltarr(nsim(1),nsim(2))
		nimage(0,0)=image
	endif else begin
		nsim=sim
		nimage=image
	endelse

	spft=size(psfft) ;check to see whether psfft is provided
	if (spft(0) NE 2) OR (spft(spft(0)+1) NE 6) OR $ ;if not
	   (spft(1) NE nsim(1)) OR (spft(2) NE nsim(2)) then begin
		psfft=fltarr(nsim(1),nsim(2))
		psfft(0,0)=psf
		psfft=shift(psfft,-sp2(1),-sp2(2)) ;shift the center to (0,0)
		psfft = FFT( psfft, -1 ) * ( nsim(1)*nsim(2) )
	endif

	conv = float( FFT( FFT( nimage, -1 ) * psfft, 1 ) )
	if keyword_set(buffer) ne 0 then $
		conv=conv(0:sim(1)-1,0:sim(2)-1)
return, conv
end
