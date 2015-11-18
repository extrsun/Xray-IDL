function convolve, image, psf, FT_PSF=psf_FT, CORRELATE=correlate,  $
				ROTATE_PSF=rotpsf, AUTO_CORREL=auto, NO_FT=noft
;+
; NAME:
;	CONVOLVE
; PURPOSE:
;	Convolution of image with Point Spread Function (PSF),
;	default is to compute using product of Fourier transforms.
;
; CALLING SEQUENCE:
;	imconv = convolve( image1, psf, FT_PSF = psf_FT )
;			or
;  	correl = convolve( image1, image2, /CORREL )
;			or
;	correl = convolve( image, /AUTO )
;
; INPUTS:
;	image = the image to be convolved with psf
;	psf = the Point Spread Function, (size < or = to size of image).
;
; OPTIONAL INPUT KEYWORDS:
;	FT_PSF = passes the Fourier transform of the PSF,
;		so that it can be re-used for the next time function is called.
;	/ROTATE : convolution with rotated PSF, same as /CORREL if using FFT.
;	/CORRELATE uses the conjugate of the Fourier transform of PSF,
;		to compute the cross-correlation of image and PSF.
;	/AUTO_CORR computes the auto-correlation function of image.
;	/NO_FT overrides the use of FFT, using the IDL function convol() 
;		instead.  (then PSF is rotated by 180 degrees to give 
;		equivalent result)
;
; METHOD:  
;	When using FFT, PSF is centered & expanded to size of image.
;
; HISTORY:	
;	written 1992 Frank Varosi STX @ NASA/GSFC
;-
	sp = size( psf_FT )
	sim = size( image )  &  sc = sim/2
;	sim = size( image )  &  sc = sim/2

	if (sim(0) NE 2) OR keyword_set( noft ) then begin
		if keyword_set( auto ) OR keyword_set( correlate ) then begin
			message,"correlation only for images, using FFT",/INFO
			return, image
		  endif else if keyword_set( rotpsf ) then $
				return, convol( image, psf ) $
			else	return, convol( image, rotate( psf, 2 ) )
	   endif

	if keyword_set( auto ) then begin
		imFT = FFT( image, -1 ) * sqrt( sim(1)*sim(2) )
		return, shift( float( FFT( imFT*conj( imFT ),1 ) ),sc(1),sc(2) )
	   endif

	if (sp(0) NE 2) OR (sp(sp(0)+1) NE 6) OR $
	   (sp(1) NE sim(1)) OR (sp(2) NE sim(2)) then begin
		sp = size( psf )
		if (sp(0) NE 2) then begin
			message,"must supply PSF matrix (2nd arg.)",/INFO
			return, image
		   endif
		sp2 = sp/2			;center PSF in new array,
;		sp2 = sp/2			;center PSF in new array,
		Loc = ( sc - sp2 ) > 0	   ;handle all cases: smaller or bigger
		s = (sp2 - sc) > 0
		L = (s + sim-1) < (sp-1)
		psf_FT = fltarr( sim(1), sim(2) )
		psf_FT( Loc(1), Loc(2) ) = psf(s(1):L(1),s(2):L(2))
		psf_FT = FFT( psf_FT, -1 ) * ( sim(1)*sim(2) )
;		psf_FT = FFT( shift(psf_FT,-sc(1)+1,-sc(2)+1), -1 ) * ( sim(1)*sim(2) )
	   endif

	if keyword_set( correlate ) OR keyword_set( rotpsf ) then $
		conv = float( FFT( FFT( image, -1 ) * conj( psf_FT ), 1 ) ) $
	  else	conv = float( FFT( FFT( image, -1 ) * psf_FT, 1 ) )
	conv=shift( conv, -sc(1), -sc(2) )
return, conv
end
