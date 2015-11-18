pro wltran,im,imwl,wbinsize,wbinrat=wbinrat,wlft=wlft,wnbins=wnbins,wlimg=wlimg
;-
; perform a wavelet transform
;
;*CALLING SEQUENCE:
;       wltran,im,imwl,wbinsize,wbinrat=wbinrat,wlft=wlft,wnbins=wnbins
;
;*PARAMETERS:
; INPUTS:
;	image - input image for wavelet transform
;       wbinsize - Size of the imput array bin (in units of Hat scale)
;		def=1/4
;
; OUTPUTS:
;	imwl - the wavelet transformed image
;       wlimg  - Output 2 dimensional array of (wnbins,wnbins) elements
;                 containing wavelet function binned to input 
;		pixel size (wbinsize)
;       wnbins   - Number of bins for calculating wavelet function array
;		The use of the default, 129 bins, should be ok in most cases
;
;*OPTIONAL IMPUTS or OUTPUTS:
;       wbinrat  - ratio between number of output bins and number of pixels
;                 for calculating wavelet array (default = 1)
;	wlft - passes the Fourier transform of the wavelet function array
;	   Set wlft=0 or without this keyword to get a new wavelet function
; 
;*PROCEDURE:
;
;*RESTRICTIONS:
;
;*NOTES:
;   Be careful - large images  will slow the calculation down considerably!
;   wnbins and wbinrat will be set to the nearest odd integers, so that the
;   centering will come out right
;
;*SUBROUTINES CALLED:
;   convolve
;   calcwl
;
;*MODIFICATION HISTORY:
;    written  by QDW 1/27/95
;+
;------------------------------------------------------------------------------
if n_params () eq 0 then begin
print,'Calling SEQ - wltran,im,imwl,wbinsize,wbinrat=wbinrat,wlft=wlft,wnbins=wnbins,wlimg=wlimg'
print,''
return
endif

if n_elements(wnbins) eq 0 then wnbins=64*2+1
if n_elements(wbinsize) eq 0 then wbinsize=1./4.
;if n_elements(wlft) le 1 then begin
if n_elements(wlimg) le 1 then begin
	print,'get wavelet function: '
	calcwl,wnbins,wbinsize,wlimg,binrat=wbinrat
endif
;print,'Now do the transform'
imwl=convolve(im,wlimg,ft_psf=wlft)
return
end