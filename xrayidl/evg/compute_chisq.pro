pro compute_chisq,blur,reblur,sigma,scale,chisq,region=region
;+
;			compute_chisq
;
; Compute the reduced chi-squared per degree of freedom for a restored
; image.
;
; CALLING SEQUENCE:
;	compute_chisq,blur,reblur,sigma,scale,chisq
;
; INPUTS:
;	blur - original image or spectrum
;	reblur - restored image or spectrum reconvolved with the point
;		spread function
;	sigma - standard deviation of the white noise
;	scale - counts/flux unit for computing the Poisson noise.
;
; OPTIONAL KEYWORD INPUTS
;	region - vector of 4 elements giving the region of the image to
;		perform the chi-squared test.  Default is the entire image.
; OUTPUT:
;	chisq - reduced chi-square per degree of freedom.
;
; HISTORY:
;	version 1  D. Lindler   April 1992
;-
;----------------------------------------------------------------------------
	if (scale eq 0) and (sigma eq 0) then begin
		print,'ERROR - No noise model information supplied'
		retall
	endif
;
; set default region if not supplied
;
	s = size(blur) & ns=s(1) & nl=s(2)
	if n_elements(region) lt 4 then region = [0,ns-1,0,nl-1]
;
; Compute poisson contribution
;
	residuals = blur - reblur
;
; variance for Poisson stats.
;
	if scale gt 0 then var_p = (reblur/scale) > (1.0/(scale^2)) $
		      else var_p = 0.0
	noise_variance = sigma^2 + var_p
	x = residuals*residuals/noise_variance
	x = x(region(0):region(1),region(2):region(3))
	chisq = total(x)/n_elements(x)
	return
	end
