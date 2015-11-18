pro make_psf,file,psf
;+
;**********************************************************************
;
;*NAME:
;	make_psf
;
;*PURPOSE:
;	This program constructs a data PSF for ASTRO-D from a ray-tracing
;	simulation.  The PSF is normalized to 1.
;
;*CALLING SEQUENCE:
;	make_psf,file,psf
;
;*PARAMETERS:
;	file = name of the input file containing the simulation
;	psf  = 2-dim array (512x512) containing the PSF
;
;*RESTRICTIONS:
;	none
;
;*SUBROUTINES CALLED:
;	none
;
;*PROCEDURES CALLED:
;	none
;
;*SYSTEM VARAIBLES USED:
;	none
;
;*PROCEDURE:
;	The ray-tracing simulation contains no background, so all that
;	is required is to normalize the image to a sum of 1.  The input
;	file is in FITS format.  The original image is 1024x1024 but
;	the PSF is the inner 512x512 to speed up the execution.
;
;*MODIFICATION HISTORY:
;	Dec 92	SZK	Version 1
;
;**********************************************************************
;-
;
; Opening and reading the FITS file.
data = readfits(file,h)
; Normalizing the PSF
psf = double(data(256:767,256:767))
psf = psf / total(psf)
return
end
