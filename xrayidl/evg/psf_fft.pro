pro psf_fft,image,psf,fftp
;+
;			fft_psf
;
; compute fft of a point spread function inserted into an image
; with the same size as an input image
;
; INPUTS:
;	image - image with size of the desired psf fft.
;	psf - point spread function
;
; OUTPUTS:
;	fftp - fft of the psf
; HISTORY:
;	version 1  D. Lindler
;-
;---------------------------------------------------------------------------
	s = size(psf) & nsp = s(1) & nlp = s(2)
	s = size(image) & ns = s(1) & nl = s(2)
	p = fltarr(ns,nl)
	p(0,0) = psf
	p = shift(p,-nsp/2,-nlp/2)
	fftp = fft(p,-1)
	return
	end
