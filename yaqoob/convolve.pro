function convolve, image, kernel
;+
;			convolve
;
; Computes a 2-D FFT convolution.
;
; CALLING SEQUENCE:
;
; INPUTS:
;
;       image  - input image array to be convolved.
;	kernel - convolution kernel image array.
;
; INPUTS/OUTPUTS:
;
;	result - convolved image.  
;
; METHOD:
;
; EXAMPLES:
;-
;-------------------------------------------------------------------------------
;
; set parameters:
;
	s = size(image) & ns=s(1) & nl=s(2) & npixels = ns*nl
	s = size(kernel) & nsp = s(1) & nlp = s(2)
	itotal = total(image) & ptotal = total(kernel)
;
	print, 'total counts on IMAGE  : ', itotal
	print, 'total counts in KERNEL : ', ptotal
;
; put PSF in correct format:
;
	p = fltarr(ns,nl) & p(0,0) = kernel & p = shift(p,-nsp/2,-nlp/2)
;
; compute FFT of the PSF:
;
	fftp = fft(p,-1)
	cfftp = conj(fftp)			;conjugate of fftp
;
; compute FFT of IMAGE:
;
	result = float(fft( fft(image,-1)*fftp, 1))
;
; normalize to original image counts:
;
        stotal = total(result)
;
	return, result * itotal / stotal
;
	end



