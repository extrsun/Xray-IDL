pro fconv, image_file, kernel_file, conv_file
;+
;			fconv
;
; Computes a convolution from a FITS file image and psf, writes out result 
; to a FITS file.
;
; CALLING SEQUENCE:
;
; INPUTS:
;
;       image_file  - input image file name.
;	kernel_file    - kernel image file name.
;
; INPUTS/OUTPUTS:
;
;	conv_file   - convolved image file name.
;
; METHOD:
;
; EXAMPLES:
;-
;-------------------------------------------------------------------------------
;
        image  = readfits(image_file, header)
;
        kernel = readfits(kernel_file, psfhdr)
;
	conv = convolve(image, kernel)
;
	if n_elements(header) gt 0 then begin
              sxaddhist,'CONVOLVED IMAGE. IMAGE  FILE:'+image_file, header
              sxaddhist,'CONVOLVED IMAGE. KERNEL FILE:'+kernel_file, header
	end
;
        print, 'FITS FILE: '+ conv_file  
;
	if n_elements(conv_file) gt 0 then begin
             print, 'WRITE RESULT TO FITS FILE: '+ conv_file  
       	     result = long(conv*1000)
	     mkhdr, header, result
             writefits, conv_file, result, header
	end
;
        end
