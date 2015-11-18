; $Id: calcmeanvalue.pro,v 1.0 2009/01/05
;
; Copyright (c), Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.
;===============================================================

pro calcmeanvalue, filename

;===============================================================
;+
; NAME:
;       calcmeanvalue
; PURPOSE:
;       calculate the total value, mean value and number of the greater than 0 pixel of an image, and print them on the screen.
; CALLING SEQUENCE:
;  	calcmeanvalue, filename
; INPUT PARAMETERS:
;	filename: image file name for calculation. 
; EXAMPLE:
;	calcmeanvalue,'b_1.fits'
; NOTE:
;	
;===============================================================

image=mrdfits(filename,0,fitshead)
number=n_elements(where(image[*,*] gt 0))
totalvalue=total(image[*,*])
meanvalue=totalvalue/float(number)
print,'total value = ',totalvalue, ',   mean value = ',meanvalue, ',   number = ', number

end
