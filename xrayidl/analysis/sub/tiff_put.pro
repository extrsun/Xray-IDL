pro tiff_put,filename
;+
; to write an image in the current display into a tiff file
; writen by WQD, Aug 9, 1993
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - tiff_put,filename'
return
endif
image=tvrd(0,0,580,400)
tvlct,r,g,b,/get
tiff_write,filename,image,1,red=r,green=g,blue=b
end