pro glactc_im,im,hdr,glad,glod,block=block,equinox=equinox,crad=crad,cdecd=cdecd
;-
; calculate the Galactic coordinates of individual pixels in an image
;
; INputs:
; im - array
; hdr - fits head 
;*select inputs:
; block - pixel size in deg (def is from hdr)
; crad, cdecd - ra and dec of the image center in units of degree 
; (def is from hdr)
; equinox - equinox of crad and cdecd (def is from hdr)
; OUTPUTS:
; glod, glad - logitudes and latitutes of all the pixels (degree)
;
; writen by WQD, 4/17/1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - glactc_im,im,hdr,glad,glod,block=block'
print,',equinox=equinox,crad=crad,cdecd=cdecd'
return
endif
if n_elements(crad) eq 0 then crad=sxpar(hdr,'crval1')
if n_elements(cdecd) eq 0 then cdecd=sxpar(hdr,'crval2')
if n_elements(block) eq 0 then begin
	block=sxpar(hdr,'cdelt2')
	if block ne abs(sxpar(hdr,'cdelt1')) then stop,'cdelt2 ne cdelt1'
	block=block*3600./!size_pixel
endif 
if n_elements(equinox) eq 0 then equinox=sxpar(hdr,'equinox',count=count)
if count eq 0 then equinox=sxpar(hdr,'epoch',count=count)
if count eq 0 then begin
	equinox=2000.
	print,'Equinox = ',equinox,' is assumed'
endif
;assuming iras  SSA image in units of !block 
;
sz=size(im)
xdim=sz(1)
ydim=sz(2)
xbc=0.5*(xdim-1.)
ybc=0.5*(ydim-1.)

pixloc=lindgen(xdim,ydim) ;pixel location for the whole image
trans_loct,((pixloc mod xdim)-xbc)*block,((pixloc/xdim)-ybc)*block, $
  crad,cdecd,frad,fdecd,/degree
frad=frad*(12./180.)
; get the Galactic logitude and latitude for the selected pixels
glactc_m,frad,fdecd,equinox,glod,glad,1
stop
end
