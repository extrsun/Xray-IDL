;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; image_cut
;
;*PURPOSE:
; to trim image data outside a circle of a user supplied radius
;
;*CALLING SEQUENCE:
; image_new=IMAGE_CUT(image,radius,keepdim=keepdim,block=block,pixel=pixel
; inner=inner,rec=rec,imc=imc)
;
;*PARAMETERS:
; INPUTS:
; image - a two-dimension array 
; radius - the size of the region to be cut: linear size(s) if the keuword 
;		rec is set; the x and y sizes could be different
; 	 	the radius of a circle if the keuword rec is set.
;		(in units of arcmimutes or pixels if
; the keyword pixel is set) within which the data will be retained
;
;*OPTIONAL INPUTS:
; keepdim - if set, keep the dimension of the input array 
; block - the block of the input array in units of 0"5 (def = 30)
; pixel - if set, the input parameter radius is assumed to in units of pixels
; inner - if set, inner part of the image will be set equal to zero
; rec - if set, the region will be considered to be a square, instead of 
;	circle.
; imcoff - 2-element vector containing the off-center pixel numbers (def=[0,0])
;
;*OUTPUTS:
; image_new - the two dimension array after the values outside the circle
;		have been set equal to zero or trimmed completely if the 
; 		parameter dim_new is present.
;
;*PROCEDURE:
; obvious
;
;*EXAMPLES:
; image_t=image_cut(image_t,20.,dim_new) 
;  The outputs will be an image with only the data within a radius of 20'
; retained and a dimension of 160 pixels (dim_new)
;
;*RESTRICTIONS:
; 
;*NOTES:
;
;*SUBROUTINES CALLED:
; circle_dist
;
;*MODIFICATION HISTORY:
; writen Sept 5 1992 (WQD)
; add the keyword imcoff, wqd, Jan 14, 2003
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function IMAGE_CUT,image,radius,keepdim=keepdim,block=block,pixel=pixel $
,inner=inner,rec=rec,imcoff=imcoff
;
if n_elements(block) eq 0 then block=!block
if n_elements(imcoff) eq 0 then imcoff=[0,0]

sz=size(image)
dim=sz(1:2)
;
if keyword_set(pixel) eq 0 then $
;radius in units of image pixels
radiusp=radius*60./(!size_pixel*block) $
else radiusp=radius
;
edge=fix(dim/2.-radiusp) ;the width of the edge going to be discarded
if keyword_set(keepdim) or keyword_set(inner) then begin 
	dim_new=dim
	image_new=image
	if keyword_set(rec) and total(edge) ne 0 then begin
	 image_new((imcoff(0)+edge(0)):imcoff(0)+dim(0)-edge(0)-1 $
		,(imcoff(1)+edge(1)):imcoff(1)+dim(1)-edge(1)-1)=0
		if keyword_set(inner) eq 0 then $
			image_new=image-image_new
	endif
	imc=(dim_new-1.)/2.+imcoff
endif else  begin
;the size of the image is going to be squeezed
	dim_new=dim-2*edge
	image_new=image((imcoff(0)+edge(0)):(imcoff(0)+edge(0))+dim_new(0)-1 $
	    ,(imcoff(1)+edge(1)):(imcoff(1)+edge(1))+dim_new(1)-1) ;new image
	imc=(dim_new-1.)/2.
endelse

if keyword_set(rec) ne 0 then return,image_new
;
dist_circle,circle,dim_new,imc(0),imc(1)
if keyword_set(inner) eq 0 then begin
	sel=where(circle GT radiusp,nsel)
	if nsel ne 0 then image_new(sel) = 0. 
endif else begin
	sel=where(circle LT radiusp,nsel)
	if nsel ne 0 then image_new(sel) = 0. 
endelse
return,image_new
end