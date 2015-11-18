;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*PURPOSE
; Merge ROSAT count and exposure images to the existing count and exposure 
; arrays
;
;*CALLING SEQUENCE:
;	merge,center_ra,center_dec,image_ra,image_dec,array_size_x,array_size_y
;         ,bin_size,array_c,array_b,array_t,image_size,image_c,image_b,image_t
;         image_tsub,block=block,edge_side = edge_side
;
;*PARAMETERS:
; INPUT:
; 	center_ra,center_dec - RA and DEC of the arrays' center (radian)
;	image_ra,image_dec - RA and DEC of the images' center (radian)
;	array_size_x,array_size_y - dimensions (number of bins) in RA and DEC
;				     diretions
;	bin_size - size of each bin in the arrays (in units of image pixels)
;	image_size - dimension of the images (number of the image pixels)
;	image_c,image_b,image_t - count and exposure images
;	block - image pixel size in the units of 0''.5 (ROSAT unit), default =30
;	edge_side - one-side size (in units of the images' pixels) of the region
;		    (in the images) in which no data are going to be merged
;		    defaut=20
;
; INPUTS AND OUTPUTS:
;	array_c,array_b,array_t - count and exposure arrays
;	
;*PROCEDURE:
;   	Finds the portion of an image which is in the field of the array
;	and was exposed and reads pixels in the portion into the corresponding
;	array.
;
;*RESTRICTION;
;
;*NOTES:
; 	set center_ra to be a negative value to use the center of the images as 
;	the center of the arrays
;
;*SUBROUTINES CALLED:
; 	trans_dist
;
;*MODIFICATION HISTORY:
; writen 3 AUG 1992 (WQD)
;-
;------------------------------------------------------------------------------
pro merge,center_ra,center_dec,image_ra,image_dec,array_size_x,array_size_y $
         ,bin_size,image_size,array_1,image_1,array_2,image_2,array_3,image_3 $
	 ,array_4,image_4,block=block,edge_side=edge_side,keep_zero=keep_zero
;--------------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - merge,center_ra,center_dec,image_ra,image_dec'
print,',array_size_x,array_size_y,bin_size,image_size,array_1,image_1'
print,',array_2,image_2,array_3,image_3,array_4,image_4,block=block'
print,',edge_side=edge_side,keep_zero=keep_zero'
retall
endif
;--------------------------------------------------------------------
n_images=(n_params()-8)/2
if n_images lt 1 then begin
print,'No image has been provided' & stop
endif 
;
if n_elements(block) eq 0 then block=30
if n_elements(edge_side) eq 0 then edge_side = 20
;
; Determine the off-set of the image center from the array center
;
if (center_ra lt 0.0) then begin
         center_ra = image_ra
         center_dec = image_dec
         hori_dist = 0.0
         vert_dist = 0.0
endif else begin
trans_dist, center_ra, center_dec, image_ra, image_dec, hori_dist, vert_dist
	 hori_dist = hori_dist/block ; now in units of the images' pixel
	 vert_dist = vert_dist/block
endelse
;----------------------------------------------------------
; define the boundaries of the images covered by the arrays
;
;     Start:

       half_array_x = float(array_size_x * bin_size) * 0.5
       half_array_y = float(array_size_y * bin_size) * 0.5
       half_image = float(image_size)*0.5

;     left side:

       hori_start = fix(half_image+0.5 - hori_dist - half_array_x) ;-0.5+1=0.5
       hori_start = hori_start > edge_side

;     right side:

       hori_end = fix(half_image-0.5 - hori_dist + half_array_x) ;-0.5 only
       hori_end = hori_end < (image_size-1-edge_side)

;     upper side:

       vert_start = fix(half_image+0.5 - vert_dist - half_array_y)
       vert_start = vert_start > edge_side

;     lower side:

       vert_end = fix(half_image-0.5 - vert_dist + half_array_y)
       vert_end = vert_end < (image_size-1-edge_side)

;     if the image is too far away, there will be no coverage:

       if (hori_start gt hori_end or vert_start gt vert_end) then begin
	print,'the array does not cover the image'
	return
       endif

;     END
;-----------------------------------------------------------------------
       pixel_size = 1.0 / float(bin_size)
       hori_dist = (hori_dist + half_array_x) * pixel_size
       vert_dist = (vert_dist + half_array_y) * pixel_size

;     examine  pixel positions in the images and in the arrays:

	image_loc = lindgen(image_size,image_size)
	image_loc = image_loc(hori_start:hori_end,vert_start:vert_end)
;
	y_bin = long( (float(image_loc/image_size) - (half_image-0.5)) $
                *pixel_size +vert_dist)
	x_bin = long( (float(image_loc MOD image_size) - (half_image-0.5)) $
                *pixel_size + hori_dist)
;
	array_loc = long(array_size_x)*y_bin+x_bin ; in the arrays
;
     if not keyword_set(keep_zero) then begin
;    only the pixels really exposed  will be used.
;    if the number of images larger than one, assume that the non-zero
;    values of the last image defined the exposed pixels
;   
      if n_images eq 1 then c=where(image_1(image_loc) ne 0.,nonzero)
      if n_images eq 2 then c=where(image_2(image_loc) ne 0.,nonzero)
      if n_images ge 3 then c=where(image_3(image_loc) ne 0.,nonzero)
; all image_t not= 0 bins will be merged
;
      if nonzero gt 0 then begin
 	array_loc=array_loc(c)
	image_loc=image_loc(c)
      endif
     endif
;
;-------------------------------------------------------------
;    add these pixels to the arrays:
;
;
     for n=1,n_images do begin
 	if n eq 1 then array_1=image_add(array_1,array_loc,image_1,image_loc)
	if n eq 2 then array_2=image_add(array_2,array_loc,image_2,image_loc)
	if n eq 3 then array_3=image_add(array_3,array_loc,image_3,image_loc)
	if n eq 4 then array_4=image_add(array_4,array_loc,image_4,image_loc)
     endfor
;
if !debug eq 1 then stop
      end
;--------------------------------------------------------------
;--------------------------------------------------------------
function image_add,array,array_loc,image,image_loc
      for k=0L,(n_elements(image_loc)-1) do begin
	k_array=array_loc(k)
  	array(k_array) = array(k_array) + image(image_loc(k))
      endfor
return,array
end