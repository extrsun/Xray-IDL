;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*PURPOSE
;	Get the edge coordinates of a portion of an image
;
;*CALLING SEQUENCE:
;	get_edges,array_ra,array_dec,image_ra,image_dec,array_size_x
;       ,array_size_y,bin_size,image_size,hori_start,hori_end,vert_start
;       ,vert_end,block=block,block=block
;
;*PARAMETERS:
; INPUT:
; 	array_ra,array_dec - RA and DEC of the arrays' center (radian)
;	image_ra,image_dec - RA and DEC of the images' center (radian)
;	array_size_x,array_size_y - respectively, dimensions 
;				    (number of bins) in RA and DEC directions
;	bin_size - size of each bin in the arrays (in units of image pixels)
;	image_size - dimension of the images (number of image pixels)
;	block - image pixel size in units of 0''.5 (ROSAT unit), default =30
;
;*INPUTS AND OUTPUTS:
;       hori_start,hori_end,vert_start,vert_end - the edge coordinates
;	 
;*PROCEDURE:
; 
;*RESTRICTION; 
; 
;*NOTES: 
;
;*SUBROUTINES CALLED: 
; 	trans_dist 
; 
;*MODIFICATION HISTORY: 
;	writen 20 AUG 1992 (WQD) 
; 
;-
;------------------------------------------------------------------------------
pro get_edges,array_ra,array_dec,image_ra,image_dec,array_size_x,array_size_y $
,bin_size,image_size,hori_start,hori_end,vert_start,vert_end,block=block
;--------------------------------------------------------------------
if n_params() eq 8 then begin print,'CALLING SEQUENCE -'
print,'get_edges,array_ra,array_dec,image_ra,image_dec'
print,',array_size_x,array_size_y,bin_size,image_size'
print,',hori_start,hori_end,vert_start,vert_end,[block=]'
retall
endif
;--------------------------------------------------------------------
if n_elements(block) eq 0 then block=30
trans_dist, array_ra, array_dec, image_ra, image_dec, hori_dist, vert_dist
	 hori_dist = hori_dist/block ; now in units of the images' pixel
	 vert_dist = vert_dist/block
	edge_side=0
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
;
if !debug eq 1 then stop
      end
