;------------------------------------------------------------------------------
pro bin_pixel,center_ra,center_dec,image_ra,image_dec,sel_bin,sel_pixel $
 ,bin_size,array_1,image_1,image_sel,block=block
;--------------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - bin_pixel,center_ra,center_dec,image_ra,image_dec'
print,',sel_bin,sel_pixel ,bin_size,array_1,image_1,block=block'
return
endif
;--------------------------------------------------------------------
sz=size(array_1)
array_size_x=sz(1)
array_size_y=sz(2)
sz=size(image_1)
image_size=sz(1)
if n_elements(block) eq 0 then block=30
edge_side = 0
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
	image_loc=image_loc(where(image_1(image_loc) gt 0.))
;
	y_bin = long( (float(image_loc/image_size) - (half_image-0.5)) $
                *pixel_size +vert_dist)
	x_bin = long( (float(image_loc MOD image_size) - (half_image-0.5)) $
                *pixel_size + hori_dist)
;
	array_loc = long(array_size_x)*y_bin+x_bin ; in the arrays
;
;

bminmax=minmax(array_loc)
bin=where(sel_bin ge bminmax(0) and sel_bin le bminmax(1),nbin)
;   
if nbin eq 0 then begin
	print,'No non-zero exposure bin is included'
	return
endif
sel_binm=sel_bin(bin)

c=where(array_loc ge min(sel_binm) and array_loc le max(sel_binm),nc)
 	array_loc=array_loc(c)
	image_loc=image_loc(c)

sel_pixel=fltarr(nc)
npixel=0
for k=0,nbin-1 do begin
	sel=where(array_loc eq sel_binm(k),nsel)
	if nsel ne 0 then begin
		sel_pixel(npixel:npixel+nsel-1)=image_loc(sel)
		npixel=npixel+nsel
	endif
endfor
image_sel=image_1*0.
if npixel ne 0 then image_sel(sel_pixel)=image_1(sel_pixel) else $
	print,'no pixel is selected'

end