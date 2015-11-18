pro image_sub,image_name,center_ra,center_dec,array_size_x,array_size_y $
            ,bin_size,array,harray,image_size=image_size,block=block
;
if n_params() eq 0 then begin
print,'CALL SEQUENCE - image_sub,image_name,center_ra,center_dec,array_size_x'
print,'  ,array_size_y,bin_size,array,harray,image_size=image_size,[block=] '
retall
endif
; get a portion of an image
;
if n_elements(image_size) eq 0 then image_size=500 ;the image size of iras SSF
if n_elements(block) eq 0 then block=180 ;iras pixel size=1'.5 
;
array=lonarr(array_size_x,array_size_y)
;
; get the corresponding image
;
image_name=!data_dir+strtrim(image_name,2)
image=readfits(image_name,h)
;
	crval=sxpar(h,'crval*')/!radeg ; convert degree into radian
	image_ra=crval(0)
	image_dec=crval(1)
;
if !debug eq 1 then stop,'before merge'
get_edges,center_ra,center_dec,image_ra,image_dec,array_size_x,array_size_y $
   ,bin_size,image_size,hori_start,hori_end,vert_start,vert_end,block=block
;
hextract,image,h,array,harray,hori_start,hori_end,vert_start,vert_end
;
if !debug eq 1 then stop
return
end





