pro image_loc,image,loc
;+
; convert image counts into a list of location in the image
;-
if N_params() eq 0 then begin
print,'CALLING SEQUENCE -  image_loc,image,loc'
return
endif
;
sz=size(image)
imtype=sz(sz(0)+1)
if imtype lt 1 or imtype gt 3 then begin
	print,'the image needs to be byte, integer, or long'
	return
endif

sel=where(image ge 1,nsel)
if nsel ne 0 then loc=[-999,sel] else loc=[-999]

sel=where(image gt 1,nsel)
if nsel ne 0 then begin
	for i=0,nsel-1 do begin
		loc=[loc,replicate(sel(i),image(sel(i))-1)]
	endfor
endif
if n_elements(loc) eq 1 then begin
	print,'the image does not contain any positive number'
	return
endif else loc=loc(1:*)

return
end
