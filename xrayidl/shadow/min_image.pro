pro min_image,image,minloc,minval
;-
; to get the minimum values and their index locations along the first axis
; of an image
; writen by WQD, Oct. 19, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - min_image,image,minloc,minval'
return
endif
if n_elements(axis) eq 0 then axis=1
sz=size(image)
minloc=lindgen(sz(1))
minval=fltarr(sz(1))
for k=0,sz(1)-1 do begin
	minval(k)=min(image(k,*),index)
	minloc(k)=index
endfor
end