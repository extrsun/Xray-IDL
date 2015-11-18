function image_comp,image,factor,sample=sample
; to compress a integer image (count map) without scaling by the compress
; factor

if n_elements(sample) eq 0 then sample=0
if sample eq 0 then print,'sample eq 0; interpolation or neighborhood average is used'
sz=size(image)
nd=[0,0,0]
for i=1,sz(0) do  begin
nd(i)=nint(sz(i)*factor)
endfor
image_type=sz(n_elements(sz)-2)
if image_type eq 4 or image_type eq 5 then begin   
 case sz(0) of
	1: image_new=rebin(image,nd(1),sample=sample) 
	2: image_new=rebin(image,nd(1),nd(2),sample=sample) 
	else: print,'dimension is not supported'
 end
endif else begin
 case sz(0) of
	1: image_new=nint(rebin(float(image),nd(1),sample=sample)/factor) 
	2: image_new=nint(rebin(float(image),nd(1),nd(2),sample=sample)/factor^2)
	else: print,'dimension is not supported'
 end
endelse
return,image_new
end