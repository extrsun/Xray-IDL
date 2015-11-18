pro image_comp3,image_c,image_t,image_ts,frac=frac

if n_params() lt 3 then begin
print,'the program requires three imput images'
return
endif
sel=where(image_ts le 0.,nsel)
	if nsel ne 0 then image_c(sel)=0.
	image_c=image_comp(image_c,frac)
	image_t=image_comp(image_t,frac)
	image_ts=image_comp(image_ts,frac)
return
end