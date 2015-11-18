pro get_star_area,array_size,star_area,bin_sel=bin_sel,block=block,frac=frac
;+
; get the source search radii for bins in an image
; WQD, Nov 30, 1992
;-
if n_params() eq 0 then begin
print,'get_star_area,array_size,star_area,bin_sel=bin_sel,block=block,frac=frac'
return
endif
if n_elements(block) eq 0 then block=!block
if n_elements(frac) eq 0 then frac=1.
bin_real = float(block)*!size_pixel/60. ; in units of arcminutes
;
dist_circle,dis,array_size,(array_size-1.)/2.,(array_size-1.)/2.
if n_elements(bin_sel) ne 0 then dis=dis(bin_sel)
dis=dis*bin_real
detect_params,dis,core_size,ann_in
star_area=nint(frac*core_size/bin_real)
end
