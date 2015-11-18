pro source_anal,image_c,image_t,radius,source_cpx,source_cpy
if n_elements(file) eq 0 then file='source_anal
;
sz=size(image_c)
array_size=sz(1)
if n_params() lt 2 then image_t=image_c*0.+1.
scan,center_ra,center_dec,image_c,image_t,image_tsub,radius=radius,file='no',/noiterate
;
print,'find the mass weighted position'
;
image_cp=(array_size-1.)/2.
circle_dist,dis,array_size,image_cp,image_cp
bin_sel=where(image_t gt 0. and dis le radius,nbin)

if nbin eq 0 or nbin eq 0 then begin
print,'No useful bin or count in the image'
return
endif 
; find the i,j positions of the selected bins in the image
j=bin_sel/long(array_size) & i=bin_sel MOD long(array_size) 
;
flux_x=total(image_c(bin_sel)*i)
flux_y=total(image_c(bin_sel)*j)
flux_sum=total(image_c(bin_sel))
source_cpx=flux_x/flux_sum
source_cpy=flux_y/flux_sum
;
if !debug eq 1 then stop
end