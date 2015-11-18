pro exp_broad,radius,image_c,image_t,filter,image_tbroad
;+
; get flux waighted exposure map. The fluxes are calculated within the
; central region of the images.
;
; radius - the radius (in pixels) within which the fluxes are calculated
; image_c - the array including count images in the bands
;		i.e., print,size(image_c)=512,512,3 means the array
;		 contains 3 images
; image_t -the array including exposure images in the bands
; filter - an exposure image with 
;	   discrete sources should have been subtracted 
; image_tboad - the composite exposure image 
; writen by wqd, april 18, 1996
;-
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - exp_broad,radius,image_c,image_t,filter,image_tbroad'
return
endif
;
sz=size(image_c)
;
if radius gt sz(1) then $
 print,'**Warning: the radius is greater than the dimension of the image_c'
;
radius =sz(1) < radius ;assuming square array

dist_circle,dis,sz(1),(sz(1)-1.)*0.5,(sz(1)-1.)*0.5
;
sel=where(dis le radius and filter gt 0.,nsel)
;
nband=sz(3)
;
totflux=0.
counts=fltarr(sz(1),sz(2))
for k=0,(nband-1) do begin
	c=image_c(*,*,k)
	t=image_t(*,*,k)
	flux=total(c(sel)/t(sel))/nsel
	counts=counts+flux*t
	totflux=totflux+flux
endfor
;
image_tbroad=counts/totflux
;
if !debug eq 1 then stop
end
