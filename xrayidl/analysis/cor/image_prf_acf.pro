pro image_prf_acf,image_t,tail,angle,image_prf_acf,block=block
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - image_prf_acf,image_t,tail'
print,',angle,image_prf_acf,block=block'
return
endif
;
if n_elements(block) eq 0 then block=!block
;
sel=where(image_t gt 0.,nsel)
if nsel eq 0 then begin
print,'No pixel in the image is selected'
return
endif
;
sz=size(image_t)
size=sz(1)
dist_circle,dis,size,(size-1.)*0.5,(size-1.)*0.5
;
;get unique distances and duplications
get_posi,dis(sel),loci,kdup ;bin size = 1 is assumed in the subroutine
if !debug eq 1 then stop
nloci=n_elements(loci)
;
loci=loci*float(block)/120. ; in units of arcminutes
;
area=nsel*(float(block)/120.)^2  ; in units of arcminutes square
;
prf_acfoff,0.,tail,area,angle,acf ; find the size of the acf
kk=n_elements(angle)
image_prf_acf=fltarr(kk)
for k=0,(nloci-1) do begin
	prf_acfoff,loci(k),tail,area,angle,acf
	image_prf_acf=image_prf_acf+acf*kdup(k)
if !debug eq 1 then stop
endfor
;
image_prf_acf=image_prf_acf/nsel
end