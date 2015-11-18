pro get_soupara,image_c,image_t,cntr,cntre,source_cpx,source_cpy,block=block $
,xc=xc,yc=yc,image_b=image,pixcenter=pixcenter
;+
; find the count rate (and error) and count (or flux) weighted center of a 
; source, i.e. the gravity center
;
; inputs: 
; image_c, image_t - count and exposure image (if a flux image is 
; 	given for image_c, image_t should have a abs(pixel value)=1
; image_b - background subtraction image
; block - block size of the image in units of 0".5. def = 30
; xc, yc - SASS pixel position of the image center (in units of 0.5" pixels
;		with the lowerleft corner pixel at (1,1))
; outputs:
; cntr, cntre - count rate and its error
; source_cpx,source_cpy - the standard SASS pixel position
;	i.e., in units of 0.5" pixels with the lower left corner at (1,1)
;
; writen by wqd, Dec 1994
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_soupara,image_c,image_t,cntr,cntre'
print,',source_cpx,source_cpy,block=block,xc=xc,yc=yc,image_b=image_b'
print,'pixcenter=pixcenter'
return
endif
;
if n_elements(block) eq 0 then block=!block
if n_elements(xc) eq 0  then begin
 if keyword_set(pixcenter) eq 0 then begin
	if !instr eq 'p' then begin
		xc=7680.5 
		yc=7680.5
	endif else begin
		xc=4096.5
		yc=4096.5
	endelse
 endif else begin
	xc=0 & yc=0
 endelse
endif

if n_elements(image_b) eq 0 then image_b=image_t*0.

sz=size(image_c)

ind_s=where(image_t lt 0.,numpix_s)
binc_s=image_c(ind_s)
bint_s=-image_t(ind_s)
binb_s=image_b(ind_s)
ind_b=where(image_t gt 0,numpix_b)
binc_b=image_c(ind_b)
bint_b=image_t(ind_b)
binb_b=image_b(ind_b)

bflux=total(binc_b-binb_b)/total(bint_b)
tc=total(binc_s)
tb=total(binb_s)
tt=total(bint_s)
cntr=((tc-tb)/tt-bflux)*numpix_s
cntre=sqrt(tc > bflux*tt)/tt*numpix_s
print,'cntr,cntre = ',cntr,cntre

; find the i,j positions of the selected bins in the image
j=ind_s/long(sz(1)) & i=ind_s MOD long(sz(1)) 
;
nbinc_s=binc_s-bflux*bint_s
flux_x=total(nbinc_s*i)
flux_y=total(nbinc_s*j)
flux_sum=float(total(nbinc_s))
source_cpx=flux_x/flux_sum
source_cpy=flux_y/flux_sum
print,'source_cpx,source_cpy = ',source_cpx,source_cpy
; get the position in the standard SASS pixel
hsizex=(sz(1)-1.)/2.
hsizey=(sz(2)-1.)/2.
source_cpx=xc+(source_cpx-hsizex)*block
source_cpy=yc+(source_cpy-hsizey)*block
print,'source_cpx,source_cpy = ',source_cpx,source_cpy,' in units of standard bins 0.5 arcsec'
;
if !debug eq 1 then stop
end