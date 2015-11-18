pro get_center,image_c,source_cpx,source_cpy,radius=radius,block=block $
,xc=xc,yc=yc,psel=psel
;+
not working! need to subtract the local background
; find the count (or flux) weighted center of a source, i.e. the gravity center
; Giving both positions in the current image and the position in the standard
; SASS pixel
;
; inputs: 
; image_c - count (or flux) image
; radius - radius of the circle in which the data are going to be used 
;      (in units of arcminutes) default=!core_size
; block - block size of the image in units of 0".5. def = 30
; xc, yc - pixel position of the image center (in units of 15" pixels)
;		def = 256, 256 (as in the SASS image)
; psel - if chosen, the crude estimate of the source position will 
;	be found on the current displayed image. default=center of the image
;
; outputs:
; source_cpx,source_cpy - the standard bin position 
;		i.e., in units of 15" pixels in a 512x512 image 
;
; writen by wqd, Aug 1992
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_center,image_c,source_cpx,source_cpy'
print,',radius=radius,block=block,xc=xc,yc=yc,psel=psel'
return
endif
;
if n_elements(radius) eq 0 then radius=!core_size
if n_elements(block) eq 0 then block=!block
if n_elements(xc) eq 0 then xc=255.5
if n_elements(yc) eq 0 then yc=255.5
radius_p=radius*60./(block*0.5)
sz=size(image_c)
array_size=sz(1)

if n_elements(psel) ne 0 then begin
	image_cpx=0. & image_cpy=0.
	print,'press the righ button to select the source'
	while (!err ne 4) do begin
	cursor,image_cpx,image_cpy,/device,/down
	endwhile
	print,'The selected position is :',image_cpx,image_cpy
endif else begin
	image_cpx=(array_size-1.)/2. & image_cpy=image_cpx ;square image is
					;assumed in this case
endelse
dist_circle,dis,array_size,image_cpx,image_cpy
bin_sel=where(dis le radius_p,nbin)
if nbin eq 0 or nbin eq 0 then begin
print,'No useful bin or count in the image'
return
endif 
; find the i,j positions of the selected bins in the image
j=bin_sel/long(array_size) & i=bin_sel MOD long(array_size) 
;
flux_x=total(image_c(bin_sel)*i)
flux_y=total(image_c(bin_sel)*j)
flux_sum=float(total(image_c(bin_sel)))
source_cpx=flux_x/flux_sum
source_cpy=flux_y/flux_sum
print,'source_cpx,source_cpy = ',source_cpx,source_cpy
; get the position in the standard SASS pixel
hsizex=(sz(1)-1.)/2.
hsizey=(sz(2)-1.)/2.
source_cpx=xc+(source_cpx-hsizex)*block
source_cpy=yc+(source_cpy-hsizey)*block
print,'source_cpx,source_cpy = ',source_cpx,source_cpy,' in units of standard bins 15 arcsec'
;
if !debug eq 1 then stop
end