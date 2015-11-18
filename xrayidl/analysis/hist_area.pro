pro hist_area,image_t,posi,kdup,nposi,negative=negative,block=block
;+
; get the histogram of the off-axis area with non-zero exposure
; square image_t is assumed
; outputs: posi (arcmin)
; 	   kdup (arcmin^2)
;-
if n_params() eq 0 then begin
	print,'CALLING SEQUENCE - hist_area,image_t,posi,kdup,nposi'
	print,',negative=negative,block=block'
	return
endif
;
if n_elements(block) eq 0 then block=!block
sz=size(image_t)
dist_circle,dis,sz(1),(sz(1)-1.)*0.5,(sz(1)-1.)*0.5 
if keyword_set(negative) eq 0 then sel=where(image_t gt 0.) else $
	sel=where(image_t lt 0.) 
dis=dis(sel)*(block*!size_pixel/60.) ; in units of arcmin
get_posi,dis,posi,kdup,nposi ;using step=1 arcmin

posi=posi+0.5 		;approximate average locations
kdup=kdup*(block/120.)^2 ;in units of arcmin^2
;
return
end