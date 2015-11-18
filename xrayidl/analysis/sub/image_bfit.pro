pro image_bfit,cbo,bbo,tbso,frac,fb,degree=degree,median=median
;+
; get a median or a polynormial fit of an image excluding sources
; the image are represented by the count, background, and exposure images
; and can be scaled by frac (e.g., 1./4.)
; degree - the degree of the polynormia.
; fm - the 
;-
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - image_bfit,cbo,bbo,tbso,frac,fb,degree=degree,median=median'
return
endif
if n_elements(degree) eq 0 then degree=1
cb=cbo & bb=bbo & tbs=tbso
image_comp3,cb,bb,tbs,frac=frac
fs=imdiv(cb-bb/frac^2,tbs)
fs=median(fs,5)
if keyword_set(median) eq 0 then begin
	fb=sfit(fs,degree)
	fb=image_comp(fb,1./frac)*frac^2
endif else begin
	image_median,2,fs,bb,tbs,fb,lmin=1
	fb=image_comp(fb,1./frac)*frac^2
endelse
stop
return
end
