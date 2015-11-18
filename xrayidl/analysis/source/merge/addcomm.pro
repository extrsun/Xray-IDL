pro addcomm,hdr,typenum,comm
;+
; add comments to type keywords a fits header, specifically for the source fits
;	file
; hdr - fits header
; typernum - vector containing the type numbers
; comm - vector containing the comments
; written by wqd, 2/17/2002
;-
if N_params() LT 1 then begin
     print,'Syntax -  addcomm,hdr,typenum,comm'   
     return
 endif 

for k=0,n_elements(typenum)-1 do begin
	type='TTYPE'+strtrim(typenum(k),2)
	tvalue=sxpar(hdr,type)
	sxaddpar,hdr,type,tvalue,comm(k)
endfor
stop
return
end