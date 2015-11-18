PRO get_hstheadm,header,xsize,ysize,cra,cdec,cpx,cpy,del,ang,equi=equi
;+
; get information from a fits header with CD keywords
;

; ang - angle in units of degree (anti-clockwise)
; del - plate scale in x and y directions.
;
; written by wqd, Jan, 2004 
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_headcdinfo,header,xsize,ysize,cra,cdec,cpx,cpy,delv,ang,equi=equi'
return
endif 

hdr=header
sxaddpar,hdr,'crval1',sxpar(header,'crval2')
sxaddpar,hdr,'crval2',sxpar(header,'crval1')
sxaddpar,hdr,'crpix1',sxpar(header,'crpix2')
sxaddpar,hdr,'crpix2',sxpar(header,'crpix1')
sxaddpar,hdr,'CD1_1',-sxpar(header,'CD1_2')
sxaddpar,hdr,'CD2_1',-sxpar(header,'CD2_2')
sxaddpar,hdr,'CD1_2',-sxpar(header,'CD1_1')
sxaddpar,hdr,'CD2_2',-sxpar(header,'CD2_1')
sxaddpar,hdr,'CTYPE1','RA---TAN'
sxaddpar,hdr,'CTYPE2','DEC---TAN'
stop
;
RETURN
END
