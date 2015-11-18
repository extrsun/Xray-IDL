pro get_fitsheaddss,image,head,refhead,del=del,hist=hist,cpx=cpx,cpy=cpy $
,equi=equi,crval=crval,dssimage=dssimage,blank=blank,block=block
;-
; create a rather comprehensive header for an image using a DSS image header as
;a reference header from a subimage created with GETIMAGE. Default information 
; may be transfered from the reference header to the output header
; 
; image - the reference image. If given as a scale, the dimensions as
;	specified in refhead will be used.
; refhead - the reference header
; head - the output header
; del - the size of a pixel (in units of degrees)
; hist - history record
; cpx, cpy - the reference pixel in the image
; equi - the equinox of the coordinate
; dssimage - if set, the RA and Dec as well as 
;		cpx and cpy of the DSS image will be used
; block - block factor which has been used to create the input image from
;	the dss subplate which is charaterized by rehead
; writen by wqd, Jun 4, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_fitsheaddss,image,head,refhead,del=del,hist=hist'
print,',cpx=cpx,cpy=cpy,equi=equi,crval=crval,dssimage=dssimage,blank=blank'
print,',block=block'
RETURN
ENDIF
if n_elements(block) eq 0 then block=1
; get the reference header information
get_dssinfo,refhead,xsz,ysz,xpixelsz,ypixelsz,a,b,objra,objdec,$
	objctx,objcty,cnpix,pltscale,pltra,pltdec,ppo
mkhdr,head,image ;create a basic header

sz=size(image)
if keyword_set(dssimage) ne 0 then begin
	dss_trans,objctx,objcty,ppo,a,b,xpixelsz,ypixelsz,cpx,cpy
		;cpx and cpy are now the distance from the plate center
		; to the objects in the direction of RA and Dec
	cpx=((objctx-cnpix(0))+cpx/(xpixelsz*pltscale*1.e-3))/block
	cpy=((objcty-cnpix(1))-cpy/(ypixelsz*pltscale*1.e-3))/block
	;The plate center coordinates in units of the 
	; average plate pixels
endif else begin
	if n_elements(cpx) eq 0 then cpx=sz(1)*0.5
	if n_elements(cpy) eq 0 then cpy=sz(1)*0.5
endelse

if n_elements(crval) eq 0 then begin
	if keyword_set(dssimage) ne 0 then crval=[pltra,pltdec] else $
		crval=[objra,objdec] 
endif
sxaddpar,head,'crval1',crval(0)
sxaddpar,head,'crval2',crval(1)
if n_elements(del) ne 0 then cdelt=[-1.,1.]*del else $
	cdelt=(pltscale*block/3.6e6)*[-xpixelsz,ypixelsz]
sxaddpar,head,'cdelt1',cdelt(0)
sxaddpar,head,'cdelt2',cdelt(1)
; use the standard definition instead of the idl coordinate, wqd, Oct 22, 93
sxaddpar,head,'crpix1',cpx
sxaddpar,head,'crpix2',cpy
sxaddpar,head,'bscale',1.
sxaddpar,head,'bzero',0.
sxaddpar,head,'crota1',0.
sxaddpar,head,'crota2',0.
sxaddpar,head,'date',systime(0)
if n_elements(blank) eq 0 then 	blank=-1.
sxaddpar,head,'blank',blank
if n_elements(equi) eq 0 then equi=2000
sxaddpar,head,'equinox',equi
if n_elements(hist) ne 0 then sxaddpar,head,'history',hist
end
