pro object_posi,imfile,osou_sel,sxp,syp,val,imhdr=imhdr
;-
; calculate offset of an image by comparing object positions with
;	their know positions.
; imfile - image file which will not be used if imhdr is given
; osou_sel - vector containing GSC sequence numbers
; sxp, ysp - vector containing x and y object positions (in image pixel)
; imhdr - header containing the image information 
; val - output vector containing x, y and angular offsets in arcsec and radian
; 
; writen by wqd, June 1, 1994
;+
if n_param() eq 0 then begin
print,'CALLING SEQUENCE - object_posi,imfile,osou_sel,sxp,syp,val,imhdr=imhdr'
return
endif
if n_elements(imhdr) eq 0 then imhdr=headfits(imfile)
get_headinfo,imhdr,xsize,ysize,cra,cdec,cpx,cpy,delx,dely,equi=equi
if equi eq 0 then read,'please give the equinox of the output file',equi
if abs(delx) ne dely then print,'delx ne dely'
pixel=dely

trans_loct,sxp-(cpx-1.),syp-(cpy-1.),cra,cdec,sra,sdec,pix=pixel*3600.,/deg

get_offset,cra,cdec,[0],osou_sel,val,ofile=ofile,xra=sra,xdec=sdec $
	,xperr=sra*0.+0.4 ;arbitrary value
end