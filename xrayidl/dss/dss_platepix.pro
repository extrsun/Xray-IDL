pro dss_platepix,xxs,yys,subhead,xx,yy,block=block
;-
; taking care of the nonlinear transformation of the plate geometry
; only the first three terms are now considered. This should be adequate
; as long as the field is small. The approximation is relative to the
; center of the subimage obtained from the software GETIMAGE
; 
; xxs, yys - X and Y coordinates of pixels relative to a reference center
;		with X axis tangent to the RA (in units of arcsec)
; subhead - header of a DSS subimage obtained with GETIMAGE 
; xx, yy - output pixels coordinates relative to the corner
;	of the lower left pixel in the subimage.
; writen by wqd, Jun 5, 1994
;+
;
if n_params() eq 0 then begin
	print,'CALLING SEQUENCE - dss_platepix,xxs,yys,subhead,xx,yy'
	print,',block=block'
	return
endif
if n_elements(block) eq 0 then block=1.
get_dssinfo,subhead,xsz,ysz,xpixelsz,ypixelsz,a,b,objra,objdec,$
	objctx,objcty,cnpix,pltscale,pltra,pltdec,ppo
dss_trans,objctx,objcty,ppo,a,b,xpixelsz,ypixelsz,cpx,cpy
		;cpx and cpy are now the distance from the plate center
		; to the objects in the direction of RA and Dec
den=a(0)*b(0)-a(1)*b(1)
c=[b(0),-a(1)]*(1.e3/(xpixelsz*block*den)) ;in units of the image pixel
d=[-b(1),a(0)]*(1.e3/(ypixelsz*block*den))
x=-c(0)*xxs+c(1)*yys-c(0)*cpx-c(1)*cpy ;relative to the object position
y=-d(0)*xxs+d(1)*yys-d(0)*cpx-d(1)*cpy
;
; calculate the pixel values in the subimage of the DSS plate
xx=((objctx-cnpix(0))/float(block)-0.5)-x
yy=((objcty-cnpix(1))/float(block)-0.5)+y

end