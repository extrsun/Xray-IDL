pro tv_iras_x,ra,dec,irh,text,equinox=equinox,deg=deg $
,radius=radius,color=color,edge=edge,outfile=outfile
;+
; illustrate the field  of view of a PSPC observation on an ISSA image
; ra,dec - RA and DEC of the PSPC observation (default is in radian)
;		which may be got by using trans_radian
; irh - the fits head of the ISSA image
; equinox - the equinox of the PSPC center coordinate (def = 2000)
; deg - if set, the input coordinate is considered to be in degrees
; radius - the radius (in arcminutes) to be drawn on TV
; color - color of the drawing.
; edge - the number pixels wanted to be expanded in plotting the image
; 	positions (def =0)
;
; writen by wqd, Aug 23, 1993
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  tv_iras_x,ra,dec,irh,text'
print,',equinox=equinox,deg=deg,radius=radius,color=color,edge=edge'
return
endif
if n_elements(edge) eq 0 then edge=0
if n_elements(radius) eq 0 then radius=60. ;in units of arcminutes
if keyword_set(deg) eq 0 then begin
	print,'ra, dec are assumed to be in radian'
	trans=180./!pi
endif else trans=1.
rad=ra*trans
decd=dec*trans
if n_elements(equinox) eq 0 then equinox=2000
if equinox ne 1950 then precess,rad,decd,equinox,1950
crval=sxpar(irh,'crval*') ;get iras image center
naxis1=sxpar(irh,'naxis1')
naxis2=sxpar(irh,'naxis2')
irra=crval(0)
irdec=crval(1)
trans_dist,irra,irdec,rad,decd,pxo,pyo,/deg
crpix=sxpar(irh,'crpix*') ;reference center
cdelt=sxpar(irh,'cdelt*')
px=pxo/(7200.*abs(cdelt(0)))+crpix(0) ;in units of iras pixel
py=pyo/(7200.*abs(cdelt(1)))+crpix(1)
pr=radius/(60.*abs(cdelt(1)))

sel=where(px ge 0.-edge and px le naxis1+edge and $
	py ge 0.-edge and py le naxis2+edge,nsel)

if nsel ne 0 then begin
	px=px(sel)
	py=py(sel)
	tvcircle,pr,px,py,color=color
	if n_elements(text) ne 0 then print,text(sel)
	if n_elements(text) ne 0 and n_elements(outfile) ne 0 then begin
		openw,unout,outfile,/get_lun
		printf,unout,text(sel)
		free_lun,unout
	endif
	print,'total selection number = ',nsel
endif
end

