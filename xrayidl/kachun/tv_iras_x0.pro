pro tv_iras_x0,ra,dec,irh,text,sequi=sequi,deg=deg,fequi=fequi, $
  radius=radius,color=color,edge=edge,outfile=outfile,corner=corner
;**+
;** Illustrate the field  of view of a PSPC observation on an ISSA image
;**
;** INPUTS:
;** 		ra,dec	 == RA and DEC of the PSPC observation (default
;**			    is in radian) which may be gotten by using
;**			    trans_radian;
;** 		irh	 == fits header of the ISSA image;
;** 		sequi	 == sequi of the PSPC center coordinate (def = 2000);
;**		deg  	 == if set, the input coordinate is considered to
;**			    be in degrees;
;** 		radius	 == the radius (in arcminutes) to be drawn on TV;
;** 		color	 == color of the drawing;
;** 		edge	 == the number pixels wanted to be expanded in
;**			    plotting the image positions (def =0).
;**		corner	 == array containing normalized coordinates for
;**			    the corners of the ISSA image if plot is to
;**			    be made on a image plotted using cont_grey.
;**
;** written by wqd, Aug 23, 1993;
;** Modified by kachun 24 July, 1994; added option to plot in normalized
;**   coordinates.
;**-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  tv_iras_x0,ra,dec,irh,text'
print,',sequi=sequi,deg=deg,radius=radius,color=color,edge=edge'
return
endif
if n_elements(edge) eq 0 then edge=0
if n_elements(radius) eq 0 then radius = 60. ;** in units of arcminutes
if keyword_set(deg) eq 0 then begin
	print,'ra, dec are assumed to be in radian'
	trans = 180./!pi
endif else trans = 1.
rad = ra*trans
decd = dec*trans
if n_elements(sequi) eq 0 then begin
	sequi = 2000
	print,'equi = 2000 is assumed for PSPC obs'
endif
if n_elements(fequi) eq 0 then begin
	fequi = 1950
	print,'equi=1950 is assumed for IRAS image'
endif
if sequi ne fequi then precess,rad,decd,sequi,fequi
crval = sxpar(irh,'crval*')                  ;** Get IRAS image center.
naxis1 = sxpar(irh,'naxis1')
naxis2 = sxpar(irh,'naxis2')
irra = crval(0)
irdec = crval(1)
trans_dist,irra,irdec,rad,decd,pxo,pyo,/deg
crpix = sxpar(irh,'crpix*')                  ;** Reference center.
cdelt = sxpar(irh,'cdelt*')

px = pxo/(7200.*abs(cdelt(0)))+crpix(0)      ;** In units of IRAS pixel.
py = pyo/(7200.*abs(cdelt(1)))+crpix(1)
pr = radius/(60.*abs(cdelt(1)))

;** For overplotting onto images created by cont_grey:

if n_elements(corner) ne 0 then begin
	nx1 = corner(0) & nx2 = corner(1)
	ny1 = corner(2) & ny2 = corner(3)
	dx = nx2 - nx1  & dy = ny2 - ny1
  ;** Compute position in normalized coordinates:
	pxn = nx1 + dx*px/(naxis1) & pyn = ny1 + dy*py/(naxis2)
  ;** Convert to display pixel coordinates:
	px = pxn*!d.x_vsize & py = pyn*!d.y_vsize
  ;** Change size of radius from display pixel size (used in TV) to
  ;** cont_grey pixel size in units of the display pixel size:
	pr = pr*(dx/naxis1)*(!d.x_vsize/1.)
endif

;sel=where(px ge 0.-edge and px le naxis1+edge and $
;	py ge 0.-edge and py le naxis2+edge,nsel)

;if nsel ne 0 then begin
;	px=px(sel)
;	py=py(sel)
	tvcircle0,pr,px,py,color=color
;	if n_elements(text) ne 0 then print,text(sel)
;	if n_elements(text) ne 0 and n_elements(outfile) ne 0 then begin
;		openw,unout,outfile,/get_lun
;		printf,unout,text(sel)
;		free_lun,unout
;	endif
;	print,'total selection number = ',nsel
;endif
end
