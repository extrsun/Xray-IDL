;.......................................................................
pro trans_loct,xdis,ydis,oxcen,oycen,xfar,yfar,degree=degree,das=das,pixsize=pixsize,type=type
;.......................................................................
;	Subroutine LOCT finds the location of a far point
;	which is XDIS and YDIS pixels away from center point
;	(XCEN,YCEN). It is the reverse subroutine of DIST.
;	The calling format of this subroutine is:
;	"call loct (xdis,ydis,xcen,ycen,xfar,yfar)"
;	XFAR and YFAR are the right ascension and declination of
;	the far point. They are the output of this subroutine.
; 	pixsize - in units of arcsec
;.......................................................................
if(n_params() eq 0) then begin
   print,'CALL SEQUENCE - trans_loct, xdis,ydis,xcen,ycen,XFAR,YFAR,degree=degree,das=das,pixsize=pixsize,type=type'
   print,' '
   print,'xdis,ydis ---- vectors (pixel, or arcsec if das is set'
   print,'xcen,ycen ---- scalars (radian)'
   print,'XFAR,YFAR ---- vectors (radian)'
   print,'if keyword degree set, radian --> degree' 
   print,'type=1 (def) X axis=RA; type=0, X axis is relative to the ref pixel'
   return
endif
;
xdis=double(xdis)
ydis=double(ydis)

if n_elements(type) eq 0 then type=1
;----------------
;convert one-element vector into scalar; otherwise, the result will have one
; element.
censz=size(oxcen)
if censz(0) ne 0 and n_elements(oxcen) eq 1 then begin ;if not a scalar
	oxcen=oxcen(0) ;make it scalar
	oycen=oycen(0)
endif
;......................................................................
;	First, Let's define a parameter for later use. This uses a
;	pre-defined system valuable !size_pixel (in arcseconds)
;
	if n_elements(pixsize) eq 0 then pixsize=!size_pixel
	if keyword_set(degree) ne 0 then trans=(!dpi/180.) else trans=1.0d
	xcen=oxcen*trans
	ycen=oycen*trans
	radpix=648000.0d/!dpi
	if keyword_set(das) eq 0 then radpix=radpix/pixsize
;	
;	We set up a spherical surface with radius equal to one.
;	Center point and far point are on the surface. Then the
;	distance between far point and center point on the surface
;	is the square root of sum of square of the two distances.
;
	dispix=sqrt(xdis^2+ydis^2)
;
;	This distance can not exceed half circle which is 
;       648000./!size_pixel pixels.
;
	if keyword_set(das) eq 0 then dispix = dispix <  (648000./!size_pixel) $
	else dispix = dispix <  648000.
;
;	This distance is equivalent to the angle between the
;	center point and far point except the fact of RADPIX.
;
	angle=dispix/radpix
;
;	Now we calculate the sine of this angle for further use.
;
	sang=sin(angle)
;
;	Find the two normal distances from the far point to
;	the two great circle planes parallel or perpendicular
;	to the meridian and passing center point.
;
;	If the angle between the two points is very small, then we
;	can consider the spherical surface as a plane in small area.
;
;	xnorm=xdis/radpix
;	ynorm=ydis/radpix
;
;	Otherwise
;
;	c=where(angle GE 0.001,count)
;	if count NE 0 then begin
;		xnorm(c)=xdis(c)*sang(c)/dispix(c)
;		ynorm(c)=ydis(c)*sang(c)/dispix(c)
;	endif
	xnorm=imdiv(xdis*sang,dispix)
	ynorm=imdiv(ydis*sang,dispix)
	dispix=0. ;will no longer be used
	sang=0.
;
;	Now we can calculate the normal distance
;	from the far point to equator plane which
;	is the sine of the declination of far point.
;
	syfar=sin(ycen)*cos(angle) + ynorm*cos(ycen)
	angle=0. ;will no longer be used
; ynorm is assumed to be positive in the direction of the north?
;
;	Then the YFAR is simply worked out.
;
	syfar=(((syfar < 1.0) +1.) > 0.) -1.
	yfar=asin(syfar)
	cyfar=cos(yfar)
	xfar=yfar*0.0
;
	c=where(cyfar EQ 0.0,count)
	if count ne 0 then begin
	xfar(c)=xcen
	if count EQ n_elements(xfar) then return
	endif
;
;	The difference of XFAR and XCEN is calculated.
;
;	difx=xfar*0.0
;	c=where (cyfar NE 0.0)
;	difx(c)=(((xnorm(c)/cyfar(c) < 1.) +1.) > 0.) -1.
;	difx(c)=asin(difx(c))
	difx=(((imdiv(xnorm,cyfar) < 1.) +1.) > 0.) -1.
	difx=asin(difx)
;
;	This diference could be great than
;	half pi if cosine of it is negative.
;
;	acos=(ynorm+syfar*cos(ycen))*sin(ycen) 
		;there is problem in this statemnet. But this is not used
		; for difx < pi
;	sign=where(acos LT 0.0,count)
;stop
;	if count NE 0 then difx(sign)=!dpi-difx(sign)
;
	ynorm=0. ;will no longer be used
	xfar=xcen-difx
	difx=0. ;will no longer be used
      if type ne 0 then begin
;
;	Right ascension is aranged from zero to two pi.
;
	c=where (xfar LT 0.0,count)
 	if count NE 0 then xfar(c)=xfar(c)+2.0*!dpi
	c=where (xfar GT 2.0*!dpi,count)
	if count NE 0 then xfar(c)=xfar(c)-2.0*!dpi
     endif

	if keyword_set(degree) ne 0 then begin
		xfar=xfar*(180./!dpi)
		yfar=yfar*(180./!dpi)
	endif
;
;	Return to the calling routine.
;
	return
;
	end
;
;.......................................................................
;
;		END OF SUBROUTINE LOCT
;
;.......................................................................
