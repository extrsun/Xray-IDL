pro trans_dist,oxcen,oycen,oxfar,oyfar,xdis,ydis,degree=degree,das=das,pixsize=pixsize,angle=angle
;.......................................................................
;
;	Subroutine DIST calculates the distance XDIS and YDIS from
;	center point (XCEN,YCEN) to far point (XFAR,YFAR) in both X
;	and Y directions in pixels of !size_pixel arc seconds. It is the
;	reverse subroutine of LOCT. X and Y directions are defined
;	as perpendicular to and parallel to the meridian passing
;	center point. The calling format of this subroutine is:
;	"call dist (xcen,ycen,xfar,yfar,xdis,ydis)"
; 	pixsize - in units of arcsec
;	angle - total projected angular distance in the same units of xdis
;		and ydis (i.e., angle=sqrt(xdis^2+ydis^2))
;.......................................................................
if(n_params() eq 0) then begin
   print,'CALL SEQUENCE - trans_dist,oxcen,oycen,oxfar,oyfar,XDIS,YDIS,degree=degree,das=das,pixsize=pixsize,angle=angle'
   print,' '
   print,'XDIS,YDIS ---- vectors (pixel, or arcsec if das is set)'
   print,'xcen,ycen ---- scalars (radian)'
   print,'xfar,yfar ---- vectors (radian)'
   print,'if keyword degree is set, radian --> degree'
   return
endif
;
if n_elements(pixsize) eq 0 then pixsize=!size_pixel
;----------------
;convert one-element vector into scalar; otherwise, the result will have one
; element.
censz=size(oxcen)
farsz=size(oxfar)
if censz(0) ne 0 and n_elements(oxcen) eq 1 then begin ;if not a scalar
	oxcen=oxcen(0) ;make it scalar
	oycen=oycen(0)
endif
if farsz(0) ne 0 and n_elements(oxfar) eq 1 then begin ;if not a scalar
	oxfar=oxfar(0) ;make it scalar
	oyfar=oyfar(0)
endif
;......................................................................
;	First, Let's define a parameter for later use. This uses a
;	pre-defined system valuable !size_pixel
;
	if keyword_set(degree) ne 0 then trans=(!dpi/double(180.)) else trans=1.
		xcen=oxcen*trans
		ycen=oycen*trans
		xfar=oxfar*trans
		yfar=oyfar*trans
 
	radpix=648000./!dpi
	if keyword_set(das) eq 0 then radpix=radpix/pixsize
;	
;	We set up a spherical surface with radius equal to
;	one. Center point and far point are on the surface.
;	The distance from far point to the great circle
;	plane passing center point and poles is as follow.
;
;	scf=xcen-xfar
;	if abs(scf) gt !dpi then scf=scf-sign(scf)*!dpi
;	xnorm=cos(yfar)*sin(scf) ; the west is possitive
	xnorm=cos(yfar)*sin(xcen-xfar) ; the west is possitive
;
;	The distance from far point to the great circle
;	plane passing center point and perpendicular
;	to the one mentioned above is as follow.
;
	ynorm=cos(ycen)*sin(yfar)
	ynorm=ynorm-sin(ycen)*cos(yfar)*cos(xcen-xfar)
; now it is the order of IDL array data format, i.e. the north is possitive
;
;	The distance from far point to the line of radius
;	passing center point is the square root of the
;	sum of square of the above two normal distances.
;
	sang=xnorm*xnorm
	sang=sqrt(sang+ynorm*ynorm)
;
;	This distance is also the sine of the angle between
;	far point and center point viewed from the origin.
;
	angle=asin(sang < 1.0)
;
;	This angle could be great than half
;	pi if cosine of it is less than zreo.
;
	cang=sin(ycen)*sin(yfar)
	cang=cang+cos(ycen)*cos(yfar)*cos(xcen-xfar)
	c=where(cang LT 0.0,count)
	if count ne 0 then angle(c)=!dpi-angle(c)
	c=0
;
;	Now convert the angle into pixels.
;
	angle=angle*radpix
;
;	For small SANG, there are two cases. Far point is
;	near center point or on the opposite side of it.
;
;	When the far point is near the center, the spherical
;	surface is approximately a plane in that small ares.
;
	xdis=xnorm*radpix
	ydis=ynorm*radpix
;
;	If far point is on the opposite side of the spherical
;	surface, it is really unimportant. But we still
;	calculate it to complete the whole spherical surface.
;
	c=where(cang LT 0.0,count)
	cang=0
	if count NE 0 then begin

		phi=tan(imdiv(abs(ynorm(c)),xnorm(c)))
		c2=where (xnorm(c) EQ 0.0,count)
		if count NE 0 then  phi(c2)=!dpi/2.0

		xdis(c)=cos(phi)*angle(c)
		ydis(c)=sin(phi)*angle(c)
		phi=0
		c2=where (xnorm(c) LT 0.0,count) 
		if count NE 0 then xdis(c(c2))=-xdis(c(c2))
		c2=where (ynorm(c) LT 0.0,count)
		if count NE 0 then ydis(c(c2))=-ydis(c(c2))
	endif
;
;	For large SANG, we need to treat in a different way.
;	The X and Y components of this distance are proportional
;	to XNORM and YNORM respectively. 
;
	c=where (sang  GE 0.0001,count) 
	if count NE 0 then begin
		xdis(c)=xnorm(c)*angle(c)/sang(c)
		ydis(c)=ynorm(c)*angle(c)/sang(c)
	endif
;
;	Return to the calling routine.
;
	return
	end
;
;.......................................................................
;
;	  	END OF SUBROUTINE DIST
;
;.......................................................................
