;.......................................................................
;TRANS.PRO includes a series of procedures for astrometric parameter transfers.
;The original FORTRAN subroutines were  written by WXY and are translated to
;IDL procedures by WQD (July, 1992). 
;.......................................................................
pro trans_dist,xcen,ycen,xfar,yfar,xdis,ydis
;.......................................................................
;
;	Subroutine DIST calculates the distance XDIS and YDIS from
;	center point (XCEN,YCEN) to far point (XFAR,YFAR) in both X
;	and Y directions in pixels of 8 arc seconds. It is the
;	reverse subroutine of LOCT. X and Y directions are defined
;	as perpendicular to and parallel to the meridian passing
;	center point. The calling format of this subroutine is:
;	"call dist (xcen,ycen,xfar,yfar,xdis,ydis)"
;.......................................................................
if(n_params() eq 0) then begin
   print,'CALL SEQUENCE - trans_dist,xcen,ycen,xfar,yfar,XDIS,YDIS'
   print,' '
   print,'XDIS,YDIS ---- vectors (pixel)'
   print,'xcen,ycen ---- scalars (radian)'
   print,'xfar,yfar ---- vectors (radian) 
   return
endif
;......................................................................
;	First, Let's define a parameter for later use. This uses a
;	pre-defined system valuable !size_pixel
;
	radpix=648000./!pi/!size_pixel
;	
;	We set up a spherical surface with radius equal to
;	one. Center point and far point are on the surface.
;	The distance from far point to the great circle
;	plane passing center point and poles is as follow.
;
	xnorm=cos(yfar)*sin(xcen-xfar) ; the west is possitive
;
;	The distance from far point to the great circle
;	plane passing center point and perpendicular
;	to the one mentioned above is as follow.
;
	ynorm=cos(ycen)*sin(yfar)-sin(ycen)*cos(yfar)*cos(xcen-xfar)
; now it is the order of IDL array data format, i.e. the north is possitive
;
;	The distance from far point to the line of radius
;	passing center point is the square root of the
;	sum of square of the above two normal distances.
;
	sang=xcen*0.0
	c= where ( XNORM NE 0. OR  YNORM NE 0., count)
	if count NE 0 then sang(c)=sqrt(xnorm(c)*xnorm(c)+ynorm(c)*ynorm(c))
;
;	This distance is also the sine of the angle between
;	far point and center point viewed from the origin.
;
	angle=asin(sang < 1.0)
;
;	This angle could be great than half
;	pi if cosine of it is less than zreo.
;
	cang=sin(ycen)*sin(yfar)+cos(ycen)*cos(yfar)*cos(xcen-xfar)
	c=where(cang LT 0.0,count)
	if count ne 0 then angle(c)=!pi-angle(c)
;
;	Now convert the angle into pixels.
;
	dispix=angle*radpix
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
	if count NE 0 then begin
	phi=ynorm*0.0
	c2=where (xnorm(c) NE 0.0,count)
	if count NE 0 then phi(c2)=tan(abs(ynorm(c2)/xnorm(c2))) 
	c2=where (xnorm(c) EQ 0.0,count)
	if count NE 0 then  phi(c2)=!pi/2.0
	xdis(c)=cos(phi(c))*dispix(c)
	ydis(c)=sin(phi(c))*dispix(c)
	c2=where (xnorm(c) LT 0.0,count) 
	if count NE 0 then xdis(c2)=-xdis(c2)
	c2=where (ynorm(c) LT 0.0,count)
	if count NE 0 then ydis(c2)=-ydis(c2)
	endif
;
;	For large SANG, we need to treat in a different way.
;	The X and Y components of this distance are proportional
;	to XNORM and YNORM respectively. 
;
	c=where (sang  GE 0.001,count) 
	if count NE 0 then begin
	xdis(c)=xnorm(c)*dispix(c)/sang(c)
	ydis(c)=ynorm(c)*dispix(c)/sang(c)
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
pro trans_loct,xdis,ydis,xcen,ycen,xfar,yfar
;.......................................................................
;	Subroutine LOCT finds the location of a far point
;	which is XDIS and YDIS pixels away from center point
;	(XCEN,YCEN). It is the reverse subroutine of DIST.
;	The calling format of this subroutine is:
;	"call loct (xdis,ydis,xcen,ycen,xfar,yfar)"
;	XFAR and YFAR are the right ascension and declination of
;	the far point. They are the output of this subroutine.
;.......................................................................
if(n_params() eq 0) then begin
   print,'CALL SEQUENCE - trans_loct, xdis,ydis,xcen,ycen,XFAR,YFAR'
   print,' '
   print,'xdis,ydis ---- vectors (pixel)'
   print,'xcen,ycen ---- scalars (radian)'
   print,'XFAR,YFAR ---- vectors (radian) 
   return
endif
;......................................................................
;	First, Let's define a parameter for later use. This uses a
;	pre-defined system valuable !size_pixel (in arcseconds)
;
	radpix=648000./!pi/!size_pixel
;	
;	We set up a spherical surface with radius equal to one.
;	Center point and far point are on the surface. Then the
;	distance between far point and center point on the surface
;	is the square root of sum of square of the two distances.
;
	dispix=xdis*0.0
	c=where (XDIS NE 0. OR YDIS NE 0.,count)
	if count ne 0 then dispix(c)=sqrt(xdis(c)*xdis(c)+ydis(c)*ydis(c))
;
;	This distance can not exceed half circle which is 
;       648000./!size_pixel pixels.
;
	dispix = dispix <  648000./!size_pixel 
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
	xnorm=xdis/radpix
	ynorm=ydis/radpix
;
;	Otherwise
;
	c=where(angle GE 0.001,count)
	if count NE 0 then begin
	xnorm(c)=xdis(c)*sang(c)/dispix(c)
	ynorm(c)=ydis(c)*sang(c)/dispix(c)
	endif
;
;	Now we can calculate the normal distance
;	from the far point to equator plane which
;	is the sine of the declination of far point.
;
	syfar=sin(ycen)*cos(angle) + ynorm*cos(ycen)
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
	difx=xfar*0.0
	c=where (cyfar NE 0.0)
	difx(c)=(((xnorm(c)/cyfar(c) < 1.) +1.) > 0.) -1.
	difx(c)=asin(difx(c))
;
;	This diference could be great than
;	half pi if cosine of it is negative.
;
	sign=where( (ynorm(c)+syfar(c)*cos(ycen))*sin(ycen) LT 0.0,count)
	if count NE 0 then difx(sign)=pi-difx(sign)
;
	xfar(c)=xcen-difx(c)
;
;	Right ascension is aranged from zero to two pi.
;
	c=where (xfar LT 0.0,count)
 	if count NE 0 then xfar(c)=xfar(c)+2.0*!pi
	c=where (xfar GT 2.0*!pi,count)
	if count NE 0 then xfar(c)=xfar(c)-2.0*!pi
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
pro trans_degree,xrad,yrad,ihour,imin,xsec,jdeg,jmin,ysec
;.......................................................................
;	Subroutine DEGREE convert the right ascension and
;	declination from radians into hour or degree,
;	minute and second. It is the reverse subroutine of
;	RADIAN. The calling format of this subroutine is:
;	"call degree (xrad,yrad,ihour,imin,xsec,jdeg,jmin,ysec)"
;	XRAD and YRAD are input right ascension and declination in
;	radians. The last six values are output right ascension
;	and declination in hour or degree, minute and second.
;.......................................................................
if n_params() eq 0  then begin
   print,'CALL SEQUENCE - trans_degree, xrad,yrad,IHOUR,IMIN,XSEC,JDEG,JMIN,YSEC'
   return
endif
;......................................................................
;	We can easily convert XRAD totally into hours.
;
	xhour=xrad*12.0/!pi
;
;	The hour of right ascension is just integer part of it.
;
	 ihour=fix(xhour)
;
;	Then, we convert the rest of it into minutes.
;
	xmin=(xhour-float(ihour))*60.0
;
;	Again, the minute is just integer part of it.
;
	imin=fix(xmin)
;
;	Then, we convert the rest of it into seconds.
;
	xsec=(xmin-float(imin))*60.0
;
;	We can treat the declination similarly except
;	for the possibility of negative declination.
;
	ydeg=yrad*180.0/!pi
	jdeg=fix(ydeg)
;
;	For negative declination, we only put the negative sign on
;	the first none zreo value and keep the rest to be positive.
;
	ymin=abs(ydeg-float(jdeg))*60.0
	jmin=fix(ymin)
	ysec=(ymin-float(jmin))*60.0
;
;	For negative declination, we have to put the negative
;	sign on minute number if degree number is zreo.
;
	c= where ( yrad LT 0.0 AND jdeg EQ 0,count)
	if count EQ 0 then return
;
	jmin(c)=-jmin(c)
;
;	Or even on second number if minute number is also zreo.
;
	c=where ( jmin(c) EQ 0,count)
	if count EQ 0 then return
	 ysec(c)=-ysec(c)
;
;	Now return to the calling routine.
;
	return
	end
;
;.......................................................................
;
;		END OF SUBROUTINE DEGREE
;
;.......................................................................
pro trans_radian,ihour,imin,xsec,jdeg,jmin,ysec,xrad,yrad
;.......................................................................
;
;	Subroutine RADIAN convert the right ascension and
;	declination from hour or degree, minute and second
;	into redians. It is the reverse subroutine of
;	DEGREE. The calling format of this subroutine is:
;	"call radian (ihour,imin,xsec,jdeg,jmin,ysec,xrad,yrad)"
;	The first six values are input right ascension and
;	declination in hour or degree, minute and second. XRAD and
;	YRAD are output right ascension and declination in radians.
;.......................................................................
if(n_params() eq 0) then begin
   print,'CALL SEQUENCE - trans_radian,ihour,imin,xsec,jdeg,jmin,ysec,XRAD,YRAD'
   return
endif
;......................................................................
;
;	It is simple to change from hour, minute and second to radian.
;
	xrad=!pi*((xsec/60.0+float(imin))/60.0+float(ihour))/12.0
;
;	For declination, It is a little bit complicated.
;	But first we will find the absolute value of it.
;
	yrad=!pi*((ysec/60.0+float(abs(jmin)))/60.0+float(abs(jdeg)))/180.0
;
;	If degree or minute is negative, then YRAD should be negative.
;
	c=where (jdeg LT 0 OR jmin LT 0,count) 
	if count NE 0 then yrad(c)=-yrad(c)
;
;	If both JDEG and JMIN are zreo, then the sign is
;	defined by YSEC, and is already included in the first
;	place. Now we can return to the calling routine.
;
	return
	end
;
;.......................................................................
;
;		END OF SUBROUTINE RADIAN
;
;.......................................................................
;.......................................................................
;
pro trans_getgc,ra,dec,DDL,DDLA
;
;-------------------------------------------------------------------------
;   The subroutine GETGC transfers the earth coordinates right accension
; and declination to the galatic coordinates logitude and latitude
;------------------------------------------------------------------------
if(n_params() eq 0) then begin
   print,'CALL SEQUENCE - trans_getgc,ra,dec,DDL,DDLA'
   return
endif
;......................................................................
;
	C=COS(62.6*!PI/180.)
	S=SIN(62.6*!PI/180.)
	R=282.25*!PI/180.
	SLAT=SIN(DEC)*C-COS(DEC)*SIN(ra-R)*S
	SLAT=SLAT < 1.0
	SLAT=((SLAT+1.) > 0.0 ) -1.
;
	RLAT=ASIN(SLAT)
	DDLA=RLAT*180./!PI
	EC=COS(DEC)*COS(ra-R)/COS(RLAT)
	ES=(COS(DEC)*SIN(ra-R)*C+SIN(DEC)*S)/COS(RLAT)
	ES = ES < 1.0
	EC =((EC+1.) > 0.0) -1. 
;
	DDL=acos(ec)*180./!pi
	c=where (es GE 0.,count) 
	if count NE 0 then ddl(c)=ddl(c)+33.
	c=where (es LT 0.,count)
	if count NE 0 then DDL(c)=360.-DDL(NOT c)+33.
;
	return
	end	
