pro trans_radian,ihour,imin,xsec,jdeg,jmin,ysec,xrad,yrad,degree=degree
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
   print,'CALL SEQUENCE - trans_radian,ihour,imin,xsec,jdeg,jmin,ysec'
   print,',XRAD,YRAD,degree=degree'
   return
endif
;......................................................................
;
;	It is simple to change from hour, minute and second to radian.
;
	xrad=((xsec/double(60.0)+imin)/60.0+ihour)*(180.0d/12.0d)
	if keyword_set(degree) eq 0 then $
		xrad=!dpi*xrad/180.0d
;
;	For declination, It is a little bit complicated.
;	But first we will find the absolute value of it.
;
	yrad=(ysec/double(60.0)+abs(jmin))/60.0+abs(jdeg)
	if keyword_set(degree) eq 0 then $
		yrad=!dpi*yrad/180.0
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
