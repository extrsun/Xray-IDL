pro trans_degree,oxrad,oyrad,ihour,imin,xsec,jdeg,jmin,ysec,degree=degree,print=print
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
   print,'CALL SEQUENCE - trans_degree, xrad,yrad,IHOUR,IMIN,XSEC,JDEG,JMIN,YSEC,degree=degree,print=print'
   return
endif
;......................................................................
;	We can easily convert XRAD totally into hours.
;
	if keyword_set(degree) ne 0 then trans=(!dpi/180.0d) else trans=1.0d
	xrad=oxrad*trans
	yrad=oyrad*trans
	xhour=xrad*12.0d/!dpi
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
	ydeg=yrad*180.0d/!dpi
	jdeg=fix(ydeg)
;
;	For negative declination, we only put the negative sign on
;	the first none zreo value and keep the rest to be positive.
;
	ymin=abs(ydeg-float(jdeg))*60.0d
	jmin=fix(ymin)
	ysec=(ymin-float(jmin))*60.0d
;
;	For negative declination, we have to put the negative
;	sign on minute number if degree number is zreo.
;
	c= where ( yrad LT 0.0 AND jdeg EQ 0,count)
	if count EQ 0 then goto,done
;
	jmin(c)=-jmin(c)
;
;	Or even on second number if minute number is also zreo.
;
	c=where ( jmin(c) EQ 0,count)
	if count EQ 0 then goto,done
	 ysec(c)=-ysec(c)
;
;	Now return to the calling routine.
;
done:
if keyword_set(print) ne 0 then print,'RA, Dec = ',ihour,imin,xsec,jdeg,jmin,ysec
	return
	end
;
;.......................................................................
;
;		END OF SUBROUTINE DEGREE
;
;.......................................................................
