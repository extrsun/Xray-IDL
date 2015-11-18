pro get_asca_date,time,mon,day,year,hr,min,sec,cdate,ctime
;Author A. Ptak 1993->**
;Last modified 29/10/95 [T. Yaqoob]
	IF N_PARAMS(0) lt 4 THEN BEGIN
	  PRINT,'GET_ASCA_DATE,time,mo,day,year,hr,min,sec,cdate,ctime'
	  PRINT,'Returns universal time for asca event occuring at time time'
          print,'CDATE and CTIME are strings, the remaining output
	  print,'parameters are numbers'  
	  RETURN
	ENDIF
	d = 3600. * 24
;	yr = d*365.25
	yr = d*365.0
	year = FIX(time / yr) 
	t = time - year*yr
	year = year+1993
	jul = FIX(t/d)
	jyear = year
	caldat,jul+1,mon,day,jyear
	t = t-jul*d
	hr = FIX(t/3600.)
	t = t-hr*3600.
	min  = FIX(t/60.)
	sec = t-min*60.
	if day ge 10 then sday=strmid(string(day),10,2) else $
	 sday='0'+strmid(string(day),11,1)
 	if mon ge 10 then  begin
 	smnth=strmid(string(mon),10,2)
	endif else begin
	smnth='0'+strmid(string(mon),11,1)
 	endelse
	syr=strmid(string(year),6,2)
	if hr ge 10 then shr=strmid(string(hr),6,2) else $
	 shr='0'+strmid(string(hr),7,1)
	if min ge 10 then smin=strmid(string(min),6,2) else $
	 smin='0'+strmid(string(min),7,1) 
	if sec ge 10 then ssec=strmid(string(sec),7,2) else $
	 ssec = '0'+strmid(string(sec),8,1)
 	cdate = sday+'/'+smnth+'/'+syr
	ctime = shr+':'+smin+':'+ssec
	RETURN
	END

