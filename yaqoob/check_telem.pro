pro check_telem,plist,datamode,bit_rate,ccdmode=ccdmode,logfile=logfile

	IF N_PARAMS(0) lt 3 THEN BEGIN
	  PRINT,'CHECK_TELEM,plist,datamode,bit_rate,ccdmode=ccdmode,logfile=logfile
	  PRINT,'Checks for telemetry saturation.  Currently only 4 chip'
	  PRINT,'readout is supported so ignore ccdmode'
	  PRINT,'Saturation points outputed to logfile'
	  PRINT,"Sample usage: check_telem,plist,'h','f',logfile='list.dat'"
	  RETURN
	ENDIF
	log = N_ELEMENTS(logfile) ne 0
	IF N_ELEMENTS(logid) eq 0 THEN logid = ''
	IF log THEN OPENW,lun,logfile,/GET_LUN
	IF N_ELEMENTS(ccdmode) eq 0 THEN ccdmode=4
; Get datamode
	datamode = STRUPCASE(STRMID(datamode,0,1))
	IF datamode eq 'F' THEN mode = 1 ELSE mode = 4
; Get bit_rate
	bit_rate =  STRUPCASE(STRMID(bit_rate,0,1))
	bit_rates = ['H','M','L']
	rate = [256,32,8]
	bit = (rate(WHERE(bit_rates eq bit_rate)))(0)
; Compute telemetry limit
	limit = bit*mode
	PRINT,'Datamode : ',datamode,'       Bit rate: ',bit_rate
	PRINT,'Telemetry limit: ',STRTRIM(STRING(limit),2),' events'
	fexposure = 4*ccdmode		; Nominal exposure
; Check each chip
	FOR ch = 0,3 DO BEGIN
	  w = where(plist.ccd eq ch,count)
	  IF count gt 0 THEN BEGIN
	    chlist = plist(w)
	    t = LONG(chlist.time + 0.1)
	    IF N_ELEMENTS(t) gt 1 THEN t = t(SORT(t))
	    i = 1
	    n = N_ELEMENTS(t)
	    count = 1
	    old = t(0)
	    sat = 0
	    max = 0
	    FOR i=1l,n-1 DO BEGIN
	      IF t(i) eq old THEN BEGIN
	        count = count+1
	        IF (count ge limit) and (not sat) THEN BEGIN
	          sat = 1
	          PRINT, 'Saturation on chip ',STRTRIM(STRING(ch),2),' at (',$
		   STRTRIM(STRING(chlist(i).x),2),',', $
		   STRTRIM(STRING(chlist(i).y),2),'), time ',$
		   STRTRIM(STRING(t(i)),2)
	          IF log THEN BEGIN
		    get_asca_date,chlist(i).time,mo,day,year,hr,min,sec
		    PRINTF,lun,chlist(i).x,chlist(i).y,ch,chlist(i).time,$
		      '       '+STRING(hr,'$(i2)')+':'+STRING(min,'$(i2)')+':'+$
		      STRING(sec,'$(f4.1)')
		  ENDIF
	        ENDIF
	      ENDIF ELSE BEGIN
;	        PRINT,old,count
	        old = t(i)
	        sat = 0
	        count = 1
	      ENDELSE
	    ENDFOR
	    PRINT,'Finished chip ',STRTRIM(STRING(ch),2)
	  ENDIF ELSE PRINT,'No events in chip ',STRTRIM(STRING(ch),2)
	ENDFOR
	IF log THEN free_lun,lun
	RETURN
	END
