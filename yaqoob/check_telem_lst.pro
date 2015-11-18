pro check_telem_lst,fname,dir

	IF N_PARAMS(0) lt 1 THEN BEGIN
	  PRINT,'CHECK_TELEM,fname,dir '
	  PRINT,'Check all the event lists in fname for telemetry saturation'
	  RETURN
	ENDIF
	a = ''
	OPENR,lun,fname,/GET_LUN
	WHILE NOT EOF(lun) DO BEGIN
	  READF,lun,a
	  a=dir+a
	  l = STRLEN(a)
	  IF STRMID(a,l-13,1) eq 'S' THEN BEGIN
	    h = HEADFITS(a)
;	    PRINT,long(sxpar(h,'NEVENTS'))
	    IF LONG(sxpar(h,'NEVENTS')) lt 100 THEN $
	      PRINT,'Too few events, skipping ',a ELSE BEGIN
	      PRINT,'Checking ',a
	      IF STRMID(a,l-7,1) eq '1' THEN datamode = 'faint' ELSE $
	        datamode = 'bright'
	      tab = READFITS(a,h,ext=1)
	      IF datamode eq 'faint' THEN ftplist,h,tab,plist ELSE $
	        brplist,h,tab,plist
	      check_telem,plist,datamode,STRMID(a,l-6,1),$
	      logfile=STRMID(a,l-30,l-5)+'.sat'
	    ENDELSE
	  ENDIF
	ENDWHILE
	FREE_LUN,lun
	RETURN
	END
