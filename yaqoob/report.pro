pro report,plist
; Reports range of x,y,pha & time of plist

	IF N_PARAMS(0) eq 0 THEN BEGIN
	  PRINT,"report,plist"
	  PRINT,"Reports range of x,y,pha & time of plist"
	ENDIF
	PRINT,"$('X range: ',i3,' - ',i3)",MIN(plist.x),MAX(plist.x)
	PRINT,"$('Y range: ',i3,' - ',i3)",MIN(plist.y),MAX(plist.y)
	stime = MIN(plist.time)
	etime = MAX(plist.time)
	PRINT,"$('Beggining time: ',f9.0)",stime
	PRINT,"$('Ending time: ',f9.0)",etime
	PRINT,"$('Exposure: ',f9.0)",etime-stime
	PRINT,"$('Pha range: ',i4,' - ',i4)",MIN(plist.pha),MAX(plist.pha)
	RETURN
	END
