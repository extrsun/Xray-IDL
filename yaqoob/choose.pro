pro choose,fname,n,plist,faint=faint
; Read in nth file from list in fname
; Assumes bright mode data unless faint is equal to something

	IF N_PARAMS(0) lt 3 THEN BEGIN
	  PRINT,'choose,fname,n,plist,faint=faint'
	  PRINT,'Reads in nth file from list in fname'
	  PRINT,'Assumes bright mode data unless faint is equal to something'
	  RETURN
	ENDIF
	bright = N_ELEMENTS(faint) eq 0
	a = ''
	OPENR,lun,fname,/GET_LUN
	FOR i=1,n DO READF,lun,a
	FREE_LUN,lun
	tab = READFITS(a,h,ext=1)
	IF bright THEN mksbpl,h,tab,plist ELSE mksfpl,h,tab,plist
	RETURN
	END
