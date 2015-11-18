pro oplot_gti,plist,gti_file,ypos=ypos,binsz=binsz,ymax=ymax

	IF N_PARAMS(0) LT 1 THEN BEGIN
	  PRINT,'OPLOT_GTI,plist,gti_file,ypos=ypos,ymax=ymax'
	  PRINT,'Oplot gti regions on top of light curve for plist'
	  PRINT,'If binsz is not given, 16.0 is used'
	  PRINT,'If ypos is not given, 75% of ymax is used'
	  PRINT,'ymax can be given to change the scaling'
	  RETURN
	ENDIF

	IF N_ELEMENTS(binsz) eq 0 THEN binsz = 16.0
	binsz = FLOAT(binsz)
	mint = min(plist.time)
	h = histogram(plist.time-plist(0).time,binsize=binsz)/binsz
	IF N_ELEMENTS(ymax) eq 0 THEN ymax=MAX(h)
	IF N_ELEMENTS(ypos) eq 0 THEN ypos = ymax*0.75
	print,ypos
	taxis = FINDGEN(N_ELEMENTS(h))*binsz+0.5*binsz
	PLOT,taxis,h,yrange=[0,ymax],/xst,/yst
	OPENR,lun,gti_file,/GET_LUN
	t1 = 0d & t2 = 0d & t3 = 0d & t4 = 0d
	WHILE NOT EOF(lun) DO BEGIN
	  READF,lun,t1,t2,t3,t4
	  print,t1-mint,t2-mint,t3,t4
	  OPLOT,[t1-mint,t2-mint],[ypos,ypos]
	ENDWHILE
	FREE_LUN,lun
	RETURN
	END
