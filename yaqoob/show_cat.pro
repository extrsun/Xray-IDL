pro show_cat,flist,OUTFILE=outfile
; Displays files in flist showing some parameters
; If outfile is present, results are also dumped to that file
	IF N_PARAMS(0) EQ 0 THEN BEGIN
	  PRINT,'show_cat,flist (,OUTFILE=outfile)'
	  RETURN
	ENDIF
	dump = 0
	IF N_ELEMENTS(OUTFILE) ne 0 THEN BEGIN
	  dump = 1
	  OPENW,lun2,outfile,/GET_LUN
	ENDIF
	PRINT,'Observation Catalogue ',flist
	PRINT,''
	PRINT,'    INSTR   OBJECT   DATAMODE BIT_RATE   DATE-OBS TIME-OBS EXPOSURE   NEVENTS'
	IF dump THEN BEGIN
	  PRINTF,lun2,'Observation Catalogue ',flist
	  PRINTF,lun2,''
	  PRINTF,lun2,'    INSTR   OBJECT   DATAMODE BIT_RATE   DATE-OBS TIME-OBS EXPOSURE   NEVENTS'
	ENDIF
	OPENR,lun,flist,/GET_LUN
	i=1
	a = ''
	WHILE NOT EOF(lun) DO BEGIN
	  READF,lun,a
	  h = HEADFITS(a,EXT=1)
	  b = STRING(i,'$(i3)') + ' '+SXPAR(h,'INSTRUME')+SXPAR(h,'OBJECT')
	  b = b+' '+SXPAR(h,'DATAMODE')+' '+SXPAR(h,'BIT_RATE')+'   '
	  b = b+SXPAR(h,'DATE-OBS')+' '+SXPAR(h,'TIME-OBS')
	  b = b+STRING(DOUBLE(STRMID(h(51),11,20)),'$(f6.0)')+'      '
	  b = b+STRING(SXPAR(h,'NAXIS2'),'$(i6)')
	  PRINT,b
	  IF dump THEN PRINTF,lun2,b
	  i = i+1
	ENDWHILE
	FREE_LUN,lun
	IF dump THEN FREE_LUN,lun2
	RETURN
	END
