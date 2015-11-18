pro report_gti,gti_file,outf=outf

	IF N_PARAMS(0) eq 0 THEN BEGIN
	  PRINT,'REPORT_GTI,gti_file,outf=outf'
	  PRINT,'Create a report about the time intervals in gti_file'
	  PRINT,'Optionally save result in outf'
	  RETURN
	ENDIF
	flag = N_ELEMENTS(outf) NE 0
	ntmax = 1000
	tstart=dblarr(ntmax) & tstop=dblarr(ntmax) & tint=dblarr(ntmax)
	t1 = 0d & t2 = 0d & t3 = 0d & t4 = 0d & itype = 0
	i = 0
	OPENR,lun,gti_file,/GET_LUN
	WHILE NOT EOF(LUN) DO BEGIN
	  READF,lun,t1,t2,t3,t4,itype
	  tstart(i)=t1 & tstop(i)=t2 & tint(i)=t2-t1 & i=i+1
	ENDWHILE
	ntimes = i
	tstart = tstart(0:ntimes-1) & tstop = tstop(0:ntimes-1)
	tint = tint(0:ntimes-1)
	s = sort(tstart)
	tstart = tstart(s) & tstop = tstop(s) & tint = tint(s)
	tbegin = tstart(0)
	tstart = tstart-tbegin
	tstop = tstop-tbegin
	PRINT,systime(0)
	PRINT,'Filename: ',gti_file
	PRINT,'Starting time: ',STRTRIM(STRING(tbegin,"$(i10)"))
	PRINT,'Total net exposure time: ',STRTRIM(STRING(TOTAL(tint),"$(i10)"))
	PRINT,"$(a,t15,a,t30,a,t45,a)",'Interval','Start Time','End Time',$
	  'Net Time'
	IF flag THEN BEGIN
	  OPENW,lun2,outf,/GET_LUN
	  PRINTF,lun2,systime(0)
	  PRINTF,lun2,'Filename: ',gti_file
	  PRINTF,lun2,'Starting time: ',STRTRIM(STRING(tbegin,"$(i10)"))
	  PRINTF,lun2,'Total net exposure time: ',STRTRIM(STRING(TOTAL(tint),"$(i10)"))
	  PRINTF,lun2,"$(a,t15,a,t30,a,t45,a)",'Interval','Start Time',$
	    'End Time','Net Time'
	ENDIF
	FOR i=0,ntimes-1 DO BEGIN
	  PRINT,"$(i3,t15,i6,t30,i6,t45,i6)",i+1,tstart(i),tstop(i),tint(i)
	  IF FLAG THEN PRINTF,lun2,"$(i3,t15,i6,t30,i6,t45,i6)",i+1,tstart(i),$
	    tstop(i),tint(i)
	ENDFOR
	FREE_LUN,lun
	IF FLAG THEN FREE_LUN,lun2
	RETURN
	END
