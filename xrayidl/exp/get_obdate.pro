pro get_obdate,refhead,julian,proc=proc,pri=pri
;+
; obtain the observing time of the observation
; writen by WQD, July, 24 1993
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- get_obdate,refhead,julian,proc=proc,pri=pri'
return
endif
;
if n_elements(proc) eq 0 then proc=!proc

if proc eq 'MPE' then begin
	getnomdate,refhead,yy,mm,dd
	date=strtrim(mm,2)+'/'+strtrim(dd,2)+'/'+strtrim(yy,2)
endif else begin
	dateo=sxpar(refhead,'date-obs')
	date=dateo
	dd=fix(gettok_m(date,'/'))
	mm=fix(gettok_m(date,'/'))
	yy=fix(date)+1900
endelse
	jdcnv,yy,mm,dd,0,julian
if keyword_set(pri) ne 0 then begin
	print,'observing date = ',date
	print,'The PSPC C ended on Jan-25-1991 and the PSPC B Gain changed on Oct-11-1991'
endif

return
end
