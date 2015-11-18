pro apm_file2,idsel,magmax,infile,outfile,clo,chi
;-
; select lines with the desirable sources in the standard APM format
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - apm_file,idsel,magmax,infile,outfile,clo,chi'
retall
endif
if n_elements(clo) eq 0 then clo=-20
if n_elements(chi) eq 0 then chi=1
	openr,uin,infile,/get_lun
	openw,uout,outfile,/get_lun
	parac=''
for k=1,8 do readf,uin,parac,form='(a107)' ;getting rid of initial comments
	while not EOF(uin) do begin
           readf,uin,parac,form='(a107)'
	   color=float(strmid(parac,30,5))
	   bmag=float(strmid(parac,66,6))
	   color=bmag-color
;	   id=fix(strmid(parac,73,4))
	   id=fix(strmid(parac,37,2))
	   if (bmag lt magmax and bmag gt 0. $
		and id eq idsel and color gt clo and color le chi) then $
	   	printf,uout,parac
	endwhile
	free_lun,uout
	free_lun,uin
return
end