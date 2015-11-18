pro apm_file,idsel,magmax,infile,outfile
;-
; select lines with the desirable sources in the standard APM format
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - apm_file,idsel,magmax,infile,outfile'
retall
endif
	openr,uin,infile,/get_lun
	openw,uout,outfile,/get_lun
	parac=''
	while not EOF(uin) do begin
           readf,uin,parac,form='(a86)'
	   bmag=float(strmid(parac,66,7))
	   id=fix(strmid(parac,73,4))
	   if (bmag lt magmax and bmag gt 0. $
		and id eq idsel) then $
	   	printf,uout,parac
	endwhile
	free_lun,uout
	free_lun,uin
return
end