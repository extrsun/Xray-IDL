pro output_int,timeint,totexp,outfile=outfile
;+
; output the time intervals into a file
; totexp is used as the livetime, May 7, 1994, wqd
;-
if N_params() eq 0 then begin
print,'CALLING SEQUENCE - output_int,timeint,totexp,outfile=outfile'
return
endif
;
if n_elements(outfile) eq 0 then outfile=!seq_no+'_actime.dat'
;
print,'output the time intervals into the file; ',outfile
sz=size(timeint)
print,'The size of timeint = ',sz
if sz(0) eq 1 then ntimeint=1 else ntimeint=sz(2)
timesel=total(timeint(1,*)-timeint(0,*))
print,'the total number of the time intervals = ',ntimeint
print,'the total time included in the intervals = ',timesel
if n_elements(totexp) eq 0 then totexp=timesel
print,'total live time = ',totexp
openw,unit,!data_dir+outfile,/get_lun
printf,unit,ntimeint,totexp
timeint=long(timeint) ;to retain the adequate didgets
for i=0,ntimeint-1 do begin
	 printf,unit,timeint(0,i),timeint(1,i)
	 print,timeint(0,i),timeint(1,i)
endfor
free_lun,unit

end