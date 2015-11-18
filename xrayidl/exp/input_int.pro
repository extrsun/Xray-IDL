pro input_int,timeint,ntimeint,infile=infile
;+
; input the time intervals from a file
;-
if n_params() eq 0 then begin
print,'CALLING SEQUNECE - input_int,fname,timeint,ntimeint'
return
endif
;
if n_elements(infile) eq 0 then infile=!seq_no+'_actime.dat'
print,'input time intervals from the file: ',infile
        OPENR,unit,!data_dir+infile,/get_lun
        READf,unit,ntimeint
        TIMEINT=DBLARR(2,ntimeint)
        READF,unit,TIMEINT	
free_lun,unit
end