pro file_ren,head,seq_no,filter=filter,plus0=plus0
;-
; to rename files into the default ROSAT files and remove files which are not
; going to be used (when the keyword filter is set).
; The input files should have a notation like: head+'i' where i=1,28
; The output files have the notation like: !seq_no_*.fits where * are 
; specified in the program
;
;*EXAMPLE:
; file_ren,'q0',/filter,/plus0
; This will remove or rename q001 - q027 into files with names !seq_no_*.fits 
;+
if n_params() eq 0 then begin
print,'CALL SEQUENCE - file_ren,head,[dir=,seq_no=,plus0=,filter=]'
retall
endif
;
head=strtrim(head)
;
; check the directory of the files
;
if n_elements(dir) eq 0 then dir=!data_dir else !data_dir=strtrim(dir)
if n_elements(seq_no) eq 0 then seq_no=!seq_no else !seq_no=strtrim(seq_no)
;
print,'The assumed file names (as defined in !seq_no) = ',!seq_no+'_*.fits'
print,'The assumed directory (as defined in !data_dir) is = ',!data_dir
stop,'If these are correct, please type .c to continue'
;
; define the tail names of the files which are going to be renamed
;
tail=strarr(28)
tail(*)='no'
tail(2)='.fits'
tail(4)='_im2.fits'
tail(5)='_im3.fits'
tail(9)='_mex.fits'
tail(12)='_src.fits'
tail(14)='.so'
tail(17)='.cas'
tail(25)='.evr'
filein=''
for i=1,27 do begin
if ( keyword_set(plus0) and i lt 10) then filein=!data_dir+head+strtrim(0,2) $
else filein=!data_dir+head
filein=filein+strtrim(i,2)
if(tail(i) eq 'no' and n_elements(filter)) then begin
spawn, 'rm '+ filein                             ;remove files
print,'file ',filein,' has been removed'
endif else begin
 if tail(i) ne 'no' then begin
 fileout=strtrim(seq_no)+tail(i)
 spawn, 'mv ' + filein + ' ' + fileout           ;rename files
 print,filein,' has been renamed as ',fileout
 endif
endelse
endfor
;
if !debug eq 1 then stop
end