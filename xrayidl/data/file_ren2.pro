pro file_ren2,head,seq_no
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- file_ren2,head,seq_no'
return
endif
head=strtrim(head,2)
seq_no='rp'+strtrim(seq_no,2)
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
for i=1,27 do begin
	if tail(i) ne 'no' then spawn, 'mv '+ head+tail(i)+' '+ seq_no+tail(i)
endfor
end