pro solar_add,list_image,solarf,solarferr,outfile=outfile
; creat a new file containing image names and solar fluxes
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - solar_add,list_image,solarf,solarferr,outfile=outfile'
return
endif
;
if n_elements(outfile) eq 0 then outfile=list_image+'_s'
seq_no=''
openr,un,list_image,/get_lun
openw,15,outfile
readf,un,nfile
printf,15,nfile
solarf=solarf > 1.e-10 
;the arbitrary small non-zero value is for creating backgroup map
for k=0,nfile-1 do begin
	readf,un,seq_no
	seq_no=strmid(seq_no,0,8)
	printf,15,seq_no,solarf(k),solarferr(k)
endfor
close,15
free_lun,un
end