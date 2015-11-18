pro ascapublic_s,infile,agblo,agbhi,sra,sdec,ctext,outfile=outfile
;-	
; writen by WQD, June 15, 1999
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - ascapublic_s,infile,outfile,agblo,agbhi'
return
endif
openr,un,infile,/get
ctextc=''
nl=2237
ctext=strarr(nl)
form='(a50)'
for k=0,nl-1 do begin
	readf,un,ctextc,format=form
	ctext(k)=ctextc
endfor
free_lun,un
ctext=ctext(6:*)
sra=strmid(ctext,30,10)
sdec=strmid(ctext,40,10)
glactc,sra*(12./180.),sdec,2000,gl,gb,1
sel=where(abs(gb) le agbhi and abs(gb) ge agblo,nsel)
if nsel eq 0 then stop,'no sample is selected'
ctext=ctext(sel)
sra=sra(sel)
sdec=sdec(sel)
if n_elements(outfile) ne 0 then begin
	openw,un,outfile,/get
	for k=0,n_elements(ctext)-1 do printf,un,ctext(k),format=form
	free_lun,un
endif
return
end