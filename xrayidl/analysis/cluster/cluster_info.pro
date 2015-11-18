pro cluster_info,infile,sra,sdec,cn,gl=gl,gb=gb,zz=zz,ctext=ctext
;-	
; writen by WQD, June 15, 1999
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - cluster_info,infile,sra,sdec,cn,gl=gl,gb=gb,ctext=ctext'
return
endif
openr,un,infile,/get
ctextc=''
ctext=strarr(3000)
form='(a70)'
k=0
while not eof(un) do begin
	readf,un,ctextc,format=form
	ctext(k)=ctextc
	k=k+1
endwhile
free_lun,un
ctext=ctext(4:k-1)
gl=strmid(ctext,37,6)
gb=strmid(ctext,45,6)
glactc,sra,sdec,2000,gl,gb,2
sra=sra/(12./180.)
cn=strmid(ctext,0,5)
zz=strmid(ctext,53,7)
return
end