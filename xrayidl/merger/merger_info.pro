pro merger_info,infile,sra,sdec,dl,gl=gl,gb=gb,gblimit=gblimit,ctext=ctext
;-
; writen by WQD, June 15, 1999
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - merger_info,infile,sra,sdec,cn,gl=gl,gb=gb,ctext=ctext'
return
endif
openr,un,infile,/get
ctextc=''
ctext=strarr(1000)
form='(a70)'
k=0
while not eof(un) do begin
	readf,un,ctextc,format=form
	ctext(k)=ctextc
	k=k+1
endwhile
free_lun,un
ctext=ctext(2:k-1)
trans_radian,strmid(ctext,0,3),strmid(ctext,3,2),strmid(ctext,5,1)*6.,strmid(ctext,6,3),strmid(ctext,9,2),0.,sra,sdec,/deg
precess,sra,sdec,1950,2000
;trans_degree,sra(30),sdec(30),/pri,/deg
glactc,sra*(12./180.),sdec,2000,gl,gb,1
dl=strmid(ctext,11,6)
;zz=strmid(ctext,53,7)
if N_elements(gblimit) eq 0 then gblimit=60.
sel=where((abs(gb) gt gblimit and dl lt 200),nsel)
if nsel ne 0 then begin
	for k=0,nsel-1 do begin
		print,gl(sel(k)),gb(sel(k)),ctext(sel(k))
		trans_degree,sra(sel(k)),sdec(sel(k)),/pri,/deg
	endfor
endif else print,'No source!'

stop
return
end
