pro cat_info,catfile,catno,ra,dec,zz,cntr,text
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - cat_info,catfile,catno,ra,dec,zz,cntr,text'
retall
endif
;
	openr,un,catfile,/get_lun
	n_source=10000
	catno=strarr(n_source)
	ra=strarr(n_source)
	dec=strarr(n_source)
	zz=strarr(n_source)
	cntr=strarr(n_source)
	text=strarr(n_source)
	k=0
	str=''
	str0=''
	for kk=0,7 do readf,un,str

	for k=0,n_source do begin
         readf,un, str
	 str0=gettok(str,'|')
	 if strmid(str0,0,1) ne '+' then begin
	   text(k)=str
	   catno(k)=gettok(str,'|')
	   ra(k)=gettok(str,'|')
	   dec(k)=gettok(str,'|')
	   zz(k)=gettok(str,'|')
	   cntr(k)=gettok(str,'|')
	 endif else begin
	   ns=k
           k=n_source 
	 endelse
	endfor
	free_lun,un
	if ns eq 0 then begin
		print,'the file contains no source, return'
		return
	endif
	text=text(0:ns-1)
	catno=catno(0:ns-1)
	ra=ra(0:ns-1)
	dec=dec(0:ns-1)
	zz=zz(0:ns-1)
	cntr=cntr(0:ns-1)
if !debug eq 2 then stop
return
end