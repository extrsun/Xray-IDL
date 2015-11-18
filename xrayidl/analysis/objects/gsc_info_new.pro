pro gsc_info_new,infile,ra,dec,souno,text
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - gsc_info_new,infile,ra,dec,souno,text'
return
endif
;

	openr,un,infile,/get_lun
	readf,un,ns
	souno=strarr(ns)
	ra=dblarr(ns)
	dec=dblarr(ns)
	text=strarr(ns)
	stext=''
	for k=0,ns-1 do begin
           readf,un,stext,form='(a80)'
 	   souno(k)=strmid(stext,8,8)
	   radec=strmid(stext,16,24)
	   stringad,radec,rac,decc
	   ra(k)=rac & dec(k)=decc
	   text(k)=stext
	endfor
	free_lun,un
return
end