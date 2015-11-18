pro apm_cosmos,souno,ra,dec,flux,id,soufile=soufile,outfile=outfile
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_info,souno,ra,dec,[self=,soufile=,pixel=,ns='
print,'slow=,flow=,text=]'
retall
endif
;
apm_info,para,radec,flux,fluxb,id,idb,soufile=soufile
if n_elements(outfile) eq 0 then $
	fname=!data_dir+'cosmos.dat' else fname=outfile
n_source=n_elements(radec)
openw,un,fname,/get_lun
for k=0L,n_source-1 do begin 
	printf,un,radec(k),para(k),flux(k),id(k),fluxb(k),idb(k),k $
		,form='(a24,a17,1x,f5.2,i2,1x,f6.2,i4,14x,i4)'
endfor
free_lun,un
if !debug eq 2 then stop
return
End