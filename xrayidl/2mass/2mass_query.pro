pro 2mass_query,soufile,list,nstart=nstart
;
; kmhi - def - 15.0, 14.3 would be safer, k error should be < 0.15
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - galaxy_info,ra,dec,km,jm,hm,kme,soufile=soufile,kmhi=kmhi '
return
endif
;
if n_elements(nstart) eq 0 then nstart=21
openr,un,soufile,/get_lun

if n_elements(n_source) eq 0 then n_source=10000
ns=lonarr(n_source) 
ra=dblarr(n_source) 
dec=dblarr(n_source)
strv=strarr(n_source)

str=''
for k=0,nstart-1 do readf,un,str
k=0L
while not EOF(un) do begin
   readf,un, str
   ns(k)=long(strmid(str,0,16))
   ra(k)=double(strmid(str,48,16))
   dec(k)=double(strmid(str,64,16))
   strv(k)=str
	k=k+1
endwhile
free_lun,un
n_source=k
if n_source eq 0 then begin
	print,'the file contains no source, return'
	return
endif
ns=ns(0:n_source-1)
strv=strv(0:n_source-1)
ra=ra(0:n_source-1)
dec=dec(0:n_source-1)

row = {id:0L,ra:0d,dec:0d,text:'empty'}
list = replicate(row,n_source)
list.ra = ra
list.dec = dec
list.id = ns
list.text = strv
if !debug eq 2 then stop
return
end