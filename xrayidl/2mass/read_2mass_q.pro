pro read_2mass_q,soufile,list,noff=noff
;+
; read 2mass query table into a list 
; soufile - 2mass query file
; noff - the number of the begining text lines not be read (def = 20)
; list - putput list
;
; written by wqd, 2/23/2001
;-

if n_params() eq 0 then begin
print,'CALLING SEQUENCE - query_2mass,soufile,list,noff=noff'
return
endif
;
if n_elements(noff) eq 0 then noff=15
openr,un,soufile,/get_lun

if n_elements(n_source) eq 0 then n_source=10000
ns=strarr(n_source) 
ra=dblarr(n_source) 
dec=dblarr(n_source)
strv=strarr(n_source)

str=''
for k=0,noff-1 do readf,un,str
k=0L
while not EOF(un) do begin
   readf,un, str
   ns(k)=(strmid(str,40,16))
   ra(k)=double(strmid(str,0,12))
   dec(k)=double(strmid(str,14,12))
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

row = {id:'',ra:0d,dec:0d,text:'empty'}
list = replicate(row,n_source)
list.ra = ra
list.dec = dec
list.id = ns
list.text = strv
if !debug eq 2 then stop
return
end