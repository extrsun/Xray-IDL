pro galaxy_info,ra,dec,km,jm,hm,rf,soufile=soufile,kmlimit=kmlimit 
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - galaxy_info,ra,dec,km,jm,hm,rf,soufile=soufile,kmlimit=kmlimit '
return
endif
;
if n_elements(soufile) eq 0 then $
	fname='galaxy.dat' else fname=soufile
if n_elements(kmlimit) eq 0 then kmlimit=13.5
openr,un,fname,/get_lun

n_source=10000
ra=fltarr(n_source)
dec=fltarr(n_source)
km=fltarr(n_source)
if n_params() gt 3 then begin
	rf=fltarr(n_source)
	hm=fltarr(n_source)
	jm=fltarr(n_source)
endif
str=''
for k=0,3 do begin
	readf,un,str
endfor
k=0
while not EOF(un) do begin
   readf,un, str
   kmc=float(strmid(str,40,8))
   if kmc lt kmlimit then begin
   	km(k)=kmc
   	ra(k)=float(strmid(str,0,12))
   	dec(k)=float(strmid(str,12,12))
	if n_params() gt 3 then begin
   		jm(k)=float(strmid(str,24,8))
   		hm(k)=float(strmid(str,32,8))
   		rf(k)=float(strmid(str,70,6))
	endif
   	k=k+1
   endif
   readf,un, str
endwhile
free_lun,un
n_source=k
if n_source eq 0 then begin
	print,'the file contains no source, return'
	return
endif
ra=ra(0:n_source-1)
dec=dec(0:n_source-1)
km=km(0:n_source-1)

if n_params() gt 3 then begin
	jm=jm(0:n_source-1)
	hm=hm(0:n_source-1)
	rf=rf(0:n_source-1)
endif
if !debug eq 2 then stop
return
end