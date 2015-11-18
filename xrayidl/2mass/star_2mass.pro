pro star_2mass,soufile,list,kmlo=kmlo,kmhi=kmhi,kmehi=kmehi,n_source=n_source
;
; kmhi - def - 15.0, 14.3 would be safer, k error should be < 0.15
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - galaxy_info,ra,dec,km,jm,hm,kme,soufile=soufile,kmhi=kmhi '
return
endif
;
if n_elements(kmhi) eq 0 then kmhi=14.5
;if n_elements(kmhi) eq 0 then kmlo=
if n_elements(kmehi) eq 0 then kmehi=0.15
openr,un,soufile,/get_lun

if n_elements(n_source) eq 0 then n_source=10000
ra=dblarr(n_source) 
dec=dblarr(n_source)
km=fltarr(n_source)
hm=fltarr(n_source)
jm=fltarr(n_source)
kme=fltarr(n_source)
hme=fltarr(n_source)
jme=fltarr(n_source)

str=''
for k=0,1 do begin
	readf,un,str
endfor
k=0L
while not EOF(un) do begin
   readf,un, str
   kmc=float(strmid(str,62,7))
   kmec=float(strmid(str,71,7))
   if (kmc lt kmhi and kmec lt kmehi) then begin
   	km(k)=kmc
   	ra(k)=double(strmid(str,1,11))
   	dec(k)=double(strmid(str,13,11))
   	jm(k)=float(strmid(str,26,7))
   	hm(k)=float(strmid(str,35,7))
   	jme(k)=float(strmid(str,44,7))
   	hme(k)=float(strmid(str,53,7))
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
hm=hm(0:n_source-1)
jm=jm(0:n_source-1)
kme=kme(0:n_source-1)
hme=hme(0:n_source-1)
jme=jme(0:n_source-1)

row = {xray,ra:0d,dec:0d,j:0.0,je:0.0,h:0.0,he:0.0,k:0.0,ke:0.0}
list = replicate(row,n_source)
list.ra = ra
list.dec = dec
list.j = jm
list.je = hme
list.h = hm
list.he = jme
list.k = km
list.ke = kme
if !debug eq 2 then stop
return
end