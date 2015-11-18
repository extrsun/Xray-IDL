pro read_2mass,soufile,list,kmlimit=kmlimit 
;
; kmlimit - def - 15.0, 14.3 would be safer, k error should be < 0.15
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - galaxy_info,ra,dec,km,jm,hm,kme,soufile=soufile,kmlimit=kmlimit '
return
endif
;
openr,un,soufile,/get_lun

if n_elements(n_source) eq 0 then n_source=1000000
ra=dblarr(n_source) 
dec=dblarr(n_source)
km=fltarr(n_source)
hm=fltarr(n_source)
jm=fltarr(n_source)
cid=intarr(n_source)

str=''
k=0L
rac=0.0d
decc=0.0d
jmc=0.
hmc=0.
kmc=0.
cidc=1
while (not eof(un)) do begin
	readf,un,rac,decc,jmc,hmc,kmc,cidc
;	if (kmc ne 0. and hmc ne 0. and jmc ne 0. and cidc eq 1) then begin
;	if (kmc ne 0. and hmc ne 0. and jmc ne 0.) then begin
;	if (kmc ne 0. and hmc ne 0. and jmc ne 0.) then begin
   	km(k)=kmc
   	ra(k)=rac
   	dec(k)=decc
   	jm(k)=jmc
   	hm(k)=hmc
   	cid(k)=cidc
   	k=k+1
;   endif
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
cid=cid(0:n_source-1)

row = {rcra,ra:0d,dec:0d,j:0.0,h:0.0,k:0.0,id:0}
list = replicate(row,n_source)
list.ra = ra
list.dec = dec
list.j = jm
list.h = hm
list.k = km
list.id = cid
if !debug eq 2 then stop
return
end
