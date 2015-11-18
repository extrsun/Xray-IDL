pro read_usno,soufile,list,rmlimit=rmlimit,n_source=n_source
;+
; read USNO data into a structure
; soufile - USNO data file (from the web)
; list - source structure
; rmlimit - R-band limiting mag
; n_source - total number of sources
; written by wqd, 6/15/2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - pro read_usno,soufile,list,rmlimit=rmlimit,n_source=n_source'
return
endif
;
openr,un,soufile,/get_lun

if n_elements(n_source) eq 0 then n_source=100000
ra=dblarr(n_source) 
dec=dblarr(n_source)
rm=fltarr(n_source)
bm=fltarr(n_source)
cid=strarr(n_source)

str=''
k=0L
while (not eof(un)) do begin
	readf,un,str
;	if (rmc ne 0. and bmc ne 0. and jmc ne 0. and cidc eq 1) then begin
   	cid(k)=strmid(str,4,15)
   	ra(k) =strmid(str,20,9)
   	dec(k)=strmid(str,29,10)
   	rm(k)=strmid(str,39,5)
   	bm(k)=strmid(str,44,6)
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
rm=rm(0:n_source-1)
bm=bm(0:n_source-1)
cid=cid(0:n_source-1)

row = {id:'',ra:0d,dec:0d,r:0.0,b:0.0}
list = replicate(row,n_source)
list.ra = ra
list.dec = dec
list.r = rm
list.b = bm
list.id = cid
if !debug eq 2 then stop
return
end
