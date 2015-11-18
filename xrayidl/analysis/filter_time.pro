pro filter_time,fname,timevector,indsel
;+
; determine the indexes of a timing vector which are covered by time intervals
; contained in a file (fname)
;-
print,'Open good time interval file ',fname
openr,un,fname,/get_lun
ninterval=0
readf,un,ninterval
tinterval=dblarr(2,ninterval)
readf,un,tinterval
free_lun,un
indsel=lonarr(n_elements(timevector))
kk=0
for k=0,ninterval-1 do begin
  sel=where((timevector gt tinterval(0,k)+1) and $
	(timevector lt tinterval(1,k)),nk) ;timevector is 2 s each
;  sel=where((timevector ge tinterval(0,k)) and $
;	(timevector le tinterval(1,k)),nk)
  if nk ne 0 then indsel(kk:kk+nk-1)=sel
  kk=kk+nk
endfor
indsel=indsel(0:kk-1)
indsel=indsel(sort(indsel))
end