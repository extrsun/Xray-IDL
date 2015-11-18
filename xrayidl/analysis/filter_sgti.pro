pro filter_sgti,timevector,indsel
;+
; determine the indexes of a timing vector which are covered by time intervals
; contained in a file (fname)
;-
indsel=lindgen(n_elements(timevector))
if !proc eq 'MPE' then 	return  ;no good time intervals have been found yet
extyp='.fits'
fname=!data_dir+!seq_no+extyp
	tab1=readfits(fname,h1,ext=1)
	start=tbget(h1,tab1,1) ; start of SGTI
	tail=tbget(h1,tab1,2)   ; tail of SGTI
	n_interval=n_elements(start)
	print,'number of time intervals is ',n_interval
;
kk=0
for k=0,n_interval-1 do begin
  sel=where((timevector ge start(k)) and $
	(timevector le tail(k)),nk)
  if nk ne 0 then indsel(kk:kk+nk-1)=sel
  kk=kk+nk
endfor
indsel=indsel(0:kk-1)
indsel=indsel(sort(indsel))
end