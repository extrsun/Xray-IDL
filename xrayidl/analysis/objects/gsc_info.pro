pro gsc_info,soufile,souno,ra,dec,perr,flux,id,plate,idsel=idsel,magmax=magmax
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - gsc_info,soufile,souno,ra,dec,perr,flux,id,plate'
print,',idsel=idsel,magmax=magmax'
retall
endif
;

	openr,un,soufile,/get_lun
	
	n_source=4000
	souno=strarr(n_source)
	plate=strarr(n_source)
	ra=dblarr(n_source)
	dec=dblarr(n_source)
	flux=fltarr(n_source)
	perr=fltarr(n_source)
	id=intarr(n_source)
	k=0
	rac=0.d
	decc=0.d
	perrc=0.
	fluxc=0.
	idc=1
	sounoc=''
	platec=''
	readf,un,ns
;	while not EOF(un) do begin
	for k=0,ns-1 do begin
           readf,un,sounoc,rac,decc,perrc,fluxc,idc,platec,form='(8x,a6,2d10.5,f8.1,f6.2,13x,i4,8x,a4)'
;	print,rac,decc,fluxc,idc,sounoc
 	   souno(k)=sounoc
	   plate(k)=platec
	   flux(k)=fluxc
	   id(k)=idc
	   perr(k)=perrc
	   ra(k)=rac & dec(k)=decc
;	   k=k+1
;	endwhile
	endfor
	free_lun,un
	n_source=k
	if n_source eq 0 then begin
		print,'the file contains no source, return'
		return
	endif
	souno=souno(0:n_source-1)
	plate=plate(0:n_source-1)
	ra=ra(0:n_source-1)
	dec=dec(0:n_source-1)
	perr=perr(0:n_source-1)
	flux=flux(0:n_source-1)
	id=id(0:n_source-1)

	nsel=n_elements(idsel)
	if nsel ne 0 then begin

	  sel=[-999]
	  for k=0,nsel-1 do begin
		c=where(id eq idsel(k),nc)
		if nc ne 0 then sel=[sel,c]
	  endfor
	  sel=sel(1:*)
	  nct=n_elements(sel)
	  if n_elements(magmax) ne 0 then begin
	  	c=where(flux(sel) le magmax,nct)
	  	sel=sel(c)
	  endif
		
	  if nct ne 0 then begin
	        souno=souno(sel)
		plate=plate(sel)
		ra=ra(sel)
		dec=dec(sel)
		flux=flux(sel)
		perr=perr(sel)
		id=id(sel)
	  endif else stop,'no object with the selected id'
        endif
if !debug eq 2 then stop
return
end