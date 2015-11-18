pro aps_info,ra,dec,flux,id,souno,soufile=soufile,idsel=idsel $
	,magmax=magmax,magmin=magmin
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - aps_info,souno,ra,dec,flux,id,soufile=soufile,idsel=idsel,magmax=magmax'
retall
endif
;
if n_elements(magmin) eq 0 then magmin=0.
if n_elements(magmax) eq 0 then magmax=100.
	if n_elements(soufile) eq 0 then $
		fname=!data_dir+'aps_g.dat' else fname=soufile
	openr,un,fname,/get_lun
	
	n_source=10000
	souno=intarr(n_source)
	ra=dblarr(n_source)
	dec=dblarr(n_source)
	flux=fltarr(n_source)
	id=fltarr(n_source)
	k=0
	rac=''
	decc=''
	fluxc=0.
	idc=1.
	sounoc=1
	while not EOF(un) do begin
;	  if keyword_set(bmag) ne 0 then $
	   readf,un,rac,decc,fluxc,idc,sounoc,form='(a13,a13,7x,f6.2,9x,f6.2,21x,I12)' ;$
;	   readf,un,radec,fluxc,idc,sounoc,form='(a23,19x,8x,f6.2,i4,14x,i5)' $
;	  else readf,un,radec,fluxc,idc,sounoc,form='(a23,19x,f6.2,i2,24x,i5)'
;	print,radec,fluxc,idc,sounoc
	   souno(k)=sounoc
	   flux(k)=fluxc
	   id(k)=idc
	   stringad_2,rac,decc,ras,decs
	   ra(k)=ras & dec(k)=decs
	   k=k+1
	endwhile
	free_lun,un
	n_source=k
	if n_source eq 0 then begin
		print,'the file contains no source, return'
		return
	endif
	souno=souno(0:n_source-1)
	ra=ra(0:n_source-1)
	dec=dec(0:n_source-1)
	flux=flux(0:n_source-1)
	id=id(0:n_source-1)
	
	sel=where(flux le magmax and flux ge magmin,nct)
	 if nct ne 0 then begin
	        souno=souno(sel)
		ra=ra(sel)
		dec=dec(sel)
		flux=flux(sel)
		id=id(sel)
	  endif else stop,'no object with the selected id'
if !debug eq 2 then stop
return
end