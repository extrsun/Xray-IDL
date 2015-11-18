;pro apm_info,para,radec,flux,fluxb,id,idb,soufile=soufile
pro apm_info,ra,dec,flux,id,idb,soufile=soufile,idsel=idsel $
		,magmax=magmax,fluxb=fluxb
	if n_elements(soufile) eq 0 then $
		fname=!data_dir+'apm.dat' else fname=soufile
	openr,un,fname,/get_lun
	
	n_source=10000
	para=strarr(n_source)
	ra=fltarr(n_source)
	dec=fltarr(n_source)
	flux=fltarr(n_source)
	id=intarr(n_source)
	fluxb=fltarr(n_source)
	idb=intarr(n_source)
	k=0
	radecc=''
	fluxc=0.
	idc=1
	fluxbc=0.
	idbc=1
	parac=''
;	for n=0,2 do readf,un,parac
	while not EOF(un) do begin
           readf,un,radecc,fluxc,idc,parac,fluxbc,idbc,form='(a29,2x,f5.2,i4,2x,a17,8x,f6.2,i4)'
;	print,radec,fluxc,idc,parac
	   para(k)=parac
	   flux(k)=fluxc
	   id(k)=idc
	   fluxb(k)=fluxbc
	   idb(k)=idbc
	   stringad,radecc,ras,decs
	   ra(k)=ras
	   dec(k)=decs
	   k=k+1
	endwhile
	free_lun,un
	n_source=k
	if n_source eq 0 then begin
		print,'the file contains no source, return'
		return
	endif
	para=para(0:n_source-1)
	ra=ra(0:n_source-1)
	dec=dec(0:n_source-1)
	flux=flux(0:n_source-1)
	id=id(0:n_source-1)
 	fluxb=fluxb(0:n_source-1)
	idb=idb(0:n_source-1)

	nidsel=n_elements(idsel)
	nmagmax=n_elements(magmax)
	if nidsel+nmagmax ne 0 then begin

         if nidsel ne 0 then begin
	  sel=[-999]
	  for k=0,nidsel-1 do begin
		c=where(id eq idsel(k),nc)
		if nc ne 0 then sel=[sel,c]
	  endfor
	  sel=sel(1:*)
	  nct=n_elements(sel)
         endif else sel=lindgen(n_elements(id))

	 if nmagmax ne 0 and nct ne 0 then begin
	  	c=where(flux(sel) le magmax and flux(sel) gt 0.,nct)
	  	if nct ne 0 then sel=sel(c) 
	 endif

	 if nct ne 0 then begin
	        para=para(sel)
		ra=ra(sel)
		dec=dec(sel)
		flux=flux(sel)
		fluxb=fluxb(sel)
		id=id(sel)
		idb=idb(sel)
	  endif else stop,'no object with the selected id'
        endif
return
end