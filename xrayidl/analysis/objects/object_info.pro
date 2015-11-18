pro object_info,souno,ra,dec,flux,id,soufile=soufile,idsel=idsel $
	,magmax=magmax,bmag=bmag
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - aps_info,souno,ra,dec,flux,id,soufile=soufile,idsel=idsel,magmax=magmax'
retall
endif
;
	if n_elements(soufile) eq 0 then $
		fname=!data_dir+'cosmos.dat' else fname=soufile
	openr,un,fname,/get_lun
	
	n_source=10000
	souno=intarr(n_source)
	ra=dblarr(n_source)
	dec=dblarr(n_source)
	flux=fltarr(n_source)
	id=intarr(n_source)
	k=0L
	radec=''
	ras=''
	decs=''
	fluxc=0.
	idc=1
	sounoc=1
	while not EOF(un) do begin
;	  if keyword_set(bmag) ne 0 then $
	   readf,un,radec,fluxc,idc,sounoc $
;		,form='(a24,18x,f6.2,i2,24x,i5)' 
		,form='(a28,38x,f6.2,i4,f7.2)' 
	 ;,form='(2a12,18x,f6.2,i2,1x,f6.2,2x,i2,x13,i5)' ;$
;	   readf,un,ras,decs,flux,souno,form='(a13,a13,7x,f6.2,36x,I12)' ;$
;	   readf,un,radec,fluxc,idc,sounoc,form='(a23,19x,8x,f6.2,i4,14x,i5)' $
;	  else readf,un,radec,fluxc,idc,sounoc,form='(a23,19x,f6.2,i2,24x,i5)'
;	print,radec,fluxc,idc,sounoc
;	if fluxc lt bmag then begin
	   souno(k)=sounoc
	   flux(k)=fluxc
	   id(k)=idc
;	   stringad_2,ras,decs,rad,decd
	   stringad,radec,rad,decd
	   ra(k)=rad & dec(k)=decd
	   k=k+1
;	endif
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
	        souno=souno(sel)
		ra=ra(sel)
		dec=dec(sel)
		flux=flux(sel)
		id=id(sel)
	  endif else stop,'no object with the selected id'
        endif
return
end