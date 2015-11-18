pro apm_info,ra,dec,rmag,id,fname=fname,idsel=idsel,magmax=magmax,bmag=bmag,textv=textv
	if n_elements(fname) eq 0 then $
		fname=!data_dir+'apm.dat' else fname=fname
	openr,un,fname,/get_lun
	
	n_source=10000
	textv=strarr(n_source)
	ra=fltarr(n_source)
	dec=fltarr(n_source)
	rmag=fltarr(n_source)
	id=intarr(n_source)
	bmag=fltarr(n_source)
	text=''
	k=0
	while not EOF(un) do begin
           readf,un,text,form='(a107)'
	   textv(k)=text
	   rmag(k)=float(strmid(text,30,5))
	   id(k)=fix(strmid(text,37,2))
	   bmag(k)=float(strmid(text,66,6))
	   stringad,strmid(text,3,25),ras,decs
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
	textv=textv(0:n_source-1)
	ra=ra(0:n_source-1)
	dec=dec(0:n_source-1)
	rmag=rmag(0:n_source-1)
	id=id(0:n_source-1)
 	bmag=bmag(0:n_source-1)

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
	  	c=where(rmag(sel) le magmax and rmag(sel) gt 0.,nct)
	  	if nct ne 0 then sel=sel(c) 
	 endif

	 if nct ne 0 then begin
	        textv=textv(sel)
		ra=ra(sel)
		dec=dec(sel)
		rmag=rmag(sel)
		bmag=bmag(sel)
		id=id(sel)
	  endif else stop,'no object with the selected id'
        endif
return
end