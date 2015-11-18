pro ppp_info,hdr,soufile,souno,ra,dec,flux,fluxe,id,idsel=idsel,ipix=ipix,jpix=jpix,magmax=magmax,magmin=magmin
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - ppp_info,hdr,soufile,souno,ra,dec,flux,fluxe,id,idsel=idsel,ipix=ipix,jpix=jpix,magmax=magmax,magmin=magmin'
return
endif
;
	openr,un,soufile,/get_lun
	
	n_source=3000
	souno=strarr(n_source)
	ra=dblarr(n_source)
	dec=dblarr(n_source)
	ra=fltarr(n_source)
	ipix=fltarr(n_source)
	jpix=fltarr(n_source)
	fluxe=fltarr(n_source)
	flux=fltarr(n_source)
	id=intarr(n_source)
	ipixc=0.d
	jpixc=0.d
	fluxec=0.
	fluxc=0.
	idc=1
	sounoc=''

	crval=sxpar(hdr,'crval*')
	crpix=sxpar(hdr,'crpix*')
	cdelt=sxpar(hdr,'cdelt*')
	xsz=sxpar(hdr,'naxis1')
	ysz=sxpar(hdr,'naxis2')

	for k=1,7 do readf,un,sounoc
	k=0
	while not EOF(un) do begin
	readf,un,sounoc,ipixc,jpixc,fluxc,fluxec,idc $
		,form='(a5,2d8.2,18x,f6.2,f5.2,i3)'
 	   souno(k)=sounoc
	   flux(k)=fluxc
	   id(k)=idc
	   fluxe(k)=fluxec
	   ipix(k)=ipixc & jpix(k)=jpixc
	   k=k+1
	endwhile
	free_lun,un
	n_source=k
	if n_source eq 0 then begin
		print,'the file contains no source, return'
		return
	endif
	souno=souno(0:n_source-1)
	ipix=ipix(0:n_source-1)
	jpix=jpix(0:n_source-1)
	xp=ipix-(0.5+crpix(0))
	yp=jpix-(crpix(1)+0.5)
	fluxe=fluxe(0:n_source-1)
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
	  if n_elements(magmin) ne 0 then begin
	  	c=where(flux(sel) ge magmin,nct)
	  	sel=sel(c)
	  endif	
	  if nct ne 0 then begin
		xp=xp(sel)
		yp=yp(sel)
		ipix=ipix(sel)
		jpix=jpix(sel)
		flux=flux(sel)
		fluxe=fluxe(sel)
		id=id(sel)
	  endif else stop,'no object with the selected id'
        endif
	if abs(cdelt(0)) ne cdelt(1) then print,'cdeltx ne cdelty'
	trans_loct,xp,yp,crval(0),crval(1),ra,dec,/deg,pixsize=cdelt(1)*3600.

if !debug eq 2 then stop
return
end