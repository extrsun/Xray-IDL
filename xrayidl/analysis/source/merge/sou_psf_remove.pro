pro sou_psf_remove,fra,fdec,struct_s,slist,rlo=rlo,rhi=rhi,perclimit=perc $
		,psffile=psffile,cntrth=cntrlimit,factor=factor
;+
; Combine sources from a list of observations into a single file
;
;*INPUTS:
; fra, fdec - RA and Dec of the telescope axis
; struct_s - source structure to be examined
; slist - structure of sources whose PSF effect is to be considered
; rlo, rhi - the lower and upper limit of the PSF effect radius 
;		def = 6, 15*60 (arcsec)
; factor - the factor of the source radius determined by psf_params	
; 		def=0.8
;*OUTPUTS:
; struct_s -  source structure with PSF affected entries removed.
;
;*Example: 
; sou_psf_remove,cra,cdec,ss,s
;
; written by wqd 2/3/2002
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - sou_psf_remove,fra,fdec,struct_s,slist,rlo=rlo,rhi=rhi'
print,',perclimit=perc,psffile=psffile,cntrth=cntrlimit,factor=factor'
return
endif
if n_elements(rlo) eq 0 then rlo=6. ;(arcsec)
if n_elements(rhi) eq 0 then rhi=60*15
if n_elements(factor) eq 0 then factor=0.8
trans_dist,fra,fdec,slist.ra,slist.dec,cxp,cyp,/das,/deg
dis=sqrt(cxp^2+cyp^2) ;off-axis angle in units of arcmin
sel=where(dis lt rhi,nsel) 
if nsel ne 0 then begin
 psf_params,dis(sel)/60.,sradius,slist(sel).cntr,perclimit=perc $
	,psffile=psffile,cntrth=cntrlimit
 if n_elements(factor) ne 0 then sradius=sradius*factor*!size_pixel
 for n=0,nsel-1 do begin
	kk=sel(n)
 	trans_dist,struct_s.ra,struct_s.dec $
		,slist(kk).ra,slist(kk).dec,cxp,cyp,/das,/deg
	sep=sqrt(cxp^2+cyp^2)
 	ss=where(sep lt sradius(n) and sep gt rlo,nss) 
	if nss ne 0 then remove,ss,struct_s
  endfor
endif
return
end