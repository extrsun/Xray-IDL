pro make_xfitsh,refhead,outhead,xsize=xsize,ysize=ysize,cra=cra,cdec=cdec

if N_elements(xsize) ne 0 then   sxaddpar,xhead,'naxis1',xsize
if N_elements(ysize) ne 0 then   sxaddpar,xhead,'naxis2',ysize
   sxaddpar,xhead,'bscale',1.
   sxaddpar,xhead,'bzero',0.
if N_elements(cra) ne 0 then   sxaddpar,xhead,'crval1',cra
if N_elements(cdec) ne 0 then   sxaddpar,xhead,'crval2',cdec
if N_elements(binsize) ne 0 then begin
	  sxaddpar,xhead,'cdelt1',binsize
 	sxaddpar,xhead,'cdelt2',binsize
endif
   sxaddpar,xhead,'crpix1',xc
   sxaddpar,xhead,'crpix2',yc
   sxaddpar,xhead,'crota2',0.
   sxaddpar,xhead,'date',systime(0)
   sxaddpar,xhead,'equinox',2000.

sxaddpar,xhead,'telescop','IRAS - regridded'
sxaddpar,xhead,'origin','JPL-IPAC,  regrid done at CASA - Univ. of Colo.'
sxaddpar,xhead,'datamax',mx
sxaddpar,xhead,'datamin',mn
sxaddpar,xhead,'history',$
          'Regridded using software written in IDL at CASA - Univ. of Colo.'
;
writefits,outfile,outarr,xhead
;
return
end
