pro modhdr,hdrref,hdrnew,fac=fac,dimx=dimx,dimy=dimy,dra=dra,ddec=ddec $
,fname=fname,bitpix=bitpix,hist=hist
;
; modify a fits header 
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- modhdr,hdrref,hdrnew,fac=fac,dimx=dimx,dimy=dimy'
print,',dra=dra,ddec=ddec,fname=fname,bitpix=bitpix,hist=hist'
return
endif
;
;
hdrnew=hdrref
; change increment of the pixel
if n_elements(fac) ne 0 then begin
	cdelt=sxpar(hdrref,'cdelt*')
	cdelt=cdelt*fac
	sxaddpar,hdrnew,'cdelt1',cdelt(0)
	sxaddpar,hdrnew,'cdelt2',cdelt(1)
endif
; change the dimension of the image
if n_elements(dimy) eq 0 then dimy=dimx
if n_elements(dimx) ne 0 then begin
	sxaddpar,hdrnew,'naxis1',dimx
	sxaddpar,hdrnew,'naxis2',dimy
	; change the pixel reference center
	crpix=[(dimx+1.)/2.,(dimy+1.)/2.]
	sxaddpar,hdrnew,'crpix1',crpix(0)
	sxaddpar,hdrnew,'crpix2',crpix(1)
endif
;
if n_elements(dra) ne 0 then sxaddpar,hdrnew,'crval1',dra
if n_elements(ddec) ne 0 then sxaddpar,hdrnew,'crval2',ddec
if n_elements(fname) ne 0 then sxaddpar,hdrnew,'irafname',fname
if n_elements(bitpix) ne 0 then sxaddpar,hdrnew,'bitpix',bitpix
if n_elements(hist) ne 0 then sxaddpar,hdrnew,'history',hist
end
