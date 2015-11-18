pro radec_image,sra,sdec,hdr,image,loc=loc,sel=sel,filter=filter
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - radec_image,sra,sdec,hdr,image,loc=loc,sel=sel,filter=filter'
return
endif
crval=sxpar(hdr,'crval*')
cdel=sxpar(hdr,'cdelt*')
if abs(cdel(0)+cdel(1)) gt 1.e-6*cdel(1) then stop,'cdel(0) ne cdel(a)'
trans_dist,crval(0),crval(1),sra,sdec,xp,yp,/deg,pixsize=cdel(1)*3600.
naxis=sxpar(hdr,'naxis*')
crpix=sxpar(hdr,'crpix*')
xp=xp+crpix(0)
yp=yp+crpix(1)
list_image,0,0,0,image,naxis(0),naxis(1),block=1,xp=xp,yp=yp,filter=filter,sel=sel,loc=loc
return
end