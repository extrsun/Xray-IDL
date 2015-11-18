pro list_axaf,infile,list,emin=emin,emax=emax,piorpha=piorpha,ccd=ccd,ext=ext
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- list_axaf,infile,list,emin=emin,emax=emax,piorpha=piorpha,ccd=ccd,ext=ext'
return
endif

if N_elements(ext) eq 0 then ext=1
tab=readfits(infile,hdr,ext=ext,/sil)
xev=fits_get(hdr,tab,'x')
yev=fits_get(hdr,tab,'y')
if n_elements(piorpha) eq 0 then piorpha='PI'
piev=fits_get(hdr,tab,piorpha)
tev=fits_get(hdr,tab,'TIME')
if n_elements(emin) eq 0 then emin = min(piev)
if n_elements(emax) eq 0 then emax = max(piev)
inde = where( (piev ge emin) and (piev le emax),nct )
if nct eq 0 then stop,'No counts are found'
ccdno=n_elements(ccd)
if ccdno ne 0 then begin
	ccdid=fits_get(hdr,tab,'ccd_id',inde)
	sel=[0L]
	for k=0,ccdno-1 do begin
		selid=where(ccdid eq ccd(k),selidn)
		if selidn ne 0 then sel=[sel,selid]
	endfor
	sel=sel(1:*)
	nct=N_elements(sel)
	if nct eq 0 then stop,'No counts in the selected ccd(s)'
	inde=inde(sel)
endif
xev=xev(inde)
yev=yev(inde)
piev=piev(inde)
tev=tev(inde)
row = {ascae,x:0,y:0,pi:0,time:0.0D0}
list = replicate(row,nct)
list.x = xev
list.y = yev
list.pi = piev
list.time = tev
return
end
