pro list_asca,infile,list,emin=emin,emax=emax,piorpha=piorpha
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- list_asca,infile,list,emin=emin,emax=emax,piorpha=piorpha'
return
endif

tab=readfits(infile,hdr,ext=1,/sil)
xev=fits_get(hdr,tab,'x')
yev=fits_get(hdr,tab,'y')
if n_elements(piorpha) eq 0 then piorpha='PI'
piev=fits_get(hdr,tab,piorpha)
tev=fits_get(hdr,tab,'TIME')
      if n_elements(emin) eq 0 then emin = min(piev)
      if n_elements(emax) eq 0 then emax = max(piev)
      inde = where( (piev ge emin) and (piev le emax),nct )
      if nct eq 0 then stop,'No counts are found'
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
