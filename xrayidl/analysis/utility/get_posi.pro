pro get_posi,loc_old,loci,Kdup,nloci
;+
; find number of photons at each image bin.
; loc_old - photon location vector
; loci - image bin location
; kdup - number of photons
; nloci - number of bins with at least one photon
;
; writen  Sept 4 1992 (WQD)
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_posi,loc_old,loci,Kdup,nloci'
return
endif
;
if n_elements(loc_old) le 1 then kdup=n_elements(loc_old) else $
	kdup=histogram(loc_old)
loci=where(kdup ne 0,nloci)
kdup=kdup(loci)
loci=min(loc_old)+loci
;
return
end
