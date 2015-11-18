pro get_posi,loc_old,loci,Kdup,nloci
;
; find number of photons at each photon position.
;writen  Sept 4 1992 (WQD)
;
if n_elements(loc_old) le 1 then kdup=n_elements(loc_old) else $
	kdup=histogram(loc_old)
loci=where(kdup ne 0,nloci)
kdup=kdup(loci)
loci=min(loc_old)+loci
;
return
end
