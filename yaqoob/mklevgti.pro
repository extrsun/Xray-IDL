pro mklevgti,slist,tint,binsize=binsize
if n_params(0) eq 0 then begin
 print,'MKLEVGTI, slist, tint, binsize=binsize '
 print,' Make GTI files based on intensity intervals (as many as you like) of your choice'
 print,' Program takes a photon list [SLIST] and a good time intervals array [TINT(*,2)]'
 print,' produced by, for exampe, the mk1gti routine. Binsize in seconds '
 print,' Output is in the form of ascii files '
 retall
end
