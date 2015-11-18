pro image_bflux,array_c,array_tsub,bflux,frac=frac,syserr=syserr
;+
; calculate the mean flux of the image 
; The incident flux is supposed to be uniform across the field (i.e., bright
; sources are supposed to be subtracted already
; writen by WQD, Aug 23, 1993
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - image_bflux,array_c,array_tsub,bflux,frac=frac'
print,',,syserr=syserr'
return
endif
if n_elements(frac) eq 0. then frac=1./32. ;so the bin size to be large
			;enough to contain more than 10 counts
if n_elements(syserr) eq 0 then begin
	syserr=0.1
	print,'syserr = ',syserr,' is assumed'
endif
count=array_c
time=array_tsub
c=where(array_tsub le 0.,nc)
if nc ne 0 then count(c)=0.
count=image_comp(count,frac)
time=image_comp(time,frac)
sel=where(time gt 0.3*max(time),nsel)
if nsel eq 0 then stop,'All bins have exposure equal to zero, stop'
count=float(count(sel))
time=time(sel)
bflux=sqrt(total(count*count/time)/total(time))
bcount=bflux*time
chi=total((count-bcount)*(count-bcount)/(bcount*(1.+(syserr*syserr)*bcount)))
bflux=bflux*(frac*frac)
print,'bflux, chi,ndf = ',bflux,chi,n_elements(time)
stop
end