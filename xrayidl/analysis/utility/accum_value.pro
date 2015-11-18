pro accum_value,loc,vv,loci,avv
;+
; cast a list of values into an image
; 
; loc - image position of values
; vv - values
; loci - positions where there are values
; avv - total values at loci
;
; written by wqd, 5/30/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - accum_value,loc,vv,loci,avv'
return
endif

slocp=sort(loc)
sloc=loc(slocp) ;sorted pixel locations of objects
svv=vv(slocp)
get_posi,sloc,loci,kdup,nloci
accum,kdup,akdup
avv=fltarr(nloci)+svv(akdup-kdup) ;first value
ss=where(kdup gt 1,nss) ; add the second and higher, if there is any
for k=0L,nss-1L do begin
 	sk=ss(k)
 	indxlo=akdup(sk)-kdup(sk)+1
 	indxhi=akdup(sk)-1
	avv(sk)=total(svv(indxlo:indxhi))
endfor
stop
return
end