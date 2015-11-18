pro vmerge,value,vindex,mvalue,kdup
;+
; merge values at same index locatioins
;
;*INPUTS: 
; value - value vector
; vindex - index locations of the value vector (array)
;
;*OUTPUTS:
; avaule - merged (totaled) value vector
; kdup - duplication at the vactor 
;
; The mean values at unique index locations are then equal to mvalue/kdup.
; 
;*Examples:
; a=[1,4,4,3,2,2]
; b=[2,3,3,6,4,5]
; vaccum,a,b,mvalue
; print,mvalue
;          2           6           6           9
;
; written by wqd, 7/24/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - vmerge,value,vindex,mvalue,kdup'
return
endif

get_posi,vindex,loci,kdup
accum,kdup,aloc ;get accumulated locations
aloc=[0,aloc]

; define the value vector at the unique locations:
mvalue=value(aloc(0:kdup-2))

sel=where(kdup gt 1,nsel)
if nsel ne 0 then begin
	alocsel=aloc(sel)+1
	alocselhi=aloc(sel+1)-1
	for k=0,nsel-1 do $
		mvalue(sel(k))=total(value(alocsel(k):alocselhi(k)))
endif
print,mvalue
stop
return
end
