pro vmerge,ovalue,ovindex,mvalue,mindex,mdup
;+
; merge values at same index locatioins
;
;*INPUTS: 
; ovalue - value vector
; ovindex - index locations of the value vector (array)
;		if pre-sorted, set the keyword psort to avoid further sorting
;
;*OUTPUTS:
; mvaule - merged (totaled) value vector
; mindex - index locations of the merged vector (the sequence may be different
;		from that of ovindex if it is not pre-sorted.
; mdup - duplication at the index of the merged vector 
;
; The mean values at unique index locations are then equal to mvalue/mdup.
; 
;*Examples:
; a=[1,4,4,3,2,2]
; b=[2,3,3,6,4,5]
; vmerge,b,a,mvalue
; print,mvalue
;          2           6           6           9
;
; written by wqd, 7/24/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - vmerge,ovalue,ovindex,mvalue,mdup'
return
endif
if keyword_set(psort) then begin
	vindex=ovindex
	value=ovalue
endif else begin
	ss=sort(ovindex)
	vindex=ovindex(ss)
	value=ovalue(ss)
endelse	
get_posi,vindex,mindex,mdup,mbin
accum,mdup,aloc ;get accumulated locations
aloc=[0,aloc]

; define the value vector at the unique locations:
mvalue=value(aloc(0:mbin-1))

sel=where(mdup gt 1,nsel)
if nsel ne 0 then begin
	alocsel=aloc(sel)+1
	alocselhi=aloc(sel+1)-1
	for k=0L,nsel-1L do $
	 mvalue(sel(k))=mvalue(sel(k))+total(value(alocsel(k):alocselhi(k)))
endif
return
end
