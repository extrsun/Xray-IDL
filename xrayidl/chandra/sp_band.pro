pro sp_band,fname,bv,cv,me
;+
; calculate broad band weight and mean everies, 
;	assuming a spectrum (input from XSPEC output)
;
; fname - input spectral model file from xspec
; bv - vectors containing band boundaries (e.g., [1,2,3] for the 1-2 keV and 
;	2-3 keV band)
;*Outputs:
; cv - weight in the bands
; me - mean energies of the bands
; 
; written by wqd, 6/6/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sp_band,fname,bv,cv,me'
return
endif

nb=n_elements(bv)-1
cv=fltarr(nb)
me=fltarr(nb)
sp_accum,fname,ee,cc,sv
linterp,ee,cc,bv,bc
bc=bc(1:nb)-bc(0:nb-1)

;calculate the mean envery
tabinv_m,ee,bv,ind
ind=nint(ind)
for k=0,nb-1 do begin
 	me(k)=total(ee(ind(k):ind(k+1))*sv(ind(k):ind(k+1))) $
	/total(sv(ind(k):ind(k+1)))
endfor
print,'bc, me = ',bc,me
return
end