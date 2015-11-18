pro get_ksd,nc,prob,d
;-
; get the reverse of prob_ks.pro: getting d with an assumed prob for >d.
; written by wqd, 2/96
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_ksd,nc,prob,d'
return
endif

np=1000
dv=findgen(np)/(2*np)
probv=fltarr(np)
for k=0,np-1 do begin
	prob_ks,dv(k),nc,pp
	probv(k)=pp
endfor

s=where(abs(probv-1.) lt 1.e-6 or abs(probv-6.) lt 1.e-8,ns) 
;get a monotonic vector for probv required by linterp
if ns ne 0 then remove,s,probv,dv

linterp,probv,dv,prob,d
return
end
