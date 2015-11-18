pro afreverse,pp,pfrac,phinit,ta,ie,af,ts,tol=tol
;+
; reverse an integrated sinusoid wave to infer the arrival time of counts
; which is used by sim_binary
; written by wqd, 7/28/2000
;-
common cfunc,frac,tini,ppn,afval,phi
if n_elements(tol) eq 0 then tol=1.e-7
ppn=pp/(2.*!pi)
frac=pfrac ; for the stupid common block, I have to do this!
phi=phinit
ns=n_elements(af)
ts=dblarr(ns)
maxdf=0.
for k=0,ns-1 do begin
	kk=ie(k)
	tini=ta(0,kk)
	afval=af(k)
	dint=ta(1,kk)-tini
	ts(k)=fx_root([tini+0.2*dint,tini+0.5*dint,ta(1,kk)+0.8*dint],'afunc',doub=1,tol=tol)
	deltaf=afunc(ts(k))
;	print,afval,ts(k),deltaf
	if afunc(ts(k)) gt maxdf then maxdf=deltaf
endfor
print,'max deviation from 0 = ',maxdf
return
end
function afunc, ts
common cfunc,frac,tini,ppn,afval,phi
	return, ts-tini+frac*ppn*(cos(tini/ppn+phi)-cos(ts/ppn+phi))-afval
end