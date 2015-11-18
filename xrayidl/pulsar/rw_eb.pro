pro rw_eb,lx,edot,rs,eb,rw,alpha=alpha,ns=ns
;
; Calculate the relation between Pulsar Wind Lorentz Factor (rw) in units of
; 10^6,
; used in the modeling of N157B
; alpha - energy index of observed spectrum
; see syn_lum.pro for other parameters.
; written by wqd Oct. 24, 2000
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- rw_eb,lx,edot,rs,eb,rw,alpha=alpha,ns=ns'
return
endif
if n_elements(alpha) eq 0 then alpha=1.5
if n_elements(gamma) eq 0 then gamma=3
if n_elements(emina) eq 0 then emin=0.2
if n_elements(emax) eq 0 then emax=4.
if n_elements(eb) eq 0 then eb=0.5
pp=2*alpha+1
if n_elements(vp) eq 0 then vp=1./3.
fac=2.68e-4
rw=16.5*(pp-2.)^(pp-1)/(pp-1.)^(pp-2)*(1.-eb)^(pp-1)*ns $
*(eb*fac/rs^2*edot)^((pp+1)*0.25)
npp=n_elements(pp)
for k=0,npp-1 do begin
 if pp(k) eq 3 then rw(k)=(rw(k)*edot* $
	alog(emax/emin)/lx)^(1./(2-pp(k))) $
 else rw(k)=(rw(k)*edot*2./(3.-pp(k))*(emax^((3.-pp(k))*0.5) $
		-emin^((3.-pp(k))*0.5))/lx)^(1./(2-pp(k)))
endfor

return
end
