pro dist_cosmic,zz,dl,da,ho=ho,qo=qo,lnorm=lnorm
;-
; calculating cosmological distances as a funtion of redshift
; zz - redshift
; dl, da - luminosity and angular distance
; ho, qo - Hubble constant and Qo
;+
if N_params() eq 0 then begin
	print,'Call Seq = dist_cosmic,zz,dl,da,ho=ho,qo=qo'
	print,'dl, da in unites of Mpc'
return
endif
if n_elements(ho) eq 0 then ho=75.
if n_elements(qo) eq 0 then qo=0.5
dl=2.998e5/(qo^2*ho)*(qo*zz+(qo-1)*(sqrt(1.+2*qo*zz)-1))
da=dl/(1.+zz)^2
print,'dl,da= ',dl,da
print,'ho,qo = ',ho,qo
if keyword_set(lnorm) ne 0 then $
	print,'4pidl**2 = ',4*!pi*(dl*0.3086)^2, 'x E50'
return
end