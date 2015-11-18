pro petrosian,rco,betao,rp,ratioo=ratioo
;+
; calculate the Petrosian radius of the beta model profile
; rco, betao - vector containing the core radius and the beta value of 
;	the beta model
; rp - output vector containing the petrosian raidus
; ratio - the ratio of the mean surface brightness to the surface brightness 
;	at the petrosian radius
; wqd, dec 1996
;-
if n_params() lt 1 then begin
 print,'CALLING SEQUENCE - petrosian,rco,betao,rp,ratioo=ratioo'
return
endif 
common para,ratio,beta,rc
if n_elements(ratioo) eq 0 then ratio=2. else ratio=ratioo
nbin=n_elements(rco)
if n_elements(rp) eq 0 then rp=rco+fltarr(nbin)

func='betamodel'
for n=0,nbin-1 do begin
	rc=rco(n)
	beta=betao(n)
	rp(n)=broyden(rp(n),func)
endfor
return
end
;==============================
function betamodel,rp
common para,ratio,beta,rc
alpha=-3.*beta+0.5
xx=(rp/rc)^2
inside=1.+xx
if alpha eq -1. then return,alog(inside)*inside/xx-ratio else $
return,(1.-inside^(alpha+1))/((-1.-alpha)*xx*inside^alpha)-ratio
end