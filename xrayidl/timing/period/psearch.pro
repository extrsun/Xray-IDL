pro psearch,time,pmax,dd,pmin=pmin,plot=plot,phd=phd,prob=prob,phase=phase,pv=pv,dv=dv,pbest=pbest
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- psearch,time,pmax,dd,pmin=pmin,plot=plot,phd=phd'
print,',prob=prob,phase=phase,pv=pv,dv=dv'
return
endif
; dd = pdot/(p^2)
if n_elements(prob) eq 0 then prob=0.00135 ;3 sigma
if n_elements(pmin) eq 0 then pmin=0.01
nc=n_elements(time)
get_ksd,nc,prob,dth
print,'probth, dth = ',prob,dth
plot=1
dv=[-999]
pv=[-999]
rmt=phd/max(time)
if n_elements(phd) eq 0 then phd=0.25
p=pmax
b=0.5*(dd*time)
rtime=1./time
while p gt pmin do begin
	pht=time/p
	ph=pht - long(pht)
	ksone,ph,'func',d
	if d gt dth then begin
		pv=[pv,p]
		dv=[dv,d]
;	!debug=1
	endif
	p=p/(1.+p*rmt) ;+b)
if !debug eq 1 then stop
endwhile

if n_elements(pv) le 1 then stop,'no significant bin'
dmax=max(dv,kmax)
p=pv(kmax)
pht=time*(1./p-b)
ph=pht mod 1. ;p)/p
ksone,ph,'func',d,prob,plot=plot,/prob_get
print,p,d,prob
pv=pv(1:*)
dv=dv(1:*)
phase=ph
pbest=p
return
end
;=============================
function func,x
return,x
end
