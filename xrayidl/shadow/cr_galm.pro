pro cr_gal,lld,bbd,noo,tt,cro,gamma=gammao,tol=tol,nha=nhao,sel=sel $
	,nch=nch,rrlimit=rrlimit
;+
; Calculate the surface brightness count rate map of a model, accounting
;	for absorption
; lld, bbd --- galactic logitude and latitude arrays
; noo, tt - electron desity (cm^-3) and temperature (in units 10^6 K)
; cro - output count rate map
; gamma - polytropic index (def = 5/3 adiabatic)
; tol - tolerance factor of the integration 
; nha - column density array (in units 10^20 cm^-2)
; sel - selected elements of the arrays for the calculation
; nch - choice for assuming different emission, absorption, and band paramters.
; rrlimit - line-of-sight integration distance (units = kpc)
;-
common para0,no,tto,lls,bbs,gamma,sigma,rrsun,ttc
common para1,temp,nh,cr,nhs
if max(abs(bbd)) gt 90. or max(abs(lld)) gt 180.  then begin
	print,'abs(bba) should be < 90 deg range'
	print,'abs(lla) should be < 180 deg'
	return
endif 

if n_elements(sel) ne 0 then begin
	lla=lld(sel)*(!pi/180.)
	bba=bbd(sel)*(!pi/180.)
endif else begin
	lla=lld*(!pi/180.)
	bba=bbd*(!pi/180.)
endelse

if n_elements(ttc) eq 0 then ttc=0.5 ; in units of 10^6 K
no=noo
tto=tt
sigma=(1.17e2)^2*tto
if n_elements(rrsun) eq 0 then rrsun=8.5
if n_elements(gammao) eq 0 then gamma=5./3. else gamma=gammao
if n_elements(tol) eq 0 then tol=1.e-3
;if n_elements(rrlimit) eq 0 then rrlimit=20. 

if n_elements(nhao) eq 0 then nha=lla*0. else begin
	if n_elements(sel) ne 0 then nha=nhao(sel)*0.01 else nha=nhao*0.01
endelse
if n_elements(nch) eq 0 then nch=0
if nch eq 0 then fname='~/rosatshell/galaxies/pspc_vr_a0.5e_45'
if nch eq 1 then fname='~/rosatshell/galaxies/pspc_vr_a1.0e_45'
if nch eq 2 then fname='~/rosatshell/galaxies/pspc_vr_a1.0e_12'
read_mcntr,temp,nh,cr,fname=fname


nbin=n_elements(lla)
cro=lla*0.

if n_elements(func) eq 0 then func='cro_sb'

step=0.5 ;kpc
rr=findgen(40)*step+0.5*step
for k=0,nbin-1 do begin
	lls=lla(k) & bbs=bba(k) &nhs=nha(k)
;	cro(k)=simpson(func,0.,rrlimit,tol=tol)
	cro(k)=total(cro_sb(rr)*step)
endfor
return
end
;================================================
function cro_sb,rr
common para0,no,tto,lls,bbs,gamma,sigma,rrsun,ttc
common para1,temp,nh,cr,nhs
rrp=rr*cos(bbs)
zz=rr*sin(bbs)
rrc=sqrt(rrsun^2+rrp^2-2.*cos(lls)*rrsun*rrp)

index=(potential(rrsun,0.)-potential(rrc,zz))/sigma
if gamma ne 1. then begin
	inside=1.+(gamma-1.)/gamma*index
	sel=where(inside gt ttc/tto,nsel)
	if nsel ne 0. then nn=no*(inside(sel))^(1./(gamma-1.)) else nn=0.
endif else nn=no*exp(index)

tt=(tto*(1./no)^(gamma-1)/11.6)*nn^(gamma-1) ; in units of keV
binterp,temp,nh,cr,tt,nhs+tt*0.,cra ;???
cro=0.208*nn^2*cra ;surface bright (cts/s arcmin^2 kpc)
if !debug eq 1 then stop
return,cro
end
;=================================
function potential,rrc,zz
;+
; Gravitational potential from Wolfire et al. (1995)
;-
vv=225.^2
ph1=-8.887*vv/sqrt(rrc^2+(6.5+sqrt(zz^2+0.26^2))^2)
rz2=zz^2+rrc^2
ph2=-3.0*vv/(0.70+sqrt(rz2))
term=sqrt(1.+(12.^2+rz2)/210.^2)
ph3=0.325*vv*alog((term-1.)/(term+1.))
return, ph1+ph2+ph3 ;-vv*alog(rrc/rrsun)
end
