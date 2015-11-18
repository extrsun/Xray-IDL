pro cr_egal,rro,noo,tt,cro,gamma=gammao,tol=tol,nha=nhao,sel=sel $
,nch=nch,ttth=ttth,blow=blow,bhigh=bhigh,fname=fname,step=step,nflux=nflux,fluxv=fluxv
;+
; Calculate the count rate map of an external face-on galaxy, accounting for 
;	absorption; the integration is from z=0 to infinite.
; rro - vector of galactic radii
; noo - the hot gas electron density at the corresponding Sun radius
; cro - output count rate map (in units of counts/s arcmin^2!!!)
; gamma - polytropic index (def = 5/3 adiabatic)
; tol - tolerance factor of the integration (not used)
; nha - column density array (in units 10^20 cm^-2)
; sel - selected elements of the arrays for the calculation
; nch - choice for assuming different emission, absorption, and band paramters.
; 	nch=0 ---  pspc_vr_a0.5e_
; 	nch=1 ---  pspc_vr_a1.0e_
; ttth - thredhold temperature above wich  the emission to be calculated.
;	(def = 5 10^5 K).
; nflux - choice for energy bands
;	nflux = 0 -- bolumetric 
;	nflux = 1 -- 0.1 - 2 keV
;	nflux = 2 -- 0.5 - 2 keV
; fluxv - energy flux map
;-
common para3,no,tto,rrc,gamma,sigma,rrsun,ttc
common para1,temp,nh,cr,nhs
common fluxb,tempv,fluxa,flux

if n_elements(ttth) eq 0 then ttc=0.5 else ttc=ttth; in units of 10^6 K
no=noo
tto=tt
sigma=(1.17e2)^2*tto
if n_elements(rrsun) eq 0 then rrsun=8.5
if n_elements(gammao) eq 0 then gamma=5./3. else gamma=gammao
;if n_elements(tol) eq 0 then tol=1.e-3
if n_elements(sel) ne 0 then rr=rro(sel) else rr=rro
nha=rr*0.
if n_elements(nhao) ne 0 then begin
	if n_elements(sel) ne 0 then nha=nhao(sel)*0.01 else nha=nha+nhao*0.01
endif
if n_elements(nch) eq 0 then nch=0
if nch eq 0 then fhead='~/rosatshell/galaxies/pspc_vr_a0.5e_'
if nch eq 1 then fhead='~/rosatshell/galaxies/pspc_vr_a1.0e_'
if n_elements(blow) eq 0 then blow=4
if n_elements(bhigh) eq 0 then bhigh=5
read_mcntr,temp,nh,cr,fhead,blow,bhigh,fname=fname
;print,'fname = ',fname
;=================
if n_elements(nflux) ne 0 then begin 
	openr,unin,'~/rosatshell/galaxies/flux_ra1.0.dat',/get_lun
	readf,unin,nl,nv
	tempv=fltarr(nl)
	readf,unin,tempv
	fluxa=findgen(1+nl,nv)
	readf,unin,fluxa
	free_lun,unin
	case nflux of
		0: fluxa=fluxa(1:*,0) ;the first column contains the band info
		1: fluxa=fluxa(1:*,1)
		2: fluxa=fluxa(1:*,2)
		else: stop,'nflux is not allowed'
	endcase
endif 
;--------------------
nbin=n_elements(rr)
cro=rr*0.

;if n_elements(func) eq 0 then func='cro_sb'

; find the indexes between which nha are located in the absorption table
tabinv,nh,nha,indlo
indlo=fix(indlo)
indhi=(indlo+1) < (n_elements(nh)-1)

if n_elements(step) eq 0 then step=0.5 ;kpc
zzo=findgen(40)*step+0.5*step
if n_elements(nflux) ne 0 then fluxv=cro
for k=0,nbin-1 do begin
	rrc=rr(k) &nhs=nha(k)
;	cro(k)=simpson(func,0.,rrlimit,tol=tol)
	zz=zzo ;zz may be changed in get_nt
	cro(k)=total(cro_esb(zz,indlo=indlo(k),indhi=indhi(k)))*step
	if n_elements(nflux) ne 0 then fluxv(k)=total(flux)*step
endfor
return
end
;================================================
function cro_esb,zz,indlo=indlo,indhi=indhi
common para3,no,tto,rrc,gamma,sigma,rrsun,ttc
common para1,temp,nh,cr,nhs
common fluxb,tempv,fluxa,flux
get_ent,zz,nn,tt
if n_elements(indlo) ne 0 then $
	binterp,temp,nh(indlo:indhi),cr(*,indlo:indhi),tt,nhs+tt*0.,cra $
		else binterp,temp,nh,cr,tt,nhs+tt*0.,cra 
cro=0.208*nn^2*cra ;surface bright (cts/s arcmin^2 kpc)
if n_elements(fluxa) ne 0 then begin
	linterp,tempv,fluxa,tt,flux 
	flux=0.208*nn^2*flux ;surface bright (cts/s arcmin^2 kpc)
endif
if !debug eq 1 then stop
return,cro
end
;===============================
pro get_ent,zz,nn,tt,zeroset=zeroset
;+
; zeroset - if set, the size of the array nn and tt will not be reduced;
;	negative nn is set to be zero
;-
common para3,no,tto,rrc,gamma,sigma,rrsun,ttc
index=(potential(rrsun,0.)-potential(rrc,zz))/sigma
if gamma ne 1. then begin
	inside=1.+(gamma-1.)/gamma*index
	sel=where(inside gt ttc/tto,nsel)
	if nsel ne 0. then begin
		if keyword_set(zeroset) ne 0 then begin
		 nn=inside*0.
		 nn(sel)=no*(inside(sel))^(1./(gamma-1.)) 
		endif else nn=no*(inside(sel))^(1./(gamma-1.)) 
	endif else nn=0.
endif else nn=no*exp(index)
tt=(tto*(1./no)^(gamma-1)/11.6)*nn^(gamma-1) ; in units of keV
return
end