;========================================
pro cr_gal_mult,lld,bbd,noo,ttlo,tthi,ttn,crov,gamma=gammao,nha=nhao,sel=sel $
	,nch=nch,ttv=ttv,blow=blow,bhigh=bhigh,fname=fname
;+
; main program for calculating the surface intensity of the corona, 
;	account for absorption
; written by wqd, April 1997
;-
if n_params() eq 0 then begin
 print,'CALLING SEQUENCE -'
 print,'cr_gal_mult,lld,bbd,noo,ttlo,tthi,ttn,crov,gamma=gammao,nha=nhao'
 print,',sel=sel,nch=nch,ttv=ttv,blow=blow,bhigh=bhigh,fname=fname'
 return
endif

if n_elements(sel) eq 0 then begin
	nbin=n_elements(lld)
	sel=lindgen(nbin)
endif else nbin=n_elements(sel)
if ttn eq 1 then ttv=ttlo else ttv=ttlo+(tthi-ttlo)/(ttn-1.)*findgen(ttn)

crov=fltarr(nbin,ttn)

for k=0,ttn-1 do begin
	cr_gal,lld,bbd,noo,ttv(k),cro,gamma=gammao,nha=nhao,sel=sel,nch=nch, $
		blow=blow,bhigh=bhigh
	crov(*,k)=cro
endfor
crov=crov*1.e4 ;in units of 10^4 counts/s arcmin^2
return
end
;========================================
pro cr_gal,lld,bbd,noo,tt,cro,gamma=gammao,tol=tol,nha=nhao,sel=sel $
	,nch=nch,ttth=ttth,blow=blow,bhigh=bhigh,fname=fname
;+
; Calculate the surface brightness count rate map of a model, accounting
;	for the absorption
; lld, bbd --- galactic logitude and latitude arrays
; noo, tt - electron desity (cm^-3) and temperature (in units 10^6 K)
; cro - output count rate map
; gamma - polytropic index (def = 5/3 adiabatic)
; tol - tolerance factor of the integration (not used)
; nha - column density array (in units 10^20 cm^-2)
; sel - selected elements of the arrays for the calculation
; nch - choice for assuming different emission, absorption, and band paramters.
; 	nch=0 ---  pspc_vr_a0.5e_
; 	nch=1 ---  pspc_vr_a1.0e_
; ttth - thredhold temperature above wich  the emission to be calculated.
;	(def = 5 x 10^5 K).
;*NOTE: 
;	lld, bbd, and nha must have the same size.
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

if n_elements(ttth) eq 0 then ttc=0.5 else ttc=ttth; in units of 10^6 K
no=noo
tto=tt
sigma=(1.17e2)^2*tto
if n_elements(rrsun) eq 0 then rrsun=8.5
if n_elements(gammao) eq 0 then gamma=5./3. else gamma=gammao
;if n_elements(tol) eq 0 then tol=1.e-3

if n_elements(nhao) eq 0 then nha=lla*0. else begin
	if n_elements(sel) ne 0 then nha=nhao(sel)*0.01 else nha=nhao*0.01
endelse
if n_elements(nch) eq 0 then nch=0 
if nch eq 0 then fhead='~/rosatshell/galaxies/pspc_vr_a0.5e_'
if nch eq 1 then fhead='~/rosatshell/galaxies/pspc_vr_a1.0e_'

if n_elements(blow) eq 0 then blow=4
if n_elements(bhigh) eq 0 then bhigh=5
read_mcntr,temp,nh,cr,fhead,blow,bhigh,fname=fname

; find the indexes between which nha are located in the absorption table
tabinv,nh,nha,ilo
ilo=fix(ilo)
ihi=(ilo+1) < (n_elements(nh)-1)

nbin=n_elements(lla)
cro=lla*0.
step=0.5 ;kpc
rro=findgen(40)*step+0.5*step
for k=0,nbin-1 do begin
	lls=lla(k) & bbs=bba(k) &nhs=nha(k)
;	cro(k)=simpson(func,0.,rrlimit,tol=tol)
	rr=rro ;rr may be changed in get_nt
	cro(k)=total(cro_sb(rr,indlo=ilo(k),indhi=ihi(k)))*step
endfor
return
end
;================================================
function cro_sb,rr,indlo=indlo,indhi=indhi
common para0,no,tto,lls,bbs,gamma,sigma,rrsun,ttc
common para1,temp,nh,cr,nhs
rrp=rr*cos(bbs)
zz=rr*sin(bbs)
rrc=sqrt(rrsun^2+rrp^2-2.*cos(lls)*rrsun*rrp)
get_nt,rrc,zz,nn,tt

if indlo eq indhi then cra=cr else begin 
 if n_elements(indlo) ne 0 then $
	binterp,temp,nh(indlo:indhi),cr(*,indlo:indhi),tt,nhs+tt*0. ,cra $
		else binterp,temp,nh,cr,tt,nhs+tt*0. ,cra 
endelse
cro=0.208*nn^2*cra ;surface bright (cts/s arcmin^2 kpc)
if !debug eq 2 then stop
return,cro
end
;===============================
pro get_nt,rrc,zz,nn,tt,zeroset=zeroset
;+
; zeroset - if set, the size of the array nn and tt will not be reduced;
;	negative nn is set to be zero
;-
common para0,no,tto,lls,bbs,gamma,sigma,rrsun,ttc
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
;=================================
function potential,rrc,zz
;+
; Gravitational potential from Wolfire et al. (1995)
;-
vv=225.^2
ph1=-8.887*vv/sqrt(rrc^2+(6.5+sqrt(zz^2+0.26^2))^2)
;ph1=-8.887*vv/sqrt(rrc^2+(4.5+sqrt(zz^2+0.26^2))^2)
rz2=zz^2+rrc^2
ph2=-3.0*vv/(0.70+sqrt((zz)^2+rrc^2))
;ph2=-3.0*vv/(0.70+sqrt((1.64*zz)^2+rrc^2))
term=sqrt(1.+(12.^2+rz2)/210.^2)
ph3=0.325*vv*alog((term-1.)/(term+1.))
return, ph1+ph2+ph3 ;-vv*alog(rrc/rrsun)
end

;================================================
pro map_nt,noo,ttmin,rrc,zz,nmap,tmap,ttth=ttth
common para0,no,tto,lls,bbs,gamma,sigma,rrsun,ttc
rrsun=8.5
gamma=5./3.
no=noo
tto=ttmin
sigma=(1.17e2)^2*tto
if n_elements(ttth) ne 0 then ttc=ttth else ttc=0.5 ; in units of 10^6 K

nrr=n_elements(rrc)
nzz=n_elements(zz)
index_conv,lindgen(nrr*nzz),[nrr,nzz],ind
rrv=rrc(ind(0,*))
zzv=zz(ind(1,*))
get_nt,rrv,zzv,nmap,tmap,/zeroset
nmap=reform(nmap,nrr,nzz)
tmap=reform(tmap,nrr,nzz)
return
end
