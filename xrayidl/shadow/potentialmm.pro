pro cr_gal,lld,bbd,noo,tt,cro,gamma=gammao,tol=tol,nha=nhao,sel=sel $
	,aa=aao,bb=bbo
;+
; Calculate the surface brightness count rate map of a model, accounting
;	for absorption
; lld, bbd --- galactic logitude and latitude arrays
; noo, tt - electron desity (cm^-3) and temperature (in units 10^6 K)
; cro - output count rate map
; gamma - polytropic index (def = 5/3 adiabatic)
; tol - tolerance factor of the integration 
;-
common para,no,tto,lls,bbs,gamma
common para1,temp,nh,cr,nhs
;common para2,aa,bb,alpha
if max(abs(bbd)) gt 90. or max(abs(lld)) gt 180.  then begin
	print,'abs(bba) should be < 90 deg range'
	print,'abs(lla) should be < 180 deg'
	return
endif 
if n_elements(gammao) eq 0 then gamma=5./3. else gamma=gammao
if n_elements(tol) eq 0 then tol=1.e-3
if n_elements(rrlimit) eq 0 then rrlimit=20. 
;	kpc; line-of-sight distance for integration

if n_elements(sel) ne 0 then begin
	lla=lld(sel)*(!pi/180.)
	bba=bbd(sel)*(!pi/180.)
endif else begin
	lla=lld*(!pi/180.)
	bba=bbd*(!pi/180.)
endelse
if n_elements(nhao) eq 0 then nha=lla*0. else begin
	if n_elements(sel) ne 0 then nha=nhao(sel)*0.01 else nha=nhao*0.01
endelse

if n_elements(nch) eq 0 then fname='~/rosatshell/galaxies/pspc_vr_a0.5e_45'
read_mcntr,temp,nh,cr,fname=fname

no=noo
tto=tt
;aa=aao ;using density_2
;bb=bbo
nbin=n_elements(lla)
cro=lla*0.

if n_elements(func) eq 0 then func='cro_sb'
rr=findgen(30)*0.5+0.25
for k=0,nbin-1 do begin
	lls=lla(k) & bbs=bba(k) &nhs=nha(k)
;	cro(k)=simpson(func,0.,rrlimit,tol=tol)
	cro(k)=total(cro_sb(rr))*0.5
endfor
return
end
;=============================================
pro em_gal,ll,bb,noo,tt,emm,gamma=gammao
common para,no,tto,lls,bbs,gamma
if n_elements(nch) eq 0 then gamma=5./3. else gamma=gammao
no=noo
tto=tt
nbin=n_elements(ll)
emm=ll*0.
rrlimit=40. ;kpc
for k=0,nbin-1 do begin
	lls=ll(k) & bbs=bb(k)
	emm(k)=simpson('sdensity',0.,rrlimit)
endfor
return
end
;================================================
function density,rrc,zz,rrsun=rrsun
common para,no,tto,lls,bbs,gamma
;if n_elements(gamma) eq 0 then gamma=5./3.
if n_elements(tto) eq 0 then tto=1.
sigma=(1.17e2)^2*tto
if n_elements(no) eq 0 then no=1.e-3
if n_elements(rrsun) eq 0 then rrsun=8.5
nrr=n_elements(rrc)
nzz=n_elements(zz)
index_conv,lindgen(nrr*nzz),[nrr,nzz],ind
rrv=rrc(ind(0,*))
zzv=zz(ind(1,*))
nn=fltarr(nrr,nzz)
index=(potential(rrsun,0.)-potential(rrv,zzv))/sigma
if gamma ne 1. then begin
	inside=1.+(gamma-1.)/gamma*index
	sel=where(inside gt 0.,nsel)
	if nsel ne 0. then nn(sel)=no*(inside(sel))^(2./(gamma-1.)) 
endif else nn=no*exp(index*2.)
return, nn
end 
;==========================================
function potential,rrc,zz
a1=6.5 ;units of kpc
a2=0.70
a3=12.
rrh=210.

if n_elements(rrsun) eq 0 then rrsun=8.5
b1=0.26
vcirc=225.
c1=8.887
c2=3.0
c3=0.325

vv=vcirc^2
ph1=-c1*vv/sqrt(rrc^2+(a1+sqrt(zz^2+b1^2))^2)
rz2=zz^2+rrc^2
ph2=-c2*vv/(a2+sqrt(rz2))
term=sqrt(1.+(a3^2+rz2)/rrh^2)
ph3=c3*vv*alog((term-1.)/(term+1.))
ph=ph1+ph2+ph3-vv*alog(rrc/rrsun)
stop
return, ph
end
;================================================
function cro_sb,rr
common para,no,tto,lls,bbs,gamma
common para1,temp,nh,cr,nhs
common para2,aa,bb,alpha
rrsun=8.5 
sigma=(1.17e2)^2*tto
rrp=rr*cos(bbs)
zz=rr*sin(bbs)
rrc=sqrt(rrsun^2+rrp^2-2.*cos(lls)*rrsun*rrp)

nn=density(rrc,zz,rrsun=rrsun)
;cc=no*(rrsun^2+(aa+bb)^2)^alpha
;nn=density(rrc,zz,alpha=alpha,aa=aa,bb=bb,cc=cc)

tt=tto*(nn/no)^(gamma-1)/11.6 ; in units of keV
binterp,temp,nh,cr,tt,nhs+tt*0.,cra
cro=0.208*nn^2*cra ;surface bright in units cts/s arcmin^2
if !debug eq 1 then stop
return,cro
end
;================================================
function sdensity,rr
common para,no,tto,lls,bbs,gamma
rrsun=8.5 
sigma=(1.17e2)^2*tto
rrp=rr*cos(bbs)
zz=rr*sin(bbs)
rrc=sqrt(rrsun^2+rrp^2-2.*cos(lls)*rrsun*rrp)
index=(potential(rrsun,0.)-potential(rrc,zz))/sigma
if gamma ne 1. then begin
	inside=1.+(gamma-1.)/gamma*index
if inside le 0. then stop
	nn2=no^2*(inside)^(2./(gamma-1.)) 
endif else nn2=no^2*exp(index*2.)
return, nn2
end 
;================================================
function density_2,rrc,zz,alpha=alpha,aa=aa,bb=bb,cc=cc
if n_elements(alpha) eq 0 then alpha=1.
if n_elements(aa) eq 0 then aa=6.5
if n_elements(bb) eq 0 then bb=0.26
nrr=n_elements(rrc)
nzz=n_elements(zz)
index_conv,lindgen(nrr*nzz),[nrr,nzz],ind
rrv=rrc(ind(0,*))
zzv=zz(ind(1,*))
nn=fltarr(nrr,nzz)
nn(*)=cc/(rrv^2+(aa+sqrt(zzv^2+bb^2))^2)^alpha
return, nn
end
