pro cr_gal_fitw,dv,nhv,ma,ww,para,chi,mbest=mbest,opfile=opfile,kmin=kmin
if n_elements(opfile) eq 0 then opfile='/home/wqd/rosatdata/shadow/vp_a0.5_'
;opfile='/home/wqd/rosatdata/shadow/vp_a1.0_'
sh_opac,nhv*0.01>0.,op,1,opf=opfile,/nolog,/noim
sz=size(ma)
if sz(0) eq 2 then nt=sz(2) else nt=1
dvww=dv*ww
td=total(dvww)
tdop=total(dvww*op)
tdp=fltarr(nt)
for k=0,nt-1 do tdp(k)=total(dvww*ma(*,k))

tww=total(ww)
opww=op*ww
top=total(opww)
top2=total(op*opww)
tp=fltarr(nt)
topp=fltarr(nt) 
tp2=fltarr(nt)
for k=0,nt-1 do begin
	wwma=ww*ma(*,k)
	tp(k)=total(wwma)
	topp(k)=total(opww*ma(*,k))
	tp2(k)=total(wwma*ma(*,k))
endfor
para=fltarr(3,nt)
for k=0,nt-1 do begin
	bb=[td,tdop,tdp(k)]
	aa=[[tww,top,tp(k)],[top,top2,topp(k)],[tp(k),topp(k),tp2(k)]]
	para(*,k)=cramer(aa,bb,/double)
endfor
chi=fltarr(nt)
for k=0,nt-1 do $
	chi(k)=total((dv-(para(0,k)+para(1,k)*op+para(2,k)*ma(*,k)))^2*ww)
print,'chi = ',chi
print,min(chi,kmin)
print,'min chi^2 = ',chi(kmin), ' at ',kmin, $
	' and fl, fe, ne2  = ',para(*,kmin)
mbest=para(0,kmin)+para(1,kmin)*op+para(2,kmin)*ma(*,kmin)

if !debug eq 1 then stop
return
end
;========================================
pro cr_gal_fitw2,dv,nhv,ma,ww,ttv,ne2,fe,chi,mbest=mbest,opfile=opfile
if n_elements(opfile) eq 0 then opfile='/home/wqd/rosatdata/shadow/vp_a0.5_'
;opfile='/home/wqd/rosatdata/shadow/vp_a1.0_'
sh_opac,nhv*0.01>0.,op,1,opf=opfile,/nolog,/noim
sz=size(ma)
if sz(0) eq 2 then nt=sz(2) else nt=1

sumop2=total(op^2*ww)
pd=ma
for k=0,nt-1 do pd(*,k)=pd(*,k)-op*(total(ma(*,k)*op*ww)/sumop2)

dd=dv-op*(total(dv*op*ww)/sumop2)
ne2=fltarr(nt)
for k=0,nt-1 do ne2(k)=total(dd*pd(*,k)*ww)/total(pd(*,k)^2*ww)

mat=fltarr(nt)
for k=0,nt-1 do mat(k)=total(op*ma(*,k)*ww)*ne2(k)
fe=(total(dv*op*ww)-mat)/sumop2

chi=fltarr(nt)
for k=0,nt-1 do chi(k)=total((dd-ne2(k)*pd(*,k))^2*ww)
print,'chi = ',chi
print,min(chi,kmin)
print,'min chi^2 = ',chi(kmin), ' at ',kmin, $
	' and T and ne = ',ttv(kmin),sqrt(ne2(kmin))
mbest=fe(kmin)*op+ne2(kmin)*ma(*,kmin)

if !debug eq 1 then stop
return
end
;========================================
pro cr_gal_fit,dv,nhv,ma,ttv,ne2,fe,chi,mbest=mbest,opfile=opfile
if n_elements(opfile) eq 0 then opfile='/home/wqd/rosatdata/shadow/vp_a0.5_'
;opfile='/home/wqd/rosatdata/shadow/vp_a1.0_'
sh_opac,nhv*0.01>0.,op,1,opf=opfile,/nolog,/noim
sz=size(ma)
if sz(0) eq 2 then nt=sz(2) else nt=1

sumop2=total(op^2)
pd=ma
for k=0,nt-1 do pd(*,k)=pd(*,k)-op*(total(ma(*,k)*op)/sumop2)

dd=dv-op*(total(dv*op)/sumop2)
ne2=fltarr(nt)
for k=0,nt-1 do ne2(k)=total(dd*pd(*,k))/total(pd(*,k)^2)

mat=fltarr(nt)
for k=0,nt-1 do mat(k)=total(op*ma(*,k))*ne2(k)
fe=(total(dv*op)-mat)/sumop2

chi=fltarr(nt)
for k=0,nt-1 do chi(k)=total((dd-ne2(k)*pd(*,k))^2)
print,'chi = ',chi
print,min(chi,kmin)
print,'min chi^2 = ',chi(kmin), ' at ',kmin, $
	' and T and ne = ',ttv(kmin),sqrt(ne2(kmin))
mbest=fe(kmin)*op+ne2(kmin)*ma(*,kmin)

if !debug eq 1 then stop
return
end
;========================================
pro cr_gal_mult,lld,bbd,noo,ttlo,tthi,ttn,crov,gamma=gammao,nha=nhao,sel=sel $
	,nch=nch,ttv=ttv
if n_elements(sel) eq 0 then begin
	nbin=n_elements(lld)
	sel=lindgen(nbin)
endif else nbin=n_elements(sel)
ttv=ttlo+(tthi-ttlo)/(ttn-1.)*findgen(ttn)
crov=fltarr(nbin,ttn) 
for k=0,ttn-1 do begin
	cr_gal,lld,bbd,noo,ttv(k),cro,gamma=gammao,nha=nhao,sel=sel,nch=nch
	crov(*,k)=cro
endfor
return
end
;====================================
pro get_mbest,kmin,nhv,ma,ttv,ne2,fe,mbest,opfile=opfile,fl=fl
if n_elements(opfile) eq 0 then opfile='/home/wqd/rosatdata/shadow/vp_a0.5_'
;opfile='/home/wqd/rosatdata/shadow/vp_a1.0_'
sh_opac,nhv*0.01>0.,op,1,opf=opfile,/nolog,/noim
mbest=fl+fe(kmin)*op+ne2(kmin)*ma(*,kmin)
return
end