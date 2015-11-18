pro xtrans,nh,geom,ens,trans,taut,taucrit
if n_params(0) eq 0 then begin
 print,'xtrans,nh,geom,ens,trans,taut,taucrit'
 print,'Compute X-ray transmission in cold matter using '
 print,'analytical approixmations '
 print,'NH is in units of 1E21 cm^2 '
 print,'ENS is output energy in keV '
 print,'GEOM = 0 for plane, 1 for sphere '
 retall
end
fname='/home/astd1/yaqoob/refl/albedo.dat'
readcol,fname,ens,alb
npts=n_elements(ens) & trans=fltarr(npts)
taut = 1.2*0.665e-3*nh/alb
taucrit=alog((3.-2.*alb)/2./(1.-alb))
if geom eq 0 then begin
 wlt=where((taut lt taucrit),nwlt)
 wge=where((taut ge taucrit),nwge)
 print,nwlt,nwge
 if nwlt gt 0 then trans(wlt)=alb(wlt)+(1.-alb(wlt))*exp(-taut(wlt))
 if nwge gt 0 then $
 trans(wge)=exp(-taut(wge))*(1.+(alb(wge)/2./(1.-alb(wge))))
endif
return
end

