pro ppdot_fold,time,fmin,fmax,minfdot,maxfdot,phd=phd,vfv=vfv,vdv=vdv,ddf=ddf $
 ,fv=fv,dv=dv,phase=phase,fbest=fbest,dmax=dmax,fdotbest=fdotbest,tmin=tmin,ffar=ffar,tfar=tfar
if n_elements(tmin) eq 0 then tmin=min(time)
dfdot=2.0d*phd/(time(n_elements(time)-1)-tmin)^2
nfdot=nint((maxfdot-minfdot)/dfdot)
print,'dfdot,nfdot = ',dfdot,nfdot

fdot=minfdot+dfdot*dindgen(nfdot+1)
vdmax=fltarr(nfdot+1)
vfbest=dblarr(nfdot+1)
for k=0,nfdot do begin
	print,'fdot = ',fdot(k)
	pfold,time,fmin,fmax,fdot(k),phd=phd,fbest=fbest,dmax=dmax $
		,tmin=tmin,fv=fv,dv=dv,/plotoff,ddf=ddf,ffar=ffar,tfar=tfar  
			;,/proc_m
	if k eq 0 then begin
		nc=n_elements(dv)
		vdv=fltarr(nc,nfdot+1)
	endif
	vdmax(k)=dmax
	vfbest(k)=fbest
	vdv(*,k)=dv
endfor
vfv=fv
dmax=max(vdmax,bestind)
fbesto=vfbest(bestind)
fdotbest=fdot(bestind)
fint=(fmax-fmin)*0.5
print,'bestind, fbesto, fdotbest = ',bestind,fbesto,fdotbest
pfold,time,fbesto-fint,fbesto+fint,fdotbest,fbest=fbest,dmax=dmax $
 ,tmin=tmin,phd=phd*0.5,fv=fv,dv=dv,phase=phase,ddf=ddf,ffar=ffar,tfar=tfar
stop
return
end