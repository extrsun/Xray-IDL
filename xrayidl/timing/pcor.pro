pro pcor,time,pvo,dvo,dd,plot=plot,nb=nb,prob=prob,phase=phase,pvm=pv,dvm=dv,pbest=pbest,dvoth=dvoth
common,mean,fmean
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- pcor,time,pmax,dd,pmin=pmin,plot=plot,phd=phd'
print,',prob=prob,phase=phase,pv=pv,dv=dv'
return
endif
; dd = pdot/(p^2)
if n_elements(prob) eq 0 then prob=0.00135 ;3 sigma
if n_elements(pmin) eq 0 then pmin=0.01
if n_elements(dvoth) ne 0 then begin
	s=where(dvo ge dvoth,nsel)
	if nsel ne 0 then begin
		pn=pvo(s) & dn=dvo(s)
	endif else stop,'nsel eq 0 with the chosen dvoth'
endif else begin
	pn=pvo
	dn=dvo
endelse
np=n_elements(pn)

nc=n_elements(time)
get_ksd,nc,prob,dth
print,'probth, dth = ',prob,dth
plot=1
dv=[-999]
pv=[-999]
fmean=n_elements(time)/float(nb)
ww=1.
b=0.5*(dd*time)
rtime=1./time
for k = 0, np-1 do begin
	rp=1./pn(k)
	pht=time*(rp) ;-b)
	ph=pht-long(pht)
	f=histogram(ph,bin=1./nb)
	chic=total((f-fmean)^2/)/nb
	fit_test,p,f,ww,chi=chi,fmean=fmean
	chir=chic/(chi)
	pv=[pv,pn(k)]
	dv=[dv,chir]
if !debug eq 1 then stop
endfor
return 
end
;=============================
function func,x
return,x
end