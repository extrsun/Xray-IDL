pro epf_sim,nsim,alpha,xmean,frac,fracerr,binwidths=binwidths,vmodel=vmodel,pnorm=pnorm,pli=pli,tcut=tcut,muvar=muvar,epf,epf_err,qdpname=qdpname
if n_params(0) eq 0 then begin
 print,'epf_sim,nsim,alpha,xmean,frac,fracerr,binwidths=binwidths,vmodel=vmodel,pnorm=pnorm,pli=pli,tcut=tcut,muvar=muvar,epf,epf_err,qdpname=qdpname'
 print,'xmean is the mean count rate'
 retall
end
if n_elements(binwidths) eq 0 then begin
 binwidths=[1.]
endif
if n_elements(tcut) eq 0 then tcut=0.
nb=(size(binwidths))(1)
openw,1,qdpname
printf,1,'READ SERR 1 2'
printf,1,'SKIP SINGLE'
printf,1,'LA X BINWIDTHS(s)'
printf,1,'LA Y EPF(T)'
printf,1,'ma 17 on 1'
printf,1,'r y 0 1'
printf,1,'r x ',binwidths(0)*0.9,binwidths(nb-1)*1.1
dummy=0.0
nb=(size(binwidths))(1)
na=(size(alpha))(1)
fnsim=double(nsim)
epf=dblarr(nb) & epf_err=dblarr(nb)
if n_elements(vmodel) eq 0 then vmodel=0
frac=dblarr(na) & fracerr=dblarr(na) & lfrac=dblarr(na)
lfracerr=dblarr(na)
for j=0l,nb-1l do begin
 cmean=xmean*binwidths(j)+fltarr(nsim)
 cts1=float(poidev(cmean,seed=seed))
 if vmodel eq 0 then begin
	cmean2=cmean
 endif
 if vmodel eq 1 then begin
 	rdir=-1.+2.*randomu(seed,nsim)
   if binwidths(j) gt tcut then begin
	cmean2=cmean*(1.+pnorm*(binwidths(j)^pli)*rdir)
   endif else begin
	cmean2=cmean
   endelse 
 endif
 if vmodel eq 2 then begin
	rdir=-1.+2.*randomu(seed,nsim)
	cmean2=cmean*(1.+ muvar*rdir) 
 endif
 cts2=float(poidev(cmean2,seed=seed))
 sep=abs(cts1-cts2)
 err=sqrt(cts1)+sqrt(cts2)
 for i=0l,na-1l do begin
  wg=where((sep ge alpha(i)*err),nwg)
  wl=where((sep lt alpha(i)*err),nwl)
 if nwg gt 0 then frac(i) = double(nwg) 
 if nwl gt 0 then lfrac(i) = double(nwl) 
 endfor
fracerr=sqrt(frac) & lfracerr=sqrt(lfrac)
frac=frac/fnsim
lfrac=lfrac/fnsim
fracerr=fracerr/fnsim
lfracerr=lfracerr/fnsim
epf(j)=frac(0) & epf_err(j)=fracerr(0)
printf,1,binwidths(j),dummy,epf(j),epf_err(j) 
endfor
close,1
return
end
