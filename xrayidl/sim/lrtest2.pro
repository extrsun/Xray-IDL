pro lrtest2,namin,namax,nad,ns,nsim,lr,seed=seed

lr=fltarr(nsim)
na=namin+(namax-namin)/float(nad)*findgen(nad+1)
lra=fltarr(nad+1)
lrae=fltarr(nad+1)
for nn=0,nad do begin
; poisson,fltarr(ns,nsim)+na(nn),rana,seed
 rana=poidev(fltarr(ns,nsim)+na(nn),seed=seed)
 for k=0,nsim-1 do begin
	sel=where(rana(*,k) ne 0,nsel)
	nc=rana(sel,k)
	lr(k)=2.*total(nc*alog(nc/na(nn)))
 endfor
 get_stat,lr,af,afe
 lra(nn)=af
 lrae(nn)=afe
endfor
plot,na,lra/(ns-1.),xtit='Counts per bin',ytitl='avg(LR)/d.o.f',chars=2
plot,na,lrae/sqrt((ns-1.)*2.),xtit='Counts per bin',ytitl='rms(LR)/sqrt(2*d.o.f)',chars=2
stop
return
end



;chid=fltarr(nh+1)
;for k=0,nh-1 do chid(k)=chi_sqr1(xa(k),echi)
;oplot,xa,chid*ns