pro lrtest,an,ns,nsim,lr,seed=seed
poisson,fltarr(ns,nsim)+an,rana,seed
lr=fltarr(nsim)
for k=0,nsim-1 do begin
	sel=where(rana(*,k) ne 0,nsel)
	nc=rana(sel,k)
	lr(k)=2.*total(nc*alog(nc/an))
endfor
bin=1.
echi=ns-1
echie=sqrt(echi*2.)
nh=100
minh=echi-6.*echie
maxh=echi+6.*echie
bin=(maxh-minh)/nh
lrh=histogram(lr,bin=bin,min=minh,max=maxh)
xa=minh+(maxh-minh)/nh*findgen(nh)+0.5*bin
plot,xa,lrh
stop
return
end



;chid=fltarr(nh+1)
;for k=0,nh-1 do chid(k)=chi_sqr1(xa(k),echi)
;oplot,xa,chid*ns