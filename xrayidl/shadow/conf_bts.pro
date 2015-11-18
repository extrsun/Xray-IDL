pro conf_bts,nhvc,countvc,backvc,timevc,xfluxa,xfluxm,xfluxlo,xfluxhi $
,siglevel=siglevel,pri=pri,chia=chia,chim=chim,chilo=chilo,chihi=chihi $
,bnlo=bnlo,bnhi=bnhi,opfile=opfile,mflux=mflux,chimin=chimin $
,pmin=pmin,pmax=pmax,pfix=pfix,gnhvc=gnhvc,noconf=noconf,xflux_ini=xflux_ini $
,gopfile=gopfile
;-
; writen by wqd, Oct, 1995
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - '
print,'conf_bts,nhvc,countvc,backvc,timevc,xfluxa,xfluxm,xfluxlo,xfluxhi'
pritn,',siglevel=siglevel,pri=pri,chia=chia,chim=chim,chilo=chilo,chihi=chihi'
print,',bnlo=bnlo,bnhi=bnhi,opfile=opfile,mflux=mflux,chimin=chimin'
print,',pmin=pmin,pmax=pmax,pfix=pfix,gnhvc=gnhvc,noconf=noconf
print,',xflux_ini=xflux_ini,gopfile=gopfile'
return
endif

sz=size(countvc)
nsim=sz(sz(0))
for k=0,nsim-1 do begin
 if sz(0) eq 3 then begin ; for more than one band
	fit_nlvc,nhvc,countvc(*,*,k),backvc,timevc,xflux $
	,bnlo=bnlo,bnhi=bnhi,opfile=opfile,mflux=mflux,chimin=chimin $
	,pmin=pmin,pmax=pmax,pfix=pfix,gnhvc=gnhvc,/noconf,/plotoff $
	,xflux_ini=xflux_ini,gopfile=gopfile
 endif else if sz(0) eq 2 then $
	fit_nlvc,nhvc,countvc(*,k),backvc,timevc,xflux $
	,bnlo=bnlo,bnhi=bnhi,opfile=opfile,mflux=mflux,chimin=chimin $
	,pmin=pmin,pmax=pmax,pfix=pfix,gnhvc=gnhvc,/noconf,/plotoff $
	,xflux_ini=xflux_ini,gopfile=gopfile
 if k eq 0 then begin
	xfluxa=fltarr(n_elements(xflux),nsim)
	chia=fltarr(nsim)
 endif
 xfluxa(*,k)=xflux
 chia(k)=chimin
endfor

bts_conf,xfluxa,xfluxm,xfluxlo,xfluxhi,siglevel=siglevel,pri=pri
avg_median,chia,chim,chilo,chihi,siglevel=siglevel
print,'chim,chilo,chihi = ',chim,chilo,chihi
return
end