pro conf_bts_cl,im_bts,dim,bflux,parain $
 ,block=block,verb=verb,offaxis=offaxis,hpsf=hpsf,clim=clim,chn=chn $
 ,paraa,param,paralo,parahi,soa=soa $
 ,siglevel=siglevel,pri=pri,chia=chia
;+
; calculate confidence limits using bootstrapping samples of a cluster model.
; writen by wqd, 7/15, 1995
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - conf_bts_cl,im_bts,dim,bflux,para'
print,' ,block=block,verb=verb,offaxis=offaxis,hpsf=hpsf,clim=clim,chn=chn '
print,',paraa,param,paralo,parahi,soa=soa'
print,' ,siglevel=siglevel,pri=pri,chia=chia'
return
endif

sz=size(im_bts)
nsim=sz(sz(0))
for k=0,nsim-1 do begin
 image_loc,im_bts(*,*,k),loc
 para=parain
 mle_cl_main,loc,dim,bflux,para,block=block,verb=verb,offaxis=offaxis $
	,hpsf=hpsf,clim=clim,chn=chn,cstat=chimin,so=so,psffto=psffto
 if k eq 0 then begin
	paraa=fltarr(n_elements(para),nsim)
	chia=fltarr(nsim)
	if chn eq 2 then soa=fltarr(nsim) ;to store the central intensity
if !debug eq 1 then stop,'inside the loop of conf_bts_cl'
 endif
 paraa(*,k)=para
 chia(k)=chimin
 if chn eq 2 then soa(k)=so
 print,'k, para, chimin = ',k, para, chimin
endfor
;-------------------------------
bts_conf,paraa,param,paralo,parahi,siglevel=siglevel,pri=pri

avg_median,chia,chim,chilo,chihi,siglevel=siglevel
print,'chim,chilo,chihi = ',chim,chilo,chihi

avg_median,soa,som,solo,sohi,siglevel=siglevel
print,'som,solo,sohi = ',som,solo,sohi
return
end