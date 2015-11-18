pro curvefit_conf,dist,fmv,bav,expv,pam,pmin=pmin,pmax=pmax,sel=sel $
,inpar=inpar,nsim=nsim,seed=seed,paraa=paraa,siglevel=siglevel $
,binex=binex,funn=funn
;+
; calculate confidence limits using bootstrapping samples of the best-fit model. see curvefit_top.pro
; writen by wqd, Sept. 17, 2003
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - curvefit_conf,dist,fmv,bav,expv,pam'
print,',pmin=pmin,pmax=pmax,sel=sel,inpar=inpar,nsim=nsim'
print,',seed=seed,paraa=paraa,siglevel=siglevel,binex=binex,funn=funn'
return
endif

nbin=n_elements(fmv)
ncm=fmv*expv+bav
fmver=sqrt(ncm)/expv
ncm_n=ncm-bav

for k=0,nsim-1 do begin
 para=inpar
 fmvr=(randomn(seed,nbin)*sqrt(ncm)+ncm_n)/expv
; fit_clusterv,dist,fmvr,fmver,pmin=pmin,pmax=pmax,sel=sel,para=para,/plotoff
  curvefit_top,dist,fmvr,fmver,para,binex=binex,funn=funn,/plotoff
 if k eq 0 then begin
	sz=size(para)
	paraa=fltarr(sz(1),nsim)
 endif
 paraa(*,k)=para
if para(2) le 0 then stop
endfor

pam=fltarr(sz(1),3)
for i=0,sz(1)-1 do begin
	avg_median,paraa(i,*),pm,plo,phi,siglevel=siglevel
	pam(i,0)=pm
	pam(i,1)=plo
	pam(i,2)=phi
endfor
print,pam
return
end
