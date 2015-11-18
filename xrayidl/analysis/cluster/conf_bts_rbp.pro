pro conf_bts_rbp,dist,fmv,bav,expv,pam,pmin=pmin,pmax=pmax,sel=sel $
	,inpar=inpar,nsim=nsim,seed=seed,paraa=paraa,siglevel=siglevel,funn=funn,binex=binex
;+
; calculate confidence limits using bootstrapping samples of a model rbp.
; writen by wqd, 10/6/1996
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - conf_bts_rbp,dist,fmv,bav,expv,pam'
print,',pmin=pmin,pmax=pmax,sel=sel,inpar=inpar,nsim=nsim,seed=seed'
print,',paraa=paraa,siglevel=siglevel'
return
endif

nbin=n_elements(fmv)
ncm=fmv*expv+bav
fmver=sqrt(ncm)/expv
ncm_n=ncm-bav

for k=0,nsim-1 do begin
 para=inpar
 fmvr=(randomn(seed,nbin)*sqrt(ncm)+ncm_n)/expv
 if n_elements(funn) eq 0 then $
   fit_clusterv,dist,fmvr,fmver,pmin=pmin,pmax=pmax,sel=sel,para=para,/plotoff $
else  begin
if !debug eq 2 then stop
   curvefit_top,dist,fmvr,fmver,para,binex=binex,funn=funn,chiv=chiv,/plotoff
endelse 

 if k eq 0 then begin
	sz=size(para)
	paraa=fltarr(sz(1),nsim)
 endif
 paraa(*,k)=para
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
