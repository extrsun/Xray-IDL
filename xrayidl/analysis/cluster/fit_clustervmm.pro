pro fit_clusterv,disto,rbpo,rbperro,yfit,pmin=pmin,pmax=pmax,sel=sel,para=para,psfp=psfp,plotoff=plotoff,distm=dist
;
if n_params() eq 0 then begin 
print,'fit_clusterv,disto,rbpo,rbperro,yfit,pmin=pmin,pmax=pmax,sel=sel,plotoff=plotoff'
return
endif
common shared,psf
if n_elements(psfp) ne 0 then psf=psfp else psf=1.
if n_elements(sel) eq 0 then sel=lindgen(n_elements(disto)) else $
	sel=sel(where(sel lt n_elements(disto)))
dist=disto(sel)
rbp=rbpo(sel)
rbperr=rbperro(sel)
if n_elements(pmin) eq 0. then pmin=0.
if n_elements(pmax) eq 0. then pmax=1.e10
;wclimit=10.
;rbp,image_c,dist,rbp,rbperr,block=block,radiushi=rlimit ;,/plotoff
;
; set initial parameter values for the nonlinear fit
if n_elements(para) eq 0 then begin
	ncomp=4
	para=dblarr(ncomp) ; an arbitrary nonzero value
	para(0)=max(rbp)-min(rbp) 
	para(1)=1.
	para(2)=1.
	para(3)=min(rbp)
	para=(para < pmax) > pmin
endif else ncomp=n_elements(para)
print,'Initial parameter values: ',para
w = 1./(rbperr*rbperr)
;	yfit=curvefit_mm(dist,rbp,w,para,sigma,function_name $
;	 ='model_cluster',chi=chi,pmin=pmin,pmax=pmax,pfix=pfix)
	yfit=curvefit(dist,rbp,w,para,sigma,function_name $
	 ='model_cluster',chi=chi) ;,pmin=pmin,pmax=pmax,pfix=pfix)
;	w = 1./(yfit+backvc)

ndf=n_elements(dist)-n_elements(para)+n_elements(pfix)
chisqrmin=chi*ndf
print,'min chisqr = ',chisqrmin,ndf
print,'best fit paramters = ',para
if keyword_set(plotoff) eq 0 then begin
	plot_io,dist,rbp>1.e-8,psym=7
	oploterr,dist,rbp,rbperr
	oplot,dist,yfit
endif
sigma=para
if n_elements(pfix) ne 0 then remove,pfix,sigma
return
end
;===================================================================
pro model_cluster,x,para,yfit,pder 
common shared,psf
ncomp=n_elements(para)
bb=1.+(x/para(1))^2
;nn=0.5-3.*para(2)
nn=0.5-3.*1. ;0.6
aa=para(0)*bb^nn
yfit=aa+para(ncomp-1)
if n_elements(psf) gt 1 then yfit=convolve(yfit,psf)
pder=fltarr(n_elements(x),ncomp)
pder(*,0)=aa/para(0)
pder(*,1)=-2.*x^2/para(1)^3*nn*aa/bb
;pder(*,2)=-3.*aa*alog(bb)
pder(*,ncomp-1)=1.
;if !debug eq 1 then stop
return
end