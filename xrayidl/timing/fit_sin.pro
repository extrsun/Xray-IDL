pro fit_sin,disto,fluxo,efluxo,xflux,pmin=pmin,pmax=pmax,pfix=pfix, $
mdist=mdist,mflux=mflux,binexc=binexc,silence=silence
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; fit_nlvc
;**PURPOSE:
; A nonlinear fit of a gaussian+background flux model to one-dimension
; flux distribution 
;
;**CALLING SEQUENCE:
; fit_gaussb,disto,fluxo,efluxo,xflux,pmin=pmin,pmax=pmax,pfix=pfix, 
; mdist=mdist,mflux=mflux,binexc=binexc
;
;**PARAMETERS:
;**INPUTS:
; disto - vector containing linear coordiates 
; fluxxo,efluxo - vectors containing fluxes and error bars
; xflux - as input, containing a guess of the parameter values of the model
;	0: ampilitude of the gaussian, 1: the centroid, 
;	2: the standard diviation, 3: the background level
;
;**OPTIONAL INPUTS:
; binexc - index of the data (e.g., disto) points to be excluded from the fit
; pfix - vector contains the index(es) of the parameter(s) to be fixed during
;	the fit. The value(s) of the parameter(s) should be given in the 
;	vectors, pmin and pmax. For example, to fix the third parameter equal
;	to 1 (i.e. the normalization of the opacity), you need to have pfix=2
;	and pmin(2)=1 and pmax(2)=1.
;	
;
;**OPTIONAL INPUTS or OUTPUTS:
; pmin,pmax - vectors containing user input boundaries for the parameters
; mflux - vectors containing model flux
;**OUTPUTS:
; xflux - coefs of the model
;
;**SUBROUTINES CALLED:
; curvefit_mm
;**MODIFICATION HISTORY:
; written by WQD, June 30, 1994
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- fit_gaussb,disto,fluxo,efluxo,xflux'
print,',pmin=pmin,pmax=pmax,pfix=pfix,mdist=mdist,mflux=mflux,binexc=binexc'
print,',,silence=silence'
return
endif
npara=n_elements(xflux)
if n_elements(pmin) eq 0 then pmin=fltarr(npara)
if n_elements(pmax) eq 0 then pmax=[1.e10,1.,1.e10]
xflux=(xflux < pmax) > pmin
dist=disto
flux=fluxo
ww=1./(efluxo*efluxo)
if n_elements(binexc) ne 0 then remove,binexc,dist,flux,ww
	mflux=curvefit_mm(dist,flux,ww,xflux,sigma,function_name='func_sin' $
		,chi=chi,pmin=pmin,pmax=pmax,pfix=pfix)
ndf=(n_elements(flux)-n_elements(xflux)+n_elements(pfix)) 
	;this the ndf used in curvefit_mm to get the reduced chisqr
minchisqr=chi*ndf
print,'min chisqr = ',minchisqr,ndf
print,'best fit paramters = ',xflux

mdist=min(disto)+(max(disto)-min(disto))/99.*findgen(100)
func_sin,mdist,xflux,mflux

conf_nl,flux,ww,xflux,minchisqr,conf,pmin=pmin,pmax=pmax $
	,sigma=sigma,pfix=pfix,func='func_sin',dist=dist,silence=silence
nl=n_elements(leveln)
print,'68% confidence intervals of the parameter: '
for k=0,1 do print,conf(*,k*2)
print,'90% confidence intervals of the parameter: '
for k=0,1 do print,conf(*,k*2+1)
return
end
;===================================================================
pro func_sin,x,para,yfit,pder,pvar=pvar
npara=n_elements(para)
xx=(x+para(1))
xx=(xx-fix(xx))*(2*!pi)
yfit=para(0)*sin(xx)+para(2)
pder=fltarr(n_elements(x),npara)
pder(*,0)=sin(xx)
pder(*,1)=(2.*!pi*para(0))*cos(xx)
pder(*,2)=1.
return
end
