;===================================================================
pro gauss_b,x,para,yfit,pder
; gaussian model - to be used by curvefit
; para -
;	0: ampilitude of the gaussian, 1: the centroid, 
;	2: the standard diviation, 3: the background level
npara=n_elements(para)
yfit=gaussian(x,para(0:npara-2),pderiv)+para(npara-1)
sz=size(pderiv)
pder=fltarr(sz(1),npara)
pder(*,0:npara-2)=pderiv
pder(*,npara-1)=1.
return
end
;===================================================================
pro gauss_br,x,para,yfit,pder 
; same as gauss_b, except for the centroid position, which is assumed to be
; fixed. 
yfit=gaussian(x,[para(0),0.,para(1)],pderiv)+para(2)
pder=fltarr(n_elements(x),n_elements(para))
pder(*,0)=pderiv(*,0)
pder(*,1)=pderiv(*,2)
pder(*,2)=1.
return
end
;===================================================================
pro gauss_r,x,para,yfit,pder
; same as gauss_b, except for both the centroid position and background
; , which is assumed to be fixed. 
yfit=gaussian(x,[para(0),0.,para(1)],pderiv)
pder=fltarr(n_elements(x),n_elements(para))
pder(*,0)=pderiv(*,0)
pder(*,1)=pderiv(*,2)
return
end
;===================================================================
pro pow_br,x,para,yfit,pder ;for fits with a fixed centroid
; power law model with the centroid fixed
yfit=para(0)*abs(x)^(-para(1))+para(2)
pder=fltarr(n_elements(x),n_elements(para))
pder(*,0)=abs(x)^(-para(1))
pder(*,1)=-para(0)*alog(abs(x))*abs(x)^(-para(1))
;sel=where(x lt 0,nsel) 
;if nsel ne 0 then pder(sel,1)=-x*pder(sel,1) else pder(sel,1)=x*pder(sel,1)
pder(*,2)=1.
return
end
;===================================================================
pro exp_br,x,para,yfit,pder ;for fits with a fixed centroid
; exponetial law model with the centroid fixed
yfit=para(0)*exp(-abs(x)/para(1))+para(2)
pder=fltarr(n_elements(x),n_elements(para))
pder(*,0)=exp(-abs(x)/para(1))
;pder(*,1)=(para(0)/para(1)^2)*abs(x)*exp(-abs(x)/para(1))
pder(*,1)=-(para(0)/para(1))*abs(x)*exp(-abs(x)/para(1))
;sel=where(x lt 0,nsel) 
;if nsel ne 0 then pder(sel,1)=-x*pder(sel,1) else pder(sel,1)=x*pder(sel,1)
pder(*,2)=1.
return
end
;===================================================================
pro exp_r,x,para,yfit,pder ;for fits with a fixed centroid
; same as exp_br, except for the cbackground
; , which is assumed to be fixed. 
yfit=para(0)*exp(-abs(x)/para(1))
pder=fltarr(n_elements(x),n_elements(para))
pder(*,0)=exp(-abs(x)/para(1))
pder(*,1)=(para(0)/para(1)^2)*abs(x)*exp(-abs(x)/para(1))
return
end

;===================================================================
pro expgs_br,x,para,yfit,pder ;for fits with a fixed centroid
; exponetial law + gaussian model with the centroid fixed
npara=n_elements(para)
pder=fltarr(n_elements(x),n_elements(para))
yfit=para(0)*exp(-abs(x)/para(1))
pder(*,0)=exp(-abs(x)/para(1))
pder(*,1)=(para(0)/para(1)^2)*abs(x)*exp(-abs(x)/para(1))
yfit=yfit+gaussian(x,[para(npara-3),0.,para(npara-2)],pderiv)+para(npara-1)
pder(*,npara-3)=pderiv(*,0)
pder(*,npara-2)=pderiv(*,2)
pder(*,npara-1)=1.
return
end
;===================================================================
pro expgs_br2,x,para,yfit,pder ;for fits with a fixed centroid
; exponetial law + gaussian model with the centroid fixed
npara=n_elements(para)
pder=fltarr(n_elements(x),n_elements(para))
yfit=para(0)*exp(-abs(x)/para(1))
yfit=yfit+gaussian(x,[para(npara-3),0.,para(npara-2)],pderiv)+para(npara-1)
pder(*,0)=exp(-abs(x)/para(1))
pder(*,1)=(para(0)/para(1)^2)*abs(x)*exp(-abs(x)/para(1))
pder(*,npara-3)=pderiv(*,0)
pder(*,npara-2)=pderiv(*,2)
pder(*,npara-1)=1.
for k=0,npara-1 do pder(*,k)=pder(*,k)*yfit*2.
yfit=yfit^2
return
end
;=======================================================================
;=======================================================================
pro curvefit_top,disto,fluxo,efluxo,para,mflux=mflux,binexc=binexc,funname=funname,sigma=sigma,chiv=chiv,plotoff=plotoff
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; curvefit_top
;**PURPOSE:
; Top program for the nonlinear fit a one-dimension distribution, using 
; the existing models or any user suppplied model.
;
;**CALLING SEQUENCE:
; curvefit_top,disto,fluxo,efluxo,para,pfix=pfix,mflux=mflux
; ,binexc=binexc,funname=funname,sigma=sigma
;
;**PARAMETERS:
;**INPUTS:
; disto - vector containing linear coordiates 
; fluxxo,efluxo - vectors containing fluxes and error bars
; para - as input, containing a guess of the parameter values of the model
;
;**OPTIONAL INPUTS:
; binexc - index of the data (e.g., disto) points to be excluded from the fit
;
;**OPTIONAL INPUTS or OUTPUTS:
; mflux - vectors containing model flux
;**OUTPUTS:
; para - coefs of the model
; sigma - the uncertainty sigma of the model parameters?
; chiv - deviations of individual data bins from the best-fit model
;
;**SUBROUTINES CALLED:
; curvefit
;
;**MODIFICATION HISTORY:
; written by WQD, Sept 17, 2003
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- curvefit_top,disto,fluxo,efluxo,para,plotoff=plotoff'
print,',mflux=mflux,binexc=binexc,funname=funname,sigma=sigma,chiv=chiv'
return
endif
dist=disto
flux=fluxo
ww=1./(efluxo*efluxo)
nbinexc=n_elements(binexc)
if nbinexc ne 0 then remove,binexc,dist,flux,ww
;funname='expgs_br'
;funname='gauss_b'
;funname='gauss_br'
if n_elements(funname) eq 0 then funname='exp_br'
	mflux=curvefit(dist,flux,ww,para,sigma,function_name=funname $
	 ,chisq=chi) 
ndf=(n_elements(flux)-n_elements(para)) 
	;this the ndf used in curvefit_mm to get the reduced chisqr
minchisqr=chi*ndf
print,'min chisqr, dof, prob = ',minchisqr,ndf,(minchisqr-ndf)/sqrt(2.*ndf)
print,'best fit paramters = ',para
chiv=(flux-mflux)*sqrt(ww)
if nbinexc ne 0 then CALL_PROCEDURE, Funname,disto,para,mflux
if keyword_set(plotoff) eq 0 then begin
	ploterr,disto,fluxo,efluxo,psym=7
	oplot,disto,mflux
	if nbinexc ne 0 then oplot,disto(binexc),fluxo(binexc),psym=6
    endif
if !debug eq 1 then stop
end
;===================================================================
