 pro fit_chi2,ocbg,obbg,oeeg,ff,para,chi2,dof=dof,rsel=rsel
;+
; calculate the chi^2 after a chi^2-square fit to a model with a 
; normalization and background, used to fit the a source rbp with a PSF.
;
; ocbg, obgg, oeeg - the two count vectors and the total error vector
; ff - the best-fit profile
; para - containing the two parameters
; eeg - the output weight used in the final chi^2 calculation
; chi^2 - the final chi^2 value
; dof - degree of freedom
; rsel - index of the vectors to be excluded from the chi^2
;        calculation
; 
; written by wqd, April 13, 2005
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- fit_chi2,ocbg,obbg,oeeg,ff,para,chi2,dof=dof,rsel=rsel'
return
endif

cbg=ocbg
bbg=obbg
eeg=oeeg
nrsel=n_elements(rsel)
if nrsel ne 0 then remove,rsel,cbg,bbg,eeg
dof=n_elements(cbg)-2
ww=1./eeg^2
a11=total(bbg^2*ww)
a12=total(bbg*ww)
a22=total(ww)
c1=total(cbg*bbg*ww)
c2=total(cbg*ww)
ac=(c1*a22-c2*a12)/(a11*a22-a12^2)
bc=-(c1*a12-c2*a11)/(a11*a22-a12^2)
ff=ac*bbg+bc
chi2=total((cbg-ff)^2*ww)
print,'after norm: chi2,dof, and sigma = ',chi2,dof,(chi2-dof)/sqrt(2.*dof)
para=[ac,bc]
return
end
