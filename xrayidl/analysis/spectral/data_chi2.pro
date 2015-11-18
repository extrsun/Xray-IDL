 pro data_chi2,cbg,bbg,fnorm,eeg,chi2,chi,bnorm=bnorm,dof=dof,rsel=rsel
;+
; calculate the chi^2 before and after an approximate chi^2-square fit 
; normalization of two vectors.
;
; cbg, bgg - the two count vectors
; fnorm - the chi^2-square fitted normalization of bgg to match cbg
; eeg - the output weight used in the final chi^2 calculation
; chi^2 - the final chi^2 value
; chi - the relative deviation
; bnorm - existing input norm to be applied
; dof - degree of freedom
; rsel - index of the vectors to be excluded from the chi^2
;        calculation
; 
; written by wqd, June 1, 2003
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- data_chi2,cbg,bbg,fnorm,eeg,chi2,chi'
print,',bnorm=bnorm,dof=dof,rsel=rsel'
return
endif

if n_elements(bnorm) eq 0. then bnorm=1.

dof=n_elements(cbg)-1
sel=lindgen(dof+1)
nrsel=n_elements(rsel)
if nrsel ne 0 then begin
    remove,rsel,sel
    dof=dof-nrsel
endif 
eeg=cbg+bnorm^2*bbg
fnorm=total(cbg(sel)*bbg(sel)/eeg(sel))/total(bbg(sel)^2/eeg(sel))/bnorm

chi2=total((cbg(sel)-bnorm*bbg(sel))^2/eeg(sel))
print,'before norm:  chi2,dof, and sigma = ',chi2,dof,(chi2-dof)/sqrt(2.*dof)
bbg=fnorm*bnorm*bbg
;now also approximately correct for the weights. The real chi^2 would
;be a bit smaller. but if bnorm*fnorm is small, the difference should
;be small. A full consistent chi^2 fit requires a numerical solution.
eeg=cbg+(bnorm*fnorm)*bbg
chi2=total((cbg(sel)-bbg(sel))^2/eeg(sel))
print,'after norm: chi2,dof, and sigma = ',chi2,dof,(chi2-dof)/sqrt(2.*dof)
return
end
