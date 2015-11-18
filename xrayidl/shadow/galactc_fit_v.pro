pro galactc_fit_v,binval,gladval,coef,ww=ww
;+
; fit the distribution of binval with a 2-D infinite disk model.
; writen by WQD, 4/17/94
;-
; 
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - galactc_fit_v,binval,gladval,coef,ww=ww'
return
endif
if n_elements(ww) eq 0 then ww=binval*0.+1. ;arbitrary value
nsel=n_elements(binval)
	ncomp=2
	func=fltarr(nsel,ncomp)
  	func(*,0)=1.
  	func(*,1)=1./sin(abs(gladval*(!pi/180.)))
  	coef=funcfitw(binval,binval,ww,func,ncomp,yfit,yband,sigma,var)
;
  	for kk=0,(ncomp-1) do begin
		print,'coef = ', coef(kk),' +- ',sqrt(var(kk,kk))
  	endfor
  	ndf=(nsel-ncomp)
  	chi=sigma*sigma*(nsel-ncomp)
 	print,'chi = ', chi,(nsel-ncomp)
	; chi and var are meaningful only when ww is meaningful
end
