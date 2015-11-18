pro fit_conf,xflux,xfluxe,minnorm,snorm,countvc,backvc,timevc,nhvc,op, $
  nf1=nf1,nf2=nf2,nf3=nf3,factor=factor
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;**NAME:
; fit_conf
;**PURPOSE:
; Find confidence levels for chi^2 with a three parameter model.
;**CALLING SEQUENCE:
; fit_conf,xflux,xfluxe,minnorm,snorm,countvc,backvc,timevc,nhvc,op, $
;  nf1=nf1,nf2=nf2,nf3=nf3,factor=factor
;**PARAMETERS:
;**INPUTS:
; xflux == fltarr(2) containing background and foreground flux;
; xfluxe == one sigma error on xflux;
; minnorm == normalization for minimum chi^2;
; snorm == one sigma error on minnorm;
; countvc, backvc, timevc == count, background, and exposure vectors;
; nhvc == column density vector;
; op == absorption due to opacity vector.
;**OPTIONAL INPUTS:
; nf1, nf2, nf3 == Number of increments of chi^2 array in each dimension;
; factor == factor for changing range of parameters.
;**PROCEDURE:
;**EXAMPLES:
; fit_conf,xflux,xfluxe,.62,.12,countvc,backvc,timevc,nhvc,op
;**RESTRICTIONS:
;**NOTES:
;**MODIFICATION HISTORY:
; Written by kachun 11 August, 1993; changed program from fit_flux to
;  let it calculate dispersion for other parameters.
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
  print,'CALLING SEQUENCE --- fit_conf,xflux,xfluxe,fluxvc,op'
  print,'  nhvc,op,nf1=nf1,nf2=nf2,nf3=nf3,factor=factor'
  return
endif
if n_elements(nf) eq 0 then nf = 15
if n_elements(factor) eq 0 then factor = 3.
ncol = n_elements(countvc(*))
sz=size(xflux)
np=sz(2)
coef(2*nf+1,np)

;** Fill coefficients array with limits equal to +/- factor*error:
for k=0,np-1 do begin
coef(*,k)=xflux(k)+(findgen(nf*2+1)-nf)*(2.*factor*xfluxe(k))/(nf*2+1)
endfor

index_conv,lindgen(nbin),lonarr(np)+nf,indexv
for kk=0,nbin-1 do begin
	get_mflux,coef(kk,*),model,opv=opv
    	modele = (model/timevc)
	chi(kk)=total((flux-model)*(flux-model)*timevc/model
endfor
  minchi = chi2(nf1,nf2,nf3)
  print,'  minimum chi ^2  = ',minchi
  print,'  with minnorm = ',minnorm,' +/-',snorm
  for i=0,1 do print,'  xflux = ',xflux(i),' +/-',xfluxe(i)
endfor

;** Find confidence levels of chi^2's; confidence levels are del chi^2
;** equal to 1. (for 68%), 2.71 (for 90%), and 6.63 (for 99%):
err = [1.,2.71,6.63] + minchi
cnf_index = intarr(2,3,3) ;** Array for index values of the confidence levels.
;** Loop over first coefficient:
for i=0,2 do begin  ;** Loop over the three confidence levels.
  sel = where(chi2 le err(i))
  sel3 = fix(sel/((2*nf1+1)*(2*nf2+1)))
  sel2 = fix((sel - sel3*(2*nf1+1)*(2*nf2+1))/(2*nf1+1))
  sel1 = fix(sel - sel3*(2*nf1+1)*(2*nf2+1) - sel2*(2*nf1+1))
  cnf_index(*,i,0) = minmax(sel1)
  cnf_index(*,i,1) = minmax(sel2)
  cnf_index(*,i,2) = minmax(sel3)
endfor
stop

;** Interpolate over 3-parameter space to get most accurate locations of
;** confidence levels:
;for i=0,2 do begin    ;** Loop over the three confidence levels.
;  for j=0,1 do begin  ;** Loop over the two places where the chi^2 confidence
;                      ;** is defined.
;    temp = chi2(cnf_index(j,i,0)-1:cnf_index(j,i,0)+1, $
;      cnf_index(j,i,1)-1:cnf_index(j,i,1)+1, $
;      cnf_index(j,i,2)-1:cnf_index(j,i,2)+1)
;    ;** Interpolate for accurate confidence levels:
;    cinterp,temp,err(i),cx,cy,cz
;  endfor
;endfor
;stop

if !debug eq 1 then stop
end
