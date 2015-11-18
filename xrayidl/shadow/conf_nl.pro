pro conf_nl,flux,w,parao,chisqrmin,conf,level=level,pmin=pmin,pmax=pmax $
	,sigmao=sigmao,pfix=pfix,func=func,dist=dist,silence=silence
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; conf_nl
;
;*PURPOSE:
; Calculate parameter ranges at specfied confidence levels
;
;*CALLING SEQUENCE:
; conf_nl,flux,w,parao,chisqrmin,conf,leveln=leveln,pmin=pmin,pmax=pmax
;	,sigmao=sigmao,pfix=pfix,func=func,dist=dist
;
;*PARAMETERS:
; INPUTS:
; flux - vector containing the Y axis data values
; w - vector containing the weight of the fit 
; parao - the best fit values of model parameters
; chisqrmin - the minimum chisqr (no reduced) 
; func - the model function
; pmin,pmax - vectors containing user input boundaries for the parameters
; level - vector containing the confidence levels (i.e., [1., 2.7] for
;	 68% and 90% condidence levels)
; pfix - vector contains the index(es) of the parameter(s) to be fixed during
;	the fit. The value(s) of the parameter(s) should be given in the 
;	vectors, pmin and pmax. For example, the fix the third parameter equal
;	to 1 (i.e. the normalization of the opacity), you need to have pfix=2
;	and pmin(2)=1 and pmax(2)=1.
; sigmao - values of the model parameters which are going to be divided by
; 	200 (arbitray values) to be used as steps in the calculation of
;	the parameter ranges.
; dist - X coordinates
;
;*OUTPUTS:
; conf - parameter values at the confidences levels
;
;*PROCEDURE:
;
;
;*EXAMPLES:
; 
;*RESTRICTIONS:
; tested only for one band now.
;*NOTES:
;
;
;*SUBROUTINES CALLED:
;
;
;*MODIFICATION HISTORY:
; writen by WQD, Aug 20, 1993
; add keyword pfix for fixing the parameter that was fixed during best fit
; 
; WQD, Nov 6, 1993
; add the keyword dist for X coordinates and comment out two lines
; where foreground and background fluxes were assumed 
; so the program can now be used for other function fits. wqd, July 1, 1994
;
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params () eq 0 then begin
	print,'CALLING SEQUENCE --  conf_nl,flux,w,parao,chisqrmin,conf $
	print,',level=level,pmin=pmin,pmax=pmax,sigmao=sigmao,dist=dist'
	print,',silence=silence'
	return
endif
if n_elements(level) eq 0 then level=[1.,2.71] ;68\% and 90\% confidence levels
if n_elements(pmin) eq 0 then pmin=parao*0.-1.e22
if n_elements(pmax) eq 0 then pmax=parao*0.+1.e22
if n_elements(sigmao) eq 0 then sigmao=parao
if n_elements(func) eq 0 then func='model_shadow'
if n_elements(dist) eq 0 then dist=flux
pmin=double(pmin)
pmax=double(pmax)
npara=n_elements(parao)
npfix=n_elements(pfix)
pfree=indgen(npara)
if npfix ne 0 then remove,pfix,pfree
npfree=n_elements(pfree)
ndf=n_elements(flux)-npfree+1
; this IS the ndf used in curvefit_mm to calculate the reduce chisqr
nl=n_elements(level)
para=parao
conf=fltarr(npara,nl*2)
dpara=fltarr(npara,2)
dpara(pfree,0)=-sigmao/100. ;chosen to make the increments small enough
dpara(pfree,1)=sigmao/100.  ; i.e., 1% accuracy
maxkk=5000 ;upper boundary will be a factor of 10 larger than 
		;the best-fit value
chisqrv=fltarr(maxkk+1)
chisqrv(0)=chisqrmin
For nn=0,npfree-1 do begin ;loop over individual parameters
  k=pfree(nn)
if npfix ne 0 then  newpfix=[pfix,k] else newpfix=k
;these parameter values are now fixed in each fit
  for i=0,1 do begin ; loop over lower and upper limits
     kk=0
     for j=0,nl-1 do begin ;loop over individual confidence levels
       repeat begin
	para(k)=(para(k)+dpara(k,i) > pmin(k)) < pmax(k)
	;	if k eq 1 then para(0)=para(0)-dpara(k,i) > pmin(0)
	;	if k eq 0 then para(1)=para(1)-dpara(k,i) > pmin(1)
	;to optimize the choice of initial parameter values
	;the k=0,1 components are assumed to be foreground and background 
   	yfit=curvefit_mm(dist,flux,w,para,sigma, $
		function_name=func,chisqr=chisqr $
		,pmin=pmin,pmax=pmax,pfix=newpfix)
;	print,'k,i,j,para(k),chisqr ',k,i,j,para,chisqr
	kk=kk+1
	if kk eq maxkk then begin
		conf(k,j+i*nl)=para(k) 
		if n_elements(silence) eq 0 then print,'the limit exceeds the boundary (a factor of 10 of the best fit value)'
		 goto,done
	endif
	chisqrv(kk)=chisqr*ndf
	ok=chisqrv(kk) gt chisqrmin+level(j)
	if not ok then begin
	 if para(k) le pmin(k) then begin 
	   conf(k,j+i*nl)=pmin(k) 
	   if n_elements(silence) eq 0 then print,'hit the boundary! chisqrv(kk), chisqrmin+level(j) =', $
		chisqrv(kk), chisqrmin+level(j)
	   goto,done 
	   endif
	 if para(k) ge pmax(k) then begin 
	   conf(k,j+i*nl)=pmax(k) 
	   if n_elements(silence) eq 0 then print,'hit the boundary! chisqrv(kk), chisqrmin+level(j) =', $
		chisqrv(kk), chisqrmin+level(j)
	   goto,done 
	   endif
	endif
       endrep until ok
	if n_elements(silence) eq 0 then print,'get the confidence level between bins: ',kk-1,kk
	tabinv,chisqrv(kk-1:kk),chisqrmin+level(j),eindex
	linterp,[0.,1.],[para(k)-dpara(k,i),para(k)],eindex,value
	conf(k,j+i*nl)=value
;	print,'Confidence limits for parameter ',k,j+i*nl,' is ',conf(k,j+i*nl)
	done: 
    endfor
	para=parao
;stop
  endfor
endfor
;print,'Confidence limits for the parameters'
;print,'lower-upper limits\parameters'
;for j=0,nl-1 do begin
;	print,'level ',j+1
;	print,conf(*,j+[0,nl])
;endfor
;print,'conf_nl is done'
end
	