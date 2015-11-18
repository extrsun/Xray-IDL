pro mle_s,list,backc,re,para,xo=xo,yo=yo,sigma=sigma,chn=chn,verb=verb,ffdd=ffdd
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;-
;*PURPOSE:
; Conduct Maximum Likelihood fitting of count distribution 
;
;*CALLING SEQUENCE:
; mle,list,dim,para,imin=imin,jmin=jmin,filter=filter 
;	,block=block,sigma=sigma,chn=chn,cntrfac=cntrfac,xo=xo,yo=yo
;
;*PARAMETERS:
; INPUTS:
; list - count list
; backc - background counts
; re - the effective radius of the imaging region (in units of pixel)
; sigma - the size of gaussian used in the fit (def = 26 for PSPC and
;		= 6 for HRI pixels)
; chn - if 2, x, y, and sigma well be fitted.
; xo, yo - initial source pixel position (e.g., SASS pixel position)
;	def = median position of counts in the source region
;
;*OUTPUTS:
;  para - vector containing cntr,cntre, and fitting parameters
;  ffdd - the inversed information matrix for calculating errors.
;*PROCEDURE:
; scoring is used for the fitting
;
;*EXAMPLES:
; mle,l,200,fil=fs,inf='test.dat',xc=4096.5+xp,yc=4596.5+yp
;*RESTRICTIONS:
; works only for well defined sources, nr_powell could fail to converge
;
;*MODIFICATION HISTORY:
; writen by WQD, April, 15, 1996
; understanding why the program tends to underestimate the size of the gaussian
; Statistically, a realization can natually produce sharp spikes. The MLM
; tends to fit such a spike with a gaussian which is smaller than the 
; actual distribution.
;+
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - mle_s,list,backc,re,para,xo=xo,yo=yo,sigma=sigma,chn=chn,verb=verb,ffdd=ffdd'
return
endif
common data,ii,jj,dc,modelc
; 
if n_elements(sigma) eq 0 then begin 
	if !instr eq 'p' then sigma=26. else sigma=6. ;unites of pixels
endif
ii=list.x
jj=list.y

if n_elements(chn) eq 0 then choice=2 else choice=chn
if n_elements(xo) eq 0 then xo=median(ii) ;initial position
if n_elements(yo) eq 0 then yo=median(jj)
tc=n_elements(ii)
;--------------------------------------
scale=sigma
ii=ii/scale
jj=jj/scale
case choice of
 2: begin
	res=re/scale
  	modelc=[choice,backc/(res^2*!pi),res] 
	dc=tc 			;data constant
	para=[xo,yo,sigma]/scale
    end
endcase
ptol=1.e-4
if keyword_set(verb) ne 0 then print,'po =',p
proname='scoring_gaussian'
scoring,para,proname,fmin,ffdd=ffdd
para=para*scale
ffdd=ffdd*scale
if keyword_set(verb) ne 0 then print,'para =',para
if !debug eq 1 then stop,'stop at the end of MLE'
return
end
;=============================================================
pro scoring,para,proname,fmin,ffdd=ffdd,itmax=itmax,double=double,ptol=ptol $
,stpmax=stpmax
;+
; Minimization with the scoring method and backtracking algorithm
; para - as input, initial parameter values; as output, parameter values
; 	which give the minimum function 
; proname - the procedure name for evaluating function value, derivatives,
;	and information matrix.
; fmin - output minimum function value
; double - if set, double precision for inverting the information matrix
; stpmax - the maximum step (def stpmax=0.2).
; ptol - parameter convergence requirement: the maximum fraction of change
;	of the parameter values in an iteration (def=10^-7).
; ffdd - the inverse of the information matrix.
;
;*CAUTION:
; para should be carefully normalized to have their values of order unit;
; otherwise, the stpmax needs to be selected carefully.
;
; implemented according to the methods described by C. Sarazin (1980, ApJ
; 236, 75) and by Press et al. (Numerical Recipes, section 9.7).
;
; written by wqd, 9/8/96
;-
if n_elements(stpmax) eq 0 then stpmax=0.2 
if n_elements(itmax) eq 0 then itmax=100
if n_elements(double) eq 0 then double=0
for k=1,itmax do begin
	parao=para
	call_procedure,proname,parao,ff,ffd,ffdd
	ffdd=invert(ffdd,status,double=double)
	if status eq 1 then stop,'singular array: ffdd inverse'
	if status eq 2 then print,'small pivot element: ffdd inverse'
	dpara=-ffd##ffdd
	lnsrch,parao,ff,ffd,dpara,para,fmin,stpmax,check,proname,tolx=ptol 
		;backtracking
	if check eq 1 then return
	;print,'para,ff,ffd,ffdd,fmin ',para,ff,ffd,ffdd,fmin
	if !debug eq 1 then stop,'stop inside scoring loop'
endfor
stop,'maximum itmax exceeded'
return
end
;=============================================================
pro scoring_gaussian,xx,ff,ffd,ffdd
common data,ii,jj,dc,modelc
na=n_params() 
ffdk=fltarr(dc(0),n_elements(xx))
case modelc(0) of 
  2: begin
	ssigma=(xx(2))^2
	rcs=modelc(n_elements(modelc)-1)^2
	areac=1.-exp(-rcs/(2.*ssigma))
	iid=ii-xx(0)
	jjd=jj-xx(1)
	e=(iid^2+jjd^2)/(2.*ssigma)
	psf=(dc(0)-!pi*rcs*modelc(1))*exp(-e)/(2.*!pi*ssigma)/areac
	r=psf/(modelc(1)+psf)
	ff=-total(alog(modelc(1)+psf))

	if na gt 2 then begin ;getting derivatives and information matrix
	 ffdk(*,0)=r*iid/ssigma
	 ffdk(*,1)=r*jjd/ssigma
	 ffdk(*,2)=((1.-areac)*rcs/(2.*ssigma)/areac-2.+2*e)*r/xx(2)
	 ffdk=-ffdk
	 ffd=total(ffdk,1)
	 ffdd=ffdk##transpose(ffdk)
	endif
     end
endcase
if !debug eq 1 then stop
return
end