pro mle_s,list,backc,re,para,xo=xo,yo=yo,sigma=sigma,chn=chn $
,verb=verb,ffdd=ffdd,status=status
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
; chn - if 2, four parameters, cntr and sigma as well as y positions are
;		fitted 
; xo, yo - initial source pixel position (e.g., SASS pixel position)
;	def = median position of counts in the source region
;*OUTPUTS:
;  para - vector containing cntr,cntre, and fitting parameters
; ffdd - containing info about the errors of the parameters.
;
;*MODIFICATION HISTORY:
; writen by WQD, April, 15, 1996
; understanding why the program tends to underestimate the size of the gaussian
; Statistically, a realization can natually produce sharp spikes. The MLE
; tends to fit such a spike with a gaussian which is smaller than the 
; actual distribution.
; WQD 9/15/97: The estimated pixel values are first subtracted from 
; the count positions for a better scaling.
;	Add chn=1 for fitting x and y position only.
;+
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - mle_s,list,backc,re,para,xo=xo,yo=yo,sigma=sigma'
print,',chn=chn,verb=verb,ffdd=ffdd,status=status'
return
endif
common data,ii,jj,modelc
; 
if n_elements(sigma) eq 0 then begin 
	if !instr eq 'p' then sigma=26. else sigma=6. ;unites of pixels
endif
ii=list.x
jj=list.y
tc=n_elements(ii)
if n_elements(chn) eq 0 then choice=2 else choice=chn
if n_elements(xo) eq 0 then xo=median(ii) ;initial position
if n_elements(yo) eq 0 then yo=median(jj)
;--------------------------------------
scale=sigma
ii=(ii-xo)/scale
jj=(jj-yo)/scale
res=re/scale
case choice of
 1: begin
  	modelc=float([choice,tc,backc,res,sigma/scale])
	para=[0,0]
    end
 2: begin
  	modelc=float([choice,tc,backc,res])
	para=[0,0,1.]
    end
endcase
;ptol=1.e-4
;if keyword_set(verb) ne 0 then print,'po =',p
proname='scoring_gaussian'
scoring,para,proname,fmin,ffdd=ffdd,status=status

para=para*scale
para(0)=para(0)+xo
para(1)=para(1)+yo
ffdd=ffdd*scale^2
if keyword_set(verb) ne 0 then print,'para =',para
if !debug eq 1 then stop,'stop at the end of MLE'
return
end
