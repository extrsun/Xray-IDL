pro mle_s,list,backc,barea,para,xo=xo,yo=yo,sigma=sigma,chn=chn $
,verb=verb,tcs=tcs,ncs=ncs
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
;  list - count list
; backc, barea - background counts and pixel number
; sigma - the size of gaussian used in the fit (def = 26 for PSPC and
;		= 6 for HRI pixels)
; chn - if 2, four parameters, cntr and sigma as well as y positions are
;		fitted 
; xo, yo - initial source pixel position (e.g., SASS pixel position)
;	def = median position of counts in the source region
;*OUTPUTS:
;  para - vector containing cntr,cntre, and fitting parameters
;
;*PROCEDURE:
; call nr_powell is used for the fitting
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
print,'CALLING SEQUENCE - mle_s,list,backc,barea,para,xo=xo,yo=yo'
print,',sigma=sigma,chn=chn,verb=verb,tcs=tcs,ncs=ncs'
return
endif
common powell_s,modelc,i,j,choice,ncso,np
; 
if n_elements(sigma) eq 0 then begin 
	if !instr eq 'p' then sigma=26. else sigma=6. ;unites of pixels
endif
i=list.x
j=list.y

if n_elements(chn) eq 0 then choice=2 else choice=chn
if n_elements(xo) eq 0 then xo=median(i) ;initial position
if n_elements(yo) eq 0 then yo=median(j)
ncso=n_elements(i)
if n_elements(ncs) eq 0 then ncs=ncso 
if n_elements(tcs) eq 0 then tcs=ncs/0.9 
;--------------------------------------
case choice of 
 1: begin
  modelc=[backc/barea,tcs,sigma^2]
  p=[xo,yo]
    end
 2: begin
;  modelc=[backc/barea,tcs]
  modelc=[backc/barea,tcs,sqrt(barea/!pi)*0.5]
  p=[xo,yo,sigma]
    end
 3: begin
  modelc=[backc/barea,1.] ;0.9 is the ratio of counts within the aperture
			   ;relative to the total (only an approximation)!
  p=[xo,yo,sigma,tcs]
    end
endcase
np=n_elements(p)
;xi=fltarr(np,np)
;for k=0,np-1 do xi(k,k)=p(k)*0.1

;initial direction
;func='func_powell'
func='func_powell'
dfunc='dfunc_powell'
ftol=1.e-8
if keyword_set(verb) ne 0 then print,'po =',p
;nr_powell,p,xi,ftol,fmin,func,itmax=400
dfpmin,p,ftol,fmin,func,dfunc,itmax=1000
;if choice ge 2 then p(2)=p(2))
if keyword_set(verb) ne 0 then print,'para =',p
para=p
if !debug eq 1 then stop,'stop at the end of MLE'
return
end
;=============================================================
function func_powell,x
;-
; ssigma - square of the sigma in units of pixel
;+
common powell_s,modelc,i,j,choice,ncso,np

common func,e,sf,ssigma
if choice eq 1 then ssigma=modelc(2) else ssigma=x(2)^2
if choice eq 3 then ss=x(3) else ss=modelc(1)
areac=1.-exp(-modelc(n_elements(modelc)-1)^2/(2.*ssigma))
e=((i-x(0))^2+(j-x(1))^2)/(2.*ssigma)
sf=ss*exp(-e)/(2.*!pi*ssigma)/areac

case choice of 
  1: begin
;	psf=exp(-((i-x(0))^2+(j-x(1))^2)/(2.*modelc(2)))/(2.*!pi*modelc(2))
;	c=total(alog(modelc(0)+modelc(1)*psf))
	c=total(alog(modelc(0)+sf))
	return,-2*c
     end
  2: begin
;	psf=exp(-((i-x(0))^2+(j-x(1))^2)/(2.*ssigma))/(2.*!pi*ssigma)
;	c=total(alog(modelc(0)+modelc(1)*psf/areac))
	c=total(alog(modelc(0)+sf))
	return,-2*c
     end
  3: begin
;	psf=exp(-((i-x(0))^2+(j-x(1))^2)/(2.*ssigma))/(2.*!pi*ssigma)
	c=total(alog(modelc(0)+sf))
	return,2*(x(3)*modelc(1)-c)
     end
endcase
end
;=============================================================
function dfunc_powell,x
;-
; ssigma - square of the sigma in units of pixel
;+
common powell_s,modelc,i,j,choice,ncso,np
;stop,'2'
common func,e,sf,ssigma

r=sf/(modelc(0)+sf)
dc=fltarr(ncso,np)
case choice of 
  1: begin
;	e=((i-x(0))^2+(j-x(1))^2)/(2.*modelc(2))
;	sf=modelc(1)*exp(-e)/(2.*!pi*modelc(2))
	dc(0)=[(i-x(0)),(j-x(1))]/ssigma
	dc=total(dc##transpose(r),1)
     end
  2: begin
;	r=sf/(modelc(0)+sf)
	dc(0)=[[(i-x(0)),(j-x(1)),2*(e-1)*x(2)]/ssigma]
	dc=total(dc##transpose(r),1)
     end
  3: begin
;	e=((i-x(0))^2+(j-x(1))^2)/(2.*ssigma)
;	sf=x(3)*exp(-e)/(2.*!pi*ssigma)/areac
;	r=sf/(modelc(0)+sf)
	dc(0)=[[(i-x(0)),(j-x(1)),2*(e-1)*x(2)]/ssigma,replicate(1./x(3),ncso)]
	dc=total(dc##transpose(r),1)
	dc(np-1)=dc(np-1)-modelc(1)
     end
endcase
return,-2.*dc
end