pro mle_king,list,backc,barea,para,xo=xo,yo=yo,sigma=sigma,chn=chn $
,verb=verb,tcs=tcs,bet=bet,ipara=ipara,fmin=fmin
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
;--------------------------------------
case choice of 
 1: begin
  if n_elements(ipara) ne 0 then p=ipara else begin
  	if n_elements(bet) eq 0 then bet=0.7
  	if n_elements(tcs) eq 0 then $
  	tcs=(ncso-backc)/(!pi*sigma^2*beta(1.,3*bet-3./2.))
  	p=[xo,yo,bet,sigma,tcs,backc/barea]
  endelse
	modelc=[barea]
    end
 2: begin
  if n_elements(ipara) ne 0 then p=ipara else begin
  	if n_elements(bet) eq 0 then bet=0.7
  	if n_elements(tcs) eq 0 then $
  	tcs=(ncso-backc)/(!pi*sigma^2*beta(1.,3*bet-3./2.))
	p=[xo,yo,bet,sigma,tcs]
  endelse
  	modelc=[backc/barea,barea]
    end
endcase
print,'initial para =',p
np=n_elements(p)

func='func_powell'
dfunc='dfunc_powell'
ftol=1.e-7
if keyword_set(verb) ne 0 then print,'po =',p
xi=fltarr(np,np)
for k=0,np-1 do xi(k,k)=p(k)*0.1
nr_powell,p,xi,ftol,fmin,func,itmax=400,/double
;dfpmin,p,ftol,fmin,func,dfunc,itmax=1000,stepmax=0.001
;if choice ge 2 then p(2)=p(2))
if keyword_set(verb) ne 0 then print,'p =',p,fmin
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
x=x > 0.
xx3=x(3)^2
rr=(i-x(0))^2+(j-x(1))^2
inside=1+rr/xx3
ee=(-3.*x(2)+0.5)
ss=x(4)*inside^ee

barea=modelc(n_elements(modelc)-1)
rrc=barea/!pi
yc=rrc/(rrc+xx3)
ee1=3.*x(2)-3./2. ;> 1.e-10
if ee1 le 0. then bfac=1.e10 else $
bfac=beta(1,ee1)*ibeta(1.,ee1,yc)
tss=!pi*xx3*bfac*x(4)
case choice of 
  1: dc=tss+barea*x(5)-total(alog(x(5)+ss))
  2: dc=tss-total(alog(modelc(0)+ss))
endcase
;print,x,dc
if !debug eq 2 then stop
return,dc
end
;=============================================================
function dfunc_powell,x
;-
; ssigma - square of the sigma in units of pixel
;+
common powell_s,modelc,i,j,choice,ncso,np
;x=x > 0.
dc=fltarr(ncso,np)

xx3=x(3)^2
rr=(i-x(0))^2+(j-x(1))^2
inside=1+rr/xx3
yy=rr/(xx3*inside)
ee=(-3.*x(2)+0.5)
ss=x(4)*inside^ee

barea=modelc(n_elements(modelc)-1)
rrc=barea/!pi
yc=rrc/(rrc+xx3)
ee1=3.*x(2)-3./2. ;> 1.e-10
if ee1 le 0. then bfac=1.e10 else $
bfac=beta(1,ee1)*ibeta(1.,ee1,yc)
tss=!pi*xx3*bfac*x(4)
b2=(3*x(2)-5./2.)
b3=(3*x(2)-3./2.)
case choice of 
  1: begin
	dc(0)=[fac*(i-x(0)),fac*(j-x(1)),3.*alog(inside),2./x(3)*ee*yy $
	,replicate(-1.,ncso)/x(4),-1./ss]
if !debug eq 2 then stop
	dc=total(dc##transpose(ss/(x(5)+ss)),1)
	dc=dc+[0.,0.,3*tss/b3*(1.+(1-yc)^b3*alog(1-yc)/bfac),2*tss/x(3)*b2* $
	 (1.-(1.-yc)^b2*yc/bfac/(1.+rrc^2/xx3)),tss/x(4),barea]
     end
  2: begin
	dc(0)=[fac*(i-x(0)),fac*(j-x(1)),3.*alog(inside),2./x(3)*ee*yy $
	,replicate(-1.,ncso)/x(4)]
if !debug eq 2 then stop
	dc=total(dc##transpose(ss/(modelc(0)+ss)),1)
	dc=dc+[0.,0.,3*tss/b3*(1.+(1-yc)^b3*alog(1-yc)/bfac),2*tss/x(3)*b2* $
	 (1.-(1.-yc)^b2*yc/bfac/(1.+rrc^2/xx3)),tss/x(4)]
     end
endcase
ss=where(x le 0,ns)
if ns ne 0 then stop
if !debug eq 2 then stop,'at the end'
return,dc
end