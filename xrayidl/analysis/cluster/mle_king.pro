;=============================================================
pro scoring_king,xx,ff,ffd,ffdd
;+
; xx - input variable vector
; ff - log likelihood: -alog(total(pp))
; ffd - derivative of the log likelihood
; ffdd - information matrix (see Sarazin 1980, ApJ, 236, 75)
;
;*Note: 
; the likelihood dose depend on the number of counts. 
; The area is normalized so that the plate area = 1 to remove the 
; dependance on spatial scaling.
;
; written by wqd, 9/10/96
;-
common data,ii,jj,modelc
common feedback,bfac,bfac2,rrsc,rcs

nc=modelc(1) 
np=n_elements(xx)
ffdk=fltarr(nc,np)
chn=modelc(0)
	aas=xx(3)^2	
	iid=ii-xx(0)
	jjd=jj-xx(1)		
	rraas=(iid^2+jjd^2)/aas
	inside=1.+rraas
	alpha=-3.*xx(2)+0.5

	rrsc=modelc(n_elements(modelc)-1)^2 ;the effective radius of the image
	yysc=rrsc/(rrsc+aas)

	ibetam,-(alpha+1.),yysc,bfac,bfacd

	if chn eq 2 then tb=xx(np-1) else tb=modelc(2)/float(nc)
	nc = 1.-tb
	if chn ge 3 then begin
		aas2=xx(6)^2
		iid2=ii-xx(4)
		jjd2=jj-xx(5)
		rraas2=(iid2^2+jjd2^2)/aas2
		if chn eq 3 then begin
			inside2=rrsc/(2*aas2)*exp(-rraas2*0.5) 
			ts=xx(7)*inside2
		endif else begin ;second beta model:
			if chn ne 5 then alpha2=-3.*xx(8)+0.5 else $
				alpha2=-3.*0.5+0.5
			if alpha2 eq -1. then begin
				rcs=(xx(0)-xx(4))^2+(xx(1)-xx(5))^2
				rrsc2=rrsc*(1.-0.25*rcs/rrsc)^2
			endif else  rrsc2=rrsc ;this is not completely correct
			yysc2=rrsc2/(rrsc2+aas2)
			ibetam,-(alpha2+1.),yysc2,bfac2,bfacd2
			inside2=1.+rraas2
			ts=xx(7)*rrsc/(aas2*bfac2)*inside2^alpha2
		endelse		
		nc=nc-xx(7)
	endif else ts=0.
	tc=nc*rrsc/(aas*bfac)*inside^alpha

	pp=tb+tc+ts
	ff=-total(alog(pp))
	
	if n_params() ge 2 then begin 
		;getting derivatives and information matrix
	 tcpp=tc/pp
         ffdk(*,0)=tcpp*iid/inside*(2.*alpha/aas)
	 ffdk(*,1)=tcpp*jjd/inside*(2.*alpha/aas)
	 ffdk(*,2)=tcpp*(alog(inside)*3.+bfacd/bfac)
	 ffdk(*,3)=tcpp*(1.+alpha*rraas/inside-(1.-yysc)^(-(alpha+2.)) $
		*yysc^2*(aas/rrsc/bfac))*(2./xx(3))
	 if chn eq 2 then ffdk(*,np-1)=(tc/nc-1.)/pp ;tb to be fitted

	 if chn eq 3 then begin
	 	tspp=ts/pp
	 	ffdk(*,4)=tspp*iid2*(-1./aas2)
	 	ffdk(*,5)=tspp*jjd2*(-1./aas2)
	 	ffdk(*,6)=tspp*(1.-rraas2*0.5)*(2./xx(6))
	 	ffdk(*,7)=(tc/nc-inside2)/pp
	 endif
	 if chn ge 4 then begin
	 	tspp=ts/pp
	 	ffdk(*,4)=tspp*iid2*(2.*alpha2/aas2)
	 	ffdk(*,5)=tspp*jjd2*(2.*alpha2/aas2)
	 	ffdk(*,6)=tspp*(1.+alpha2*rraas2/inside2- $
			(1.-yysc2)^(-(alpha2+2.)) $
			*yysc2^2*(aas2/rrsc2/bfac2))*(2./xx(6))
	 	ffdk(*,7)=(tc/nc-rrsc/(aas2*bfac2)*inside2^alpha2)/pp
		if chn ne 5 then $
			ffdk(*,8)=tspp*(alog(inside2)*3.+bfacd2/bfac2)
	 endif
	 ffd=total(ffdk,1)
	 ffdd=ffdk##transpose(ffdk)
	endif
if !debug eq 1 then stop
return
end
;==========================================
pro ibetam,alpha,yysc,bfac,bfacd
if alpha eq 0. then begin ;beta=0.5
	bfac=-alog(1-yysc) 
	bfacd=-0.5*(alog(1-yysc))^2 
endif else begin ;derivative of beta function relative to beta
	bfac=(1.-(1.-yysc)^alpha)/alpha
	bfacd=-3./alpha*(bfac+(1-yysc)^alpha*alog(1-yysc))
endelse
return
end
;=================================
pro mle_king,xx,yy,backc,re,para,xo=xo,yo=yo,bet=bet,r_core=r_core,ffdd=ffdd $
	,fmin=fmin,chn=chn,verb=verb,tc=tc,inpar=inpar
;+
; For a large area, one probably needs to consider vignetting effects!
; When the area is not big enough, the background level cannot be 
; adequately constrainted. It is better to be fixed with a 1-D fit.
; The results, beta value, obtained here are not particularly sensitive to the 
; the background level if the uncertainty of the level is <20% or so.
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - mle_king,list,backc,re,para,xo=xo,yo=yo'
print,',bet=bet,r_core=r_core,ffdd=ffdd,fmin=fmin,chn=chn,verb=verb'
return
endif
common data,ii,jj,modelc
ii=xx & jj=yy
; 
;set the initial parameters:
if n_elements(chn) eq 0 then chn=1 
if n_elements(inpar) eq 0 then begin
	if n_elements(xo) eq 0 then xo=median(ii) ;initial position
	if n_elements(yo) eq 0 then yo=median(jj)
	if n_elements(bet) eq 0 then bet=0.7
	if n_elements(r_core) eq 0 then begin 
	  if !instr eq 'p' then r_core=26. else r_core=6. ;unites of pixels
	endif
	case chn of
 		1: para=float([xo,yo,bet,r_core])
 		2: para=float([xo,yo,bet,r_core,backc])
		3: para=float([xo,yo,bet,r_core,xs,ys,sigma,ns])
	endcase
endif else para=inpar
print,'mle_king: initial para = ',para

;---------------------------------------------
; scale parameters and set model constants
if n_elements(tc) eq 0 then tc=n_elements(ii)
scale=para(3)
ii=ii/scale
jj=jj/scale
case chn of
 1: begin
  	modelc=float([chn,tc,backc,re/scale])
	scalev=[scale,scale,1.,scale]
	para=para/scalev
    end

 2: begin
  	modelc=float([chn,tc,re/scale])
	scalev=[scale,scale,1.,scale,tc]
	para=para/scalev
    end
 3: begin
  	modelc=float([chn,tc,re/scale])
	scalev=[scale,scale,1.,scale,scale,scale,scale,tc]
	para=para/scalev
    end
 else: begin
  	modelc=float([chn,tc,re/scale])
	scalev=[scale,scale,1.,scale,scale,scale,scale,tc,1.]
	para=para/scalev
    end
endcase

; ready for the fit:
ptol=1.e-4
proname='scoring_king'
scoring,para,proname,fmin,ffdd=ffdd

;scale back 
para=para*scalev 
np=n_elements(para)
for j=0,np-1 do begin 
for i=0,np-1 do begin 
	ffdd(i,j)=ffdd(i,j)*(scalev(i)*scalev(j)) ;not complete right yet
endfor & endfor

if keyword_set(verb) ne 0 then print,'para =',para
if !debug eq 1 then stop,'stop at the end of MLE'
return
end
