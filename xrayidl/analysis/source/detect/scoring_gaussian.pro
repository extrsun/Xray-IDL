pro scoring_gaussian,xx,ff,ffd,ffdd
common data,ii,jj,modelc
na=n_params() 
ffdk=fltarr(modelc(1),n_elements(xx))
case modelc(0) of 
  1: begin
	ssigma=(modelc(n_elements(modelc)-1))^2
	rcs=modelc(n_elements(modelc)-2)^2
	areac=1.-exp(-rcs/(2.*ssigma))
	iid=ii-xx(0)
	jjd=jj-xx(1)
	e=(iid^2+jjd^2)/(2.*ssigma)
	nb=modelc(2)/modelc(1)
	psf=(1.-nb)*exp(-e)/(2.*!pi*ssigma)/areac
	rr=nb/(!pi*rcs)+psf
	ff=-total(alog(rr))
	rr=psf/rr

	if na gt 2 then begin ;getting derivatives and information matrix
	 ffdk(*,0)=rr*iid/ssigma
	 ffdk(*,1)=rr*jjd/ssigma
	 ffdk=-ffdk
	 ffd=total(ffdk,1)
	 ffdd=ffdk##transpose(ffdk)
	endif
     end
  2: begin
	ssigma=(xx(2))^2
	rcs=modelc(n_elements(modelc)-1)^2
	areac=1.-exp(-rcs/(2.*ssigma))
	iid=ii-xx(0)
	jjd=jj-xx(1)
	e=(iid^2+jjd^2)/(2.*ssigma)
	nb=modelc(2)/modelc(1)
	psf=(1.-nb)*exp(-e)/(2.*!pi*ssigma)/areac
	rr=nb/(!pi*rcs)+psf
	ff=-total(alog(rr))
	rr=psf/rr

	if na gt 2 then begin ;getting derivatives and information matrix
	 ffdk(*,0)=rr*iid/ssigma
	 ffdk(*,1)=rr*jjd/ssigma
	 ffdk(*,2)=((1.-areac)*rcs/(2.*ssigma)/areac-2.+2*e)*rr/xx(2)
	 ffdk=-ffdk
	 ffd=total(ffdk,1)
	 ffdd=ffdk##transpose(ffdk)
	endif
     end
endcase
if !debug eq 1 then stop
return
end
