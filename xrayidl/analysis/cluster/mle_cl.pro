

;======================================
pro mle_cl,para,choice=choice,verb=verb
;
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() lt 1 then begin
	print,'CALLING SEQUENCE - '
	return
endif
case choice of 
 1: xi=transpose([[1.,0.,0.],[0.,1.,0.],[0.,0.,1.]]) 
 2: xi=transpose([[1.,0.],[0.,1.]])
endcase
;initial direction
func='func_powell'
ftol=1.e-4
nr_powell,para,xi,ftol,fmin,func,itmax=400
return
end

;=============================================================
function func_powell,x
;-
; ssigma - square of the sigma in units of pixel
;+
common powell_s,locs,choice,modelc,imagec,psfim,psfft

	ncomp=n_elements(x)
	calcl,imagec(0),x(1),clim,binrat=imagec(1),modelp=x
	clim=convolve(clim,psfim,ft=psfft)
	c=total(alog(modelc(0)+x(ncomp-1)*clim(loc)))
	return,2*(x(ncomp-1)*modelc(ncomp-1)-c)
end
;======================================
;+
; program called by calcl.pro
;-
pro cal_model,xx,wlloc,modelp=modelp
wlloc=(1.+xx^2)*(-3.*modelp(0)+0.5)
return
end
;======================================