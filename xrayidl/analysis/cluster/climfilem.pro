pro cl_imc,dim,para,clim,binrat=binrat,psfim=psfim,psfft=psfft,hpsfdim=hpsfdim
;+
; calculate a PSF convolved cluster model image
;-
if n_params() eq 0 then begin 
 print,'cl_imc,dim,para,clim,binrat=binrat,psfim=psfim,psfft=psfft'
 print,',hpsfdim=hpsfdim'
 return
endif
if n_elements(binrat) eq 0 then binrat=1
 calcl,dim+2*hpsfdim,para(1),clim,binrat=binrat,modelp=para,chatt=0
if !debug eq 2 then stop,'before convolution'
if hpsfdim ne 0 then begin
	clim=convlv(clim,psfim,psfft=psfft)
if !debug eq 2 then stop
	if n_elements(hpsfdim) ne 0 then $
		clim=clim(hpsfdim:dim-1+hpsfdim,hpsfdim:dim-1+hpsfdim)
endif
;if !debug eq 2 then stop,'at the buttom of cl_imc'
return
end
;==================================
;+
; program called by calcl.pro
;-
pro cal_model,xx,wlloc,modelp=modelp
modelp(0)=modelp(0) 
wlloc=(1.+xx^2)^(-3.*modelp(0)+0.5)
return
end
;======================================