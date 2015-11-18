pro mle_cl_main,loc,dim,bflux,para,chn=chn,block=block,verb=verb $
,binrat=binrat,offaxis=offaxis,hpsfdim=hpsfdim,clim=clim,imonly=imonly,cstat=cstat,so=so
;+
; maximum likelihood fit of the King model to cluster data 
;
; loc - index location of counts within an image. The image should be
;	large enough to cover a substantial background regions.
; dim - dimension of the image
; bflux - background counts per bin of the image
; para - vector containing inital model parameter values as input
;	and the best-fit values as output.
; chn - choice selection: chn=1 --- the fit is to the three parameters,
;	beta, r_c (actually the bin size in units of r_c), and central 
;	counts per pixel of the model (which is not really independent);
;	chn=2 --- fit only beta and r_c (def=2).
; block - the block size of the image bin.
; binrat - the subdivision of the image for accurate model calculation
; offaxis - the offaxis of the cluster center in a rosat observation
;	used for PSF calculation
; hpsfdim - the half dimension of the PSF image to be calculated and to
;	be used to expand the initial model image on each side
; clim - output model image
; imonly - if set, no fit will be done, only the model image will be calculated
; cstat - the best-fit C statistic value
; so - the central counts per bin of the model for chn=2.
; 
;*procedure:
;	The Cash's C statistic is used for a 2-D fit to the data with
; assuming the circular King model. A model image is first calculated
; and then convolved with the PSF of the rosat PSPC observation.
;
; 
; written by wqd, 7/16/96
;-
if n_params() lt 1 then begin
 print,'CALLING SEQUENCE - mle_cl_main,'
 print,'ls,block=block,xmin=xmin,ymin=ymin (count image parameters)'
 print,',bflux,para,chn=chn,binrat=binrat,verb=verb (model parameters)'
 print,',offaxis=offaxis,hpsfdim=hpsfdim (PSF parameters)'
 return
endif

common powell_cl,locs,choice,modelc,imagec,psfim,psfft
	;common block to be shared by func_powell

if n_elements(chn) eq 0 then choice=2 else choice=chn ;for common block
if n_elements(filter) eq 0 then locs=loc else locs=where(filter(loc) ne 0.)
;---------------------------------
; get the psf
if n_elements(hpsfdim) eq 0 then begin
	psfdim=120/block
	print,' the half dimension of the psf is ',psfdim
endif
	psfdim=2*fix(hpsfdim)+1 ; make sure that the psf has an odd dim
get_psfim,psfim,dim=psfdim,block=block,binrat=7,offaxis=offaxis $
	,chlow=chlow,chhigh=chhigh
	;binrat = 7 should be enough for rosat images
;---------------------------------
; 
if n_elements(binrat) eq 0 then binrat = 1
imagec=[dim,binrat,hpsfdim]
if n_elements(block) eq 0 then block=10 
tc=n_elements(locs)-bflux*float(dim)^2 
case choice of 
 1: 	begin
;	modelc=[bflux] ; model parameters not to be changed in the fit
	modelc=[float(dim)^2] ;model parameters not to be changed in the fit
 	end
 2: 	begin
     	modelc=[bflux,tc]
	print,'the total cluster counts are fixed'
	end
 3: 	modelc=[bflux,tc] ;for the time being same as chn=2
endcase
;print,'modelc = ',modelc

if keyword_set(imonly) eq 0 then begin
	; finally the fit:
	mle_cl,para,choice=choice,verb=verb
	cstat=func_powell(para)
	print,'cstat, para = ',cstat,para
endif
cl_imc,dim,para,clim,binrat=binrat,psfim=psfim,psfft=psfft,hpsfdim=hpsfdim
case choice of 
 1: clim=clim*para(n_elements(para)-1)
 2: begin
	so=tc/total(clim)
	clim=clim*so
    end
 3: print,'no normalization of clim is done'
endcase
;print,'total observed cluster counts in the image = ',tc
return	
end

;======================================
pro mle_cl,para,choice=choice,verb=verb
;
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() lt 1 then begin
	print,'CALLING SEQUENCE - mle_cl,para,choice=choice,verb=verb'
	return
endif

; initial direction:
npara=n_elements(para)
xi=fltarr(npara,npara)
for k=0,npara-1 do xi(k,k)=para(k)*0.1

ftol=1.e-4
nr_powell,para,xi,ftol,fmin,'func_powell',itmax=400,iter=iter
print,'iter, fmin = ',iter,fmin
return
end

;=============================================================
function func_powell,xx
;+
;
;-
common powell_cl,locs,choice,modelc,imagec,psfim,psfft
; common shared with the main program
ncomp=n_elements(xx)
;construct a convolved model cluster image:
cl_imc,imagec(0),xx,clim,binrat=imagec(1),psfim=psfim,psfft=psfft $
 ,hpsfdim=imagec(2)
case choice of 
 1: 	begin 
	c=total(alog(xx(2)+xx(ncomp-1)*clim(locs)))
	tc=total(clim)*xx(ncomp-1)
		;integrated model counts per pixel, which cannot be replaced
		; be a constant (observed)
	cc=2*(xx(2)*modelc(0)+tc-c)
;	print,'model para, -c, tc, cc= ',xx,-c,tc,cc
	end

 2:	begin
	so=modelc(ncomp-1)/total(clim) ;central counts per pixel
	cc=-2*total(alog(modelc(0)+so*clim(locs)))
;	print,'model para, so, cc= ',xx,so,cc
	end
endcase
if !debug eq 1 then stop,'at the buttom of func_powell'
return,cc
end
;======================================
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