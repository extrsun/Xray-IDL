pro adp_gs,im,ims,gsimsz,npo=npo,npd=npd,expt=expt,filter=filter,th1=th1 $
	,nimax=nimax,th2=th2,back=back
;-
; perform a multiscale gaussian smooth
;
;*CALLING SEQUENCE:
;     adp_gs,im,ims,gsimsz,npo=npo,npd=npd,expt=expt,filter=filter,th1=th1 
;	,nimax=nimax
;
;*PARAMETERS:
; INPUTS:
;	im - input image for smoothing
;	
; OUTPUTS:
;	ims - smoothed image
;
;*OPTIONAL IMPUTS or OUTPUTS:
;	th1 - the limit selected for calculating the flux-to-noise ratio
;		of the image
;	expt - the exposure of the input image (to get the varience
;		map count/expt^2), if the imput image is a FLUX map
;	back - background FLUX map to be added to the flux map, im for
;		calculating dispersions with pixel size same as that of
;		im. the image IM should now be a background subtracted 
;		FLUX map.
;       gsimsz   - gaussian image size in units of bins 
;       np - Size of the imput array bin 
;	npo - the initial size of npd
; 	
;*PROCEDURE:
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;   covolve
;   gsf_gaussian
;
;*MODIFICATION HISTORY:
;    written  by QDW 2/20/95
;-
;------------------------------------------------------------------------------
if n_params () eq 0 then begin
print,'Calling SEQ - adp_gs,im,ims,gsimsz,npo=npo,npd=npd,expt=expt,filter=filter'
print,',th1=th1,nimax=nimax'
return
endif
if n_elements(nimax) eq 0 then nimax=25
if n_elements(th1) eq 0 then th1=10
if n_elements(gsimsz) eq 0 then gsimsz=60*2+1
if n_elements(npd) eq 0 then npd=1
if n_elements(npo) eq 0 then npo=2.
if n_elements(expt) eq 0 then begin
	choice=1
;	expt=im
endif else choice=2
if n_elements(filter) eq 0 then begin
	sz=size(im)
	sel=lindgen(sz(1)*sz(2))
	nsel=sz(4)
endif else sel=where(filter gt 0,nsel)
nback=n_elements(back)
if n_elements(th2) eq 0 then th2=th1*0.7
binsize=npo
ims=im*0.
ni=0
nsel2=0
nselo=nsel
while ni lt nimax do begin
	print,'ni, binsize,nsel = ',ni,binsize,nsel
	wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
	imc=convolve(im,wlimg) 
	imc2=convolve(im2,wlimg) 
	if nback ne 0 then begin
		imc=imc+back & imc2=imc2+back
	endif
	imssigma=imc/(1.50*binsize)^2 
	imssigma2=imc2/(1.50*binsize)^2 
	if choice eq 2 then begin
		imssigma=(imc*imc2/(imc+imc2))^2*(1./(
		imssigma=imdiv(imssigma,expt) ;to get c/(t*t)
		imssigma=imdiv(imssigma2,expt2) ;to get c/(t*t)

	fton=imdiv(imc,sqrt(imssigma))
	c=where(fton(sel) gt th1,nc)
	if nc ne 0 then begin
		selc=sel(c)
		ims(selc)=imc(selc)
		if nc eq nsel then goto,done
		remove,c,sel
		nsel=nsel-nc
		ni=ni+1
	endif
       if th1 ne th2 then begin
	if nsel2 ne 0 then begin
	  c=where(fton(sel2) gt th1,nc)
	  if nc ne 0 then begin	
		sel2c=sel2(c)
		ims(sel2c)=(fton(sel2c)*imc(sel2c)+ftonsel(c) $
			*ims(sel2c))/(ftonsel(c)+fton(sel2c))
	  endif
	endif
	if ni ne 0 then begin 
	  if nsel2 ne 0 then begin
		c2=where(fton(sel) gt th12,nsel2)
		sel2=sel(c2)
		ftonsel=fton(sel2)
		ims(sel2)=imc(sel2)
	  endif
	endif 
       endif		
binsize=binsize+npd
;tv,bscale(alog10(ims > 0.1),-1,0.9),ni-1
if !debug eq 1 then stop
if nsel lt nselo*0.05 then goto,done
endwhile
done:
ims(sel)=imc(sel)
if nback ne 0 then ims=ims-back
return
end