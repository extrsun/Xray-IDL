pro adp_gs,im,ims,gsimsz,npo=npo,npd=npd,ims2=ims2,filter=filter,th1=th1 $
	,nimax=nimax,th2=th2
;-
; perform a multiscale gaussian smooth
;
;*CALLING SEQUENCE:
;     adp_gs,im,ims,gsimsz,npo=npo,npd=npd,ims2=ims2,filter=filter,th1=th1 
;	,nimax=nimax
;
;*PARAMETERS:
; INPUTS:
;	im - input image for wavelet transform
;	
; OUTPUTS:
;	ims - smoothed image
;
;*OPTIONAL IMPUTS or OUTPUTS:
;	th1 - the limit selected for calculating the flux-to-noise ratio
;		of the image
;	ims2 - the variance of the input image (i.e., count/time^2). If
;		im is not a count image, it is neccessary to input ims2!!!
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
print,'Calling SEQ - adp_gs,im,ims,gsimsz,npo=npo,npd=npd,ims2=ims2,filter=filter'
print,',th1=th1,nimax=nimax'
return
endif
if n_elements(nimax) eq 0 then nimax=25
if n_elements(th1) eq 0 then th1=10
if n_elements(gsimsz) eq 0 then gsimsz=60*2+1
if n_elements(npd) eq 0 then npd=1
if n_elements(npo) eq 0 then npo=2.
if n_elements(ims2) eq 0 then begin
	choice=1
	ims2=im
endif else choice=2
if n_elements(filter) eq 0 then begin
	sz=size(im)
	sel=lindgen(sz(1)*sz(2))
	nsel=sz(4)
endif else sel=where(filter gt 0,nsel)
if n_elements(th12) eq 0 then th12=th1*0.7
binsize=npo
ims=im*0.
ni=0
nsel2=0
avgim=avg(im)
while ni lt nimax do begin
	print,'ni, binsize,nsel = ',ni,binsize,nsel
	wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
	imc=convolve(im,wlimg) ;> 0.001*avgim
	if choice eq 1 then begin
		imssigma=(imc > (0.001*avgim)^2)/(1.50*binsize)^2
	endif else begin
		imssigma=convolve(ims2,wlimg*wlimg) 
		c=where(imssigma le 0.,nc) ;due to alias effect of FFT
		if ni eq 0 then maxss=max(imssigma)
		if nc ne 0 then imssigma(c)=maxss
	endelse
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
       if th1 ne th12 then begin
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
endwhile
done:
ims(sel)=imc(sel)
return
end