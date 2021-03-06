pro adp_gs,im,ims,imse,imsb,gsimsz=gsimsz,npo=npo,npd=npd,expt=expt,back=back $
,filter=filter,th1=th1,nimax=nimax
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
np=n_params ()
if np eq 0 then begin
print,'Calling SEQ - adp_gs,im,ims,gsimsz,npo=npo,npd=npd,expt=expt,filter=filter,th2=th2,back=back'
print,',th1=th1,nimax=nimax'
return
endif
if n_elements(nimax) eq 0 then nimax=200
if n_elements(th1) eq 0 then th1=10
if n_elements(gsimsz) eq 0 then gsimsz=60*2+1
if n_elements(npd) eq 0 then npd=0.1
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
binsize=npo
ims=im*0.

if np gt 2 then imse=ims
if np gt 3 then begin
	imsb=ims
	if n_elements(back) eq 0 then begin
		print,'back image is needed'
		return
	endif
endif
ni=0
nselo=nsel

while ni lt nimax do begin
	print,'ni, binsize,nsel = ',ni,binsize,nsel
	wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
	imc=convolve(im,wlimg) 

	if ni eq 0 then begin
		if n_elements(back) eq 0 then $
		backmin=median(imc)*0.1 > min(imc) else backmin=back
		;0.1 is an arbitrarily chosen minimum 'real' background
	endif

	if np gt 2 then begin
		imssigma=convolve(im > backmin,wlimg^2) 
		if np gt 3 then imb=convolve(back,wlimg)
	endif else imssigma=(imc > backmin)/(1.50*binsize)^2 

	if choice eq 2 then begin
		imssigma=sqrt(imdiv(imssigma,expt) > 1.e-20) ;to get c/(t*t)
		fton=imdiv(imc-imb,imssigma) 
			;to avoid problems with imc \sim 0.
	endif else fton=imdiv(imc,sqrt(imssigma > 1.e-20))

	c=where(fton(sel) gt th1,nc)
	if nc ne 0 then begin
		selc=sel(c)
		ims(selc)=imc(selc)
		if np gt 2 then imse(selc)=imssigma(selc)
		if np gt 3 then imsb(selc)=imb(selc)
		if nc eq nsel then goto,done
		remove,c,sel
		nsel=nsel-nc
		ni=ni+1
	endif
	
binsize=binsize*(1.+npd)
if !debug eq 1 then stop
imr=imdiv((imc-imb),imb)
imth=0.5
if max(imr(sel)) lt imth then goto,done
if nsel lt nselo*0.5 then goto,done
endwhile
done:
;ims(sel)=imc(sel)
;if np gt 2 then imse(sel)=imssigma(sel)
;if np gt 3 then imsb(sel)=imb(sel)
return
end