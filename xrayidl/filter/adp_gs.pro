pro adp_gs,im,ims,imse,imsb,gsimsz=gsimsz,npo=npo,npd=npd,expt=expt,back=back $
,filter=filter,th1=th1,nimax=nimax,bn=bn,bmax=bmax,efth=efth,refback=refback
;+
; perform a multiscale gaussian smooth
;
;*CALLING SEQUENCE:
;     adp_gs,im,ims,gsimsz,npo=npo,npd=npd,expt=expt,filter=filter,th1=th1 
;	,nimax=nimax,bn=bn,bmax=bmax
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
;	bn - background normalization used in background subtraction
;		used for including background error (def=0.)
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
print,'Calling SEQ - adp_gs,im,ims,imse,imsb,gsimsz=gsimsz,npo=npo,npd=npd'
print,',expt=expt,back=back,filter=filter,th1=th1,nimax=nimax'
return
endif
if n_elements(bn) eq 0 then bn=0.
if n_elements(nimax) eq 0 then nimax=10000
if n_elements(th1) eq 0 then th1=10
if n_elements(gsimsz) eq 0 then gsimsz=60*2+1
if n_elements(npd) eq 0 then npd=0.1
if n_elements(npo) eq 0 then npo=2.
if n_elements(expt) eq 0 then begin
	choice=1 
endif else begin
	expt=expt > 0.
	choice=2
endelse
sz=size(im)
if n_elements(filter) eq 0 then begin
	sel=lindgen(sz(1)*sz(2))
	nsel=sz(4)
endif else sel=where(filter gt 0,nsel)
if n_elements(bmax) eq 0 then bmax=min(sz(1:2))/2

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
if n_elements(efth) eq 0 then efth=1./th1
while ni lt nimax and binsize lt bmax do begin
	print,'ni, binsize,nsel = ',ni,binsize,nsel
	wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
	imc=convolve(im,wlimg) 
        exptc=convolve(expt,wlimg) 
        flux=imdiv(imc,exptc)
        imssigma=sqrt(convolve(expt,wlimg^2)*flux  > 1.e-20)
        fton=imdiv(imc, imssigma)
;	if ni eq 0 then begin
;		if n_elements(back) eq 0 then $
;		backmin=median(imc)*0.1 > min(imc) else $
;		 backmin=(1.+bn^2)*back > avg(back(where(expt gt 0.)))*0.1
;	endif

	if np gt 2 then begin
		imssigma=convolve(imdiv(im+backmin,expt),wlimg^2) 
		if np gt 3 then imb=convolve(backmin,wlimg)
	endif else begin
;		imssigma=(imc+backmin)/(1.50*binsize)^2  > 1.e-20
		imssigma=(imc+backmin)/(1.70*binsize)^2  > 1.e-20
		if choice eq 2 then imssigma=imdiv(imssigma,expt)
        endelse
        eimc=sqrt(imssigma > 1.e-20)
	fton=imdiv(imc,eimc) 
	c=where(fton(sel) gt th1 or eimc(sel) lt efth*refback(sel),nc)
	if nc ne 0 then begin
		selc=sel(c)
		;ims(selc)=imc(selc)
                ims(selc)=flux(selc)
		if np gt 2 then imse(selc)=imssigma(selc)
		if np gt 3 then imsb(selc)=imb(selc)
		if nc eq nsel then goto,done
		remove,c,sel
		nsel=nsel-nc
	endif
ni=ni+1
binsize=binsize*(1.+npd)
if !debug eq 1 then stop
if nsel lt nselo*0.1 then goto,done
endwhile
done:
ims(sel)=imc(sel)
if np gt 2 then imse(sel)=imssigma(sel)
if np gt 3 then imsb(sel)=imb(sel)
ims=total(im)/total(ims)*ims 
;normalized it back to im because of biasing in the smoothing 
; (about 10% increasing the intensity)
return
end
