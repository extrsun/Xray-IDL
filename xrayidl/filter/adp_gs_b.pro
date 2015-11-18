pro adp_gs_b,im,expt,back,ims,imt,imb,gsimsz=gsimsz,npo=npo,npd=npd $
,filter=filter,th1=th1,nimax=nimax,bn=bn,bmax=bmax
;-
; perform a multiscale gaussian smooth for count, background, and exposure
; maps simultaneously.
; modified version of adp_gs.
;
;*CALLING SEQUENCE:
;     adp_gs,im,ims,gsimsz,npo=npo,npd=npd,expt=expt,filter=filter,th1=th1 
;	,nimax=nimax
;
;*PARAMETERS:
; INPUTS:
;	im - input image for smoothing
;	expt - the exposure of the input image (to get the varience
;		map count/expt^2), if the imput image is a FLUX map
;	back - background FLUX map to be added to the flux map, im for
;		calculating dispersions with pixel size same as that of
;		im. the image IM should now be a background subtracted 
;		FLUX map.
;	
; OUTPUTS:
;	ims - smoothed image
;	imt, imb - smoothed exposure and background maps
;
;*OPTIONAL IMPUTS or OUTPUTS:
;	bn - the normalization needed for the background subtraction
;	th1 - the limit selected for calculating the flux-to-noise ratio
;		of the image
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
;    written  by QDW 1/20/2001
;-
;------------------------------------------------------------------------------
np=n_params ()
if np eq 0 then begin
print,'Calling SEQ - adp_gs,im,ims,imse,imsb,gsimsz=gsimsz,npo=npo,npd=npd'
print,',expt=expt,back=back,filter=filter,th1=th1,nimax=nimax'
return
endif
if n_elements(nimax) eq 0 then nimax=1000
if n_elements(th1) eq 0 then th1=10
if n_elements(gsimsz) eq 0 then gsimsz=60*2+1
if n_elements(npd) eq 0 then npd=0.1
if n_elements(npo) eq 0 then npo=2.
sel=where(expt gt 0,nsel)
osel=sel
binsize=npo
ims=im*0.
imt=ims
imb=ims
ni=0
nselo=nsel
while ni lt nimax and binsize lt bmax do begin
	print,'ni, binsize,nsel = ',ni,binsize,nsel
	wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
	imc=convolve(im,wlimg) 
	ims2=sqrt(convolve(im,wlimg^2))
	ims2=ims2 > avg(ims2(sel))*0.1 ;minimum noise
	fton=imdiv(imc,ims2) 
	c=where(fton(sel) gt th1,nc)
	if nc ne 0 then begin
		selc=sel(c)
		ims(selc)=imc(selc)
		cexpt=convolve(expt,wlimg) 
		cback=convolve(back,wlimg) 
		imt(selc)=cexpt(selc)
		imb(selc)=cback(selc)
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
imt(sel)=cexpt(sel)
imb(sel)=cback(sel)
stop
return
end