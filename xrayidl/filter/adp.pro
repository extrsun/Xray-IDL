pro adp,im,ims,ima,imas,gsimsz=gsimsz,bmax=bmax,npo=npo,npd=npd,expt=expt,th1=th1,nimax=nimax,bch=bch,sexpt=sexpt,mexpt=mexpt,nonorm=nonorm
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

if n_elements(bch) eq 0 then bch=0
if n_elements(nimax) eq 0 then nimax=1000
if n_elements(th1) eq 0 then th1=3
if n_elements(npd) eq 0 then npd=0.1
if n_elements(npo) eq 0 then npo=1.
if n_elements(bmax) eq 0 then bmax=min(sz(1:2))/2
if n_elements(gsimsz) eq 0 then gsimsz=sz(1) < sz(2)

sz=size(im)
nexpt=n_elements(expt)
if nexpt eq 0 then sel=lindgen(sz(1)*sz(2)) else begin
    sel=where(expt gt 0,nsel)
    mexpt=max(expt)
    nexpt=expt/mexpt
    sexpt=expt*0.
endelse

nima=n_elements(ima)
if nima ne 0 then begin
    sza=size(ima)
    if sza(0) eq 2 then nima=1 else nima=sza(3)
    imas=ima*0.
endif

binsize=npo
ims=im*0.

ni=0
nselo=nsel
while ni lt nimax and binsize lt bmax do begin
	print,'ni, binsize,nsel = ',ni,binsize,nsel
	wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
	imc=convolve(im,wlimg) 
        if nexpt ne 0 then begin
            exptc=convolve(expt,wlimg) 
            flux=imdiv(imc,exptc)
            imssigma=sqrt(convolve(expt,wlimg^2)*flux  > 1.e-20)
            mimc=median(flux(sel))*exptc
        endif else mimc=median(imc)
        if bch eq 1 then fton=imdiv(imc, imssigma) else $
          fton=imdiv(imc-mimc, imssigma)
	c=where(fton(sel) gt th1,nc)
	if nc ne 0 then begin
            selc=sel(c)
            ims(selc)=flux(selc)
            if nexpt ne 0 then sexpt(selc)=exptc(selc)
            if nima gt 0 then begin
                    for k=0,nima-1 do begin
                        imst=convolve(ima(*,*,k),wlimg)
                        imt=replicate(0.,sz(1)*sz(2))
                        imt(selc)=imst(selc)
                        imas(*,*,k)=imas(*,*,k)+imt
                    endfor
            endif
	if nc eq nsel then goto,done
	remove,c,sel
	nsel=nsel-nc
	endif
ni=ni+1
binsize=binsize*(1.+npd)
if !debug eq 1 then stop
if nsel lt nselo*0.05 then goto,done
endwhile
done:
ims(sel)=flux(sel)
if nexpt ne 0 then sexpt(sel)=exptc(sel)
if keyword_set(nonorm) ne 0 then begin
    if nexpt eq 0 then ims=total(im)/total(ims)*ims else begin
      im=total(imdiv(im,nexpt))/total(ims)*ims 
      sexpt=total(expt)/total(sexpt)*sexpt
    endelse
endif

if nima gt 0 then begin
    for k=0,nima-1 do begin
	imst=convolve(ima(*,*,k),wlimg)
	imt=replicate(0.,sz(1)*sz(2))
	imt(sel)=imst(sel)
	imas(*,*,k)=imas(*,*,k)+imt
	if keyword_set(nonorm) ne 0 then $
          imas(*,*,k)=total(ima(*,*,k))/total(imt)*imt
    endfor
endif
;normalized it back to im because of biasing in the smoothing 
; (about 10% increasing the intensity)
return
end
