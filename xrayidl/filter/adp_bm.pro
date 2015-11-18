pro adp_m,imao,exptao,sflux,sfluxa,mexpt=mexpt,filter=filter,gslo=gslo,gstep=gstep,gshi=gshi,ftonth=ftonth,bch=bch,nonorm=nonorm,gsimsz=gsimsz,nimax=nimax,back=back,fstep=fstep,ncmin=ncmin
;+
; perform an adaptive gaussian smooth of images
;
;*CALLING SEQUENCE:
; adp_m,im,ims,ima,imas,expt=expt,sexpt=sexpt,mexpt=mexpt,filter=filter
; ,gslo=gslo,gstep=gstep,gshi=gshi,ftonth=ftonth,bch=bch,nonorm=nonorm
; ,gsimsz=gsimsz,nimax=nimax
;*PARAMETERS:
; INPUTS:
; imao - stack of input count images for smoothing
; exptao - the exposure of the input images. If supplied, the
;              output is the flux image mutiplied by mexpt
;	
; OUTPUTS:
; sflux - smoothed flux image
; sfluxa - smoothed flux images in individual bands
;
;*OPTIONAL IMPUTS or OUTPUTS:
; mexpt - the maximum value of expt
; sexpt - smoothed expt
; filter - image for selecting pixels in the smoothing
;          calculation. only pixels > 0 are chosen.
; gslo - the initial size of the Gaussian FWHM (def=1 pixel)
; gstep - initial fractional step of the Gaussian size increase (def =
;         0.02), adjusted in the program
;
; gshi - the maximum Gaussian size before stopping (def=sz(1) < sz(2))
; ftonth - the flux-to-noise ratio lower limit  for the smoothing
; bch - choice for background estimate during the flux to noise
;             ratio calculation. (def =0). If =1, the flux is the net
;             flux above the median flux value of the remaining pixels
; gsimsz   - gaussian image size in units of bins (def=min(sz(1:2)))
; nimax - the maximum number of smoothing steps (def=1000)
;
;*SUBROUTINES CALLED:
;   covolve
;   gsf_gaussian
;
;*MODIFICATION HISTORY:
;    written  by wqd, May 27, 2003
;-
;------------------------------------------------------------------------------
np=n_params ()
if np eq 0 then begin
print,'Calling SEQ - adp_m,imao,exptao,sflux,sfluxa,mexpt=mexpt,filter=filter'
print,',gslo=gslo,gstep=gstep,gshi=gshi,ftonth=ftonth,bch=bch,nonorm=nonorm'
print,',gsimsz=gsimsz,nimax=nimax,fstep=fstep,ncmin=ncmin'
return
endif
if n_elements(ncmin) eq 0 then ncmin=0.01
if n_elements(fstep) eq 0 then fstep=0.02
if n_elements(bch) eq 0 then bch=0
if n_elements(nimax) eq 0 then nimax=1000
if n_elements(ftonth) eq 0 then ftonth=3
if n_elements(gstep) eq 0 then gstep=0.02
if n_elements(gslo) eq 0 then gslo=1.
sz=size(imao)
if sz(0) eq 2 then nima=1 else if sz(0) eq 3 then nima=sz(3) else begin
    print,'ima must be a 2-D or 3-D array!'
    return
endelse
if n_elements(gshi) eq 0 then gshi=min(sz(1:2))/2.
if n_elements(gsimsz) eq 0 then gsimsz=((sz(1) < sz(2))/8)*2+1 ;odd dim

mexpt=max(exptao)
expta=exptao/mexpt
ima=imao

;if the filter is supplied
nfilter=n_elements(filter)
if nfilter eq 0 then begin
    nsel=sz(1)*sz(2)
    sel=lindgen(nsel) 
endif else begin
    sel=where(filter gt 0,nsel)
    imagea_sel,sel,ima
    imagea_sel,sel,expta
endelse


sflux=fltarr(sz(1),sz(2))
sfluxa=expta*0.
imac=sfluxa
exptac=sfluxa
imaec=sfluxa
mimac=sfluxa

nback=n_elements(back) 
if nback ne 0 then bback=total(back,3)
ni=0
binsize=gslo
nselo=nsel
ncc=0
while ni lt nimax and binsize lt gshi do begin
    print,'ni, binsize,nsel = ',ni,binsize,nsel
     wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
     for k=0,nima-1 do begin
         imac(*,*,k)=convolve(ima(*,*,k),wlimg) 
         exptac(*,*,k)=convolve(expta(*,*,k),wlimg) 
         imaec(*,*,k)=convolve(expta(*,*,k),wlimg^2)
     endfor
     fluxac=imdiv(imac,exptac)
     fluxc=total(fluxac,3)
     if nback ne 0 then begin
         ime=sqrt(total(imaec*fluxac,3)+convolve(bback,wlimg^2)) 
     endif else ime=sqrt(total(imaec*fluxac,3))
     if bch eq 0 then fton=imdiv(total(imac,3),ime) else begin
        for k=0,nima-1 do begin
            imt=fluxac(*,*,k)
            mimac(*,*,k)=median(imt(sel))*exptac(*,*,k)
        endfor 
        fton=imdiv(total(imac-mimac,3), ime) 
    endelse 
     
     c=where(fton(sel) gt ftonth,nc)
     if nc ne 0 then begin
         selc=sel(c)
         sflux(selc)=fluxc(selc)
         imagea_sel,selc,fluxac,sfluxa
         if nc eq nsel then goto,done
;imagea_sel,sel,ima ;for removing the pixels completely
;imagea_sel,sel,expta
         remove,c,sel
         if nc gt fstep*nsel and nc gt fstep*(nselo-nsel) then $
           gstep=gstep*0.5      ;current step is too large
         nsel=nsel-nc
;         if ncc eq 0 then begin
;             ncc=1
;             cc=selc 
;         endif else cc=[cc,selc]
;         imagea_sel,cc,imac,ima
;         imagea_sel,cc,exptac,expta
     endif 
     if nc lt (ncmin*(nselo-nsel) > 1) then $
       gstep=gstep*1.5          ;current step is too small
     ni=ni+1
     binsize=binsize*(1.+gstep)
     if !debug eq 1 then stop
     if nsel lt nselo*0.05 then goto,done
endwhile
done:
stop
;for the rest of the image, set to the current values
; and normalize the smoothed image value to avoid any intensity shift
sflux(sel)=fluxc(sel) 
imagea_sel,sel,fluxac,sfluxa
if keyword_set(nonorm) ne 0 then begin
    ;get the image index of the pixels with non-zero exposure in all bands
    for k=0,nb-1 do if k eq 0 then temp=expta(0) else temp=temp*expta(k)
    sel=where(temp gt 0.)

    fluxa=imdiv(ima,exptn)
    sflux=total(fluxa(sel))/total(sflux(sel))*sflux
    ;get the totals of the images in all bands
    total_3d,fluxa,totalf,sel=sel
    total_3d,sfluxa,totalsf,sel=sel
    for k=0,nima-1 do $
      sfluxa(*,*,k)=totalf(k)/totalsf(k)*sfluxa(*,*,k)
endif
stop
return
end
