pro adp_gs_m,im,ima,ims,imas,npo=npo,npd=npd,expt=expt,back=back $
,th1=th1,nimax=nimax,bn=bn,bmax=bmax,gsimsz=gsimsz,filter=filter
;-
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
;	back - background map to be added to the flux map, im for
;		calculating dispersions with pixel size same as that of
;		im. the image IM should now be a background subtracted 
;		map. If expt is given, both IM and BACK should be flux maps
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
print,'Calling SEQ - adp_gs_m,im,ima,ims,imas,npo=npo,npd=npd,expt=expt'
print,',back=back,th1=th1,nimax=nimax,bn=bn,bmax=bmax,gsimsz=gsimsz,filter=filter'
return
endif
if n_elements(nimax) eq 0 then nimax=1000
if n_elements(th1) eq 0 then th1=6
if n_elements(gsimsz) eq 0 then gsimsz=60*2+1
if n_elements(npd) eq 0 then npd=0.1
if n_elements(npo) eq 0 then npo=2.

sza=size(ima)
if sza(0) eq 2 then nima=1 else nima=sza(3)

if n_elements(back) eq 0 then back=im*0.+avg(im)*0.1 ;arbitrary value 
if n_elements(expt) ne 0 then begin ;when image contains flux values
	eim=imdiv(im,expt) 
	eback=imdiv(back,expt)
endif else begin ;when im contain counts
	eim=im
	eback=back
endelse

sz=size(im)
if n_elements(filter) eq 0 then begin
	sel=lindgen(sz(1)*sz(2))
	nsel=sz(4)
	nfilter=0
endif else begin
	sel=where(filter gt 0,nsel)
	nfilter=1
endelse
if n_elements(bmax) eq 0 then bmax=min(sz(1:2))/2

binsize=npo
ims=im*0.
imas=ima*0.
ni=0
nselo=nsel
while ni lt nimax and binsize lt bmax do begin
	print,'ni, binsize,nsel = ',ni,binsize,nsel
	wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
	imc=convolve(im,wlimg) 
	swlimg=wlimg^2
	imssigma=convolve(eim,swlimg)
	imssigma=imssigma > convolve(eback,swlimg) 
	fton=imdiv(imc,sqrt(imssigma)) 
;	c=where(fton(sel) gt th1,nc)
	imcth1=(float(th1)/binsize)^2*0.25*alog(2.) 
	;at least 0.5 of the expected level
	c=where(fton(sel) gt th1 and imc(sel) gt imcth1,nc)
	if nc ne 0 then begin
		nsel=nsel-nc
		if nsel lt nselo*0.1 then begin
			selc=sel
			ni=nimax ;the loop is finished
		endif else selc=sel(c)
		ims(selc)=imc(selc)
		for k=0,nima-1 do begin
			imst=convolve(ima(*,*,k),wlimg)
			imt=replicate(0.,sz(1),sz(2))
			imt(selc)=imst(selc)
			imas(*,*,k)=imas(*,*,k)+imt
		endfor
		remove,c,sel
	if !debug eq 1 then stop
	endif
	ni=ni+1
	binsize=binsize*(1.+npd)
endwhile

if nfilter then im(where(filter eq 0))=0.
ims=total(im)/total(ims)*ims 
for k=0,nima-1 do begin
	imt=imas(*,*,k) 
	if nfilter then imt(where(filter eq 0))=0.
	imas(*,*,k)=total(ima(*,*,k))/total(imt)*imt
endfor
;normalized it back to im because of biasing in the smoothing 
; (about 10% increasing the intensity)
return
end