;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; scan_wl_main
;
;*PURPOSE:
; scan over wavelet images for sources with a certain signal-to-noise ratio 
; greater than a threshold
;
;*CALLING SEQUENCE:
; scan_wl_main,cra,cdec,flux,fluxe,block=block,radius=radius 
; 	,threshold=threshold,wbinsize=wbinsize,outfile=outfile
;
;*PARAMETERS:
;*INPUTS:
; cra, cdec - the ra and dec of the image center (radian)
; flux, fluxe - the array containing flux and flux error images
; 	example: flux=imdiv(c45,t45)+imdiv(c67,t67)
;		 fluxe=imdiv(c45,t45^2)+imdiv(c67,t67^2)
; block - the bin size of the image in units of 0".5 (def = 30)
; wbinsiz - vector containing array bin sizes (in units of wavelet Hat scale)
;	def=1./(findgen(4)+1.)^2 =1/[1,2,4,8]
; radius - the radius (in units of bins) of the circle 
; 	(centered at the image center) within which the source search
;	is going to be conducted (def = 54')
; threshold - the lower limit of the signal-to-noise ratio of a peak which
;	is considered to be a source candidate (def = 2.5)
; outfile - the output source file name (def = !seq_no+'_sou_wl47.dat')
;
;*PROCEDURE:
;
; assuming block=30, the si values used sample the different sizes of the 
; PSPC PSF 
;wbinsize=1     ; the on-axis PSF, does add new sources, sensitive only <20'
;wbinsize=1./2. ; most sensitive to point sources
;wbinsize=1./4. ; important in both central and outskirt
;wbinsize=1./8. ;about the maximum PSF Gaussian size in the pspc
;
;*RESTRICTIONS:
; Faint sources near bright sources may be missed. Thus a initial low 
; threshold can be helpful. Or alternative algorithm should then be used. 
;
;*EXAMPLES:
; scan_wl_main,cra,cdec,flux,fluxe
;
;;*MODIFICATION HISTORY:
;
; writen by wqd, April 17, 1996
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro scan_gs,cra,cdec,count,back,expt,block=block,radius=radius $
 ,wbinsize=wbinsize,threshold=threshold,outfile=outfile $
,noscan=noscan,gsimsz=gsimsz,imcs=imcs
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - scan_wl_main,cra,cdec,flux,fluxe,block=block'
print,',wbinsize=wbinsize,radius=radius,threshold=threshold,outfile=outfile'
print,',wims=wims,noscan=noscan'
return
endif
;
if n_elements(gsimsz) eq 0 then gsimsz=60*2+1
if n_elements(threshold) eq 0 then threshold =2.5
if n_elements(outfile) eq 0 then outfile='sou_gs47'
if n_elements(wbinsize) eq 0 then begin
	nk=3 ;4 will leads to detections which have problems for MLE analysis
	wbinsize=2^(findgen(nk)+1) 
endif else nk=n_elements(wbinsize)

for k=1,nk do begin
	fwhm=wbinsize(k-1)
	wlimg=psf_gaussian(nd=2,fwhm=fwhm,np=gsimsz,/norm)
	wlimg=image_cut(wlimg,0.74*fwhm,/pix,/keep)
	;0.74 is chosen to be the scale of maximum wavelet coef and
	; to have 3x3 classical detection box for wbinsize=2
	flux=imdiv(count,expt)
	imc=convolve(flux-back,wlimg)
	imcs=convolve(imdiv(flux,expt),wlimg^2)
	imcs=imdiv(imc,sqrt(imcs > 1.e-20)) < 1000. 
	imc=imc*(2*!pi*(0.4246*fwhm)^2) ;convert into count rate for a gaussian
	;remove the effects of artifacts when
				; the wavelet cap size is small.
if !debug eq 1 then stop
if keyword_set(noscan) eq 0 then begin
 if k eq 1 then begin
  scan_wl,cra,cdec,imc,imcs,thre=threshold,block=block,sr=wbinsize(k-1)*0.5 $
	,rad=radius,file=outfile 
 endif else begin
  scan_wl,cra,cdec,imc,imcs,thre=threshold,block=block,sr=wbinsize(k-1)*0.5 $
	,rad=radius,file=outfile,/append
 endelse
endif
endfor
return
end