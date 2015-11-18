;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; scan_wl_main
;
;*PURPOSE:
; Main program for wavelet source detection 
;
;*CALLING SEQUENCE:
; scan_wl_main,cra,cdec,flux,fluxe,mask=mask,,block=block,radius=radius 
;,threshold=threshold,wbinsize=wbinsize,outfile=outfile,wimsa=wimsa,noscan=noscan
;
;*PARAMETERS:
;*INPUTS:
; cra, cdec - the ra and dec of the image center (deg)
; flux, fluxe - the array containing flux and flux error images
; 	example: flux=imdiv(c45,t45)+imdiv(c67,t67)
;		 fluxe=imdiv(c45,t45^2)+imdiv(c67,t67^2)
; mask - image with values <= 0 indicating no data available 
;		to remove alias in blank regions around the image.
; block - the bin size of the image in units of 0".5 (def = 30)
; wbinsiz - vector containing array bin sizes (in units of wavelet Hat scale)
;	def=1./2^findgen(4) =1/[1,2,4,8]
; radius - the radius (in units of bins) of the circle 
; 	(centered at the image center) within which the source search
;	is going to be conducted (def = 54')
; threshold - the lower limit of the signal-to-noise ratio of a peak which
;	is considered to be a source candidate (def = 2.5)
; outfile - the output source file name (def = !seq_no+'_sou_wl47.dat')
;	   the cntr in the output is the wavelet filtered value
; bfluxe - flux error contributioned by the subtracted background 
;          component (e.g., de-streaking of Chandra M82 Ob #361). 
; added by wqd, 5/7/03
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
pro scan_wl_main,cra,cdec,flux,fluxe,mask=mask,block=block,radius=radius,inr=inr $
 ,wbinsize=wbinsize,threshold=threshold,outfile=outfile,wimsa=wimsa,noscan=noscan,bfluxe=bfluxe,append=append
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - scan_wl_main,cra,cdec,flux,fluxe,block=block'
print,',wbinsize=wbinsize,radius=radius,inr=inr,threshold=threshold,outfile=outfile'
print,',wimsa=wimsa,noscan=noscan,bfluxe=bfluxe,append=append'
return
endif
;
mine=imdiv(0.1*avg(flux),mask) > 1.e-10 ;assuming mask=exposure map
 ;0.1 is for 10% of the mean flux as the minimum background
if n_elements(threshold) eq 0 then threshold = 2.5
if n_elements(append) eq 0 then append=0
if n_elements(outfile) eq 0 then outfile='sou_wl'
if n_elements(wbinsize) eq 0 then begin
	if !instr eq 'P' then begin 
		nk=4
		wbinsize=1./2^(findgen(nk)+1) 
	endif else begin
		nk=2
		wbinsize=1./2^(findgen(nk))
	endelse
endif else nk=n_elements(wbinsize)

sz=size(flux)
wimsa=fltarr(sz(1),sz(2),nk)

for k=1,nk do begin
 wlim=0
 bintosize=wbinsize(k-1)
 wltran,flux,wim,bintosize,wlim=wlim
 wltran,fluxe,wims,wlim=wlim^2
 if n_elements(bfluxe) ne 0 then begin
     wltran,bfluxe,bwims,wlim=wlim^2
     wims=wims+bwims
 endif
wims=imdiv(wim,sqrt(wims > mine)) < 1000.
 ;wims=imdiv(wim,sqrt(wims > median(wims(where(mask gt 0))))) < 1000. ;remove effects of artifacts when
				; the wavelet cap size is small.
 wimsa(*,*,k-1)=wims

 ss=where(mask le 0.,nss)
 if nss ne 0 then wims(ss)=0.
 if !debug eq 1 then stop
 if keyword_set(noscan) eq 0 then begin
  if append eq 0 then begin
   scan_wl,cra,cdec,wim,wims,thre=threshold,block=block,sr=1./bintosize $
	,rad=radius,inr=inr,file=outfile 
   append=1
  endif else begin
	scan_wl,cra,cdec,wim,wims,thre=threshold,block=block,sr=1./bintosize $
	,rad=radius,inr=inr,file=outfile,/append
  endelse
endif
endfor
return
end
