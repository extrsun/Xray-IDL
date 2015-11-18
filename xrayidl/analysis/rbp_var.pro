;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; rbp_var
;
;*PURPOSE:
; Obtain a radial brightness profile around a specified position in an image,
; using adaptive binning
;
;*PARAMETERS:
; INPUTS:
; image_c - count image or its equivelent 
;
;*OUTPUTS:
; dist - output vector containing count-weighted distances of output flux bins
; 	(in units of image bins, starting at the edge of the image -0.5)
; flux, eflux - output vectors containing count fluxes and flux errors 
; 	(in units of counts/s arcmin^2)
; dlow,dhigh - lower and upper limits of the distance intervals of the bins
; cbv,bbv,ebv - the adaptively binned count, background, exposure vectors
;
;*OPTIONAL INPUTS:
; stonth - signal-to-noise ratio for calculating the flux (def=3)
; ctonth - count-to-noise ratio for calculating the flux, which replaces stonth
;		if given
; block - blocksize of the image pixel in units of 0".5
; xc, yc - respectively,  the x, y pixel positions of the center around which
;          the rbp will be calculated
; radiuslow, radiushi - the lower and upper radius limits
;	(in units of arcsec) of the annulus within which 
;          the rbp is to be calculated 
; plotoff - if set, the program will not creat the rbp plot on the screen
; ee, al - ellipticity and angle for elliptical coordinates
;
;*SUBROUTINES CALLED:
; tvcircle
;
;
;*MODIFICATION HISTORY:
; writen as an adaptive version of rbp; wqd, May 22, 03
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro  rbp_var,image_c,dist,flux,eflux,dlow,dhigh,cbv=cbv,bbv=bbv,ebv=ebv $
,image_t=image_t,block=block,xc=xc,yc=yc,radiuslow=radiuslow $
,radiushi=radiushi,plotoff=plotoff ,image_b=image_b $
,ctonth=ctonth,stonth=stonth,ee=ee,al=al
;---------------------------------------------------------------
if n_params() EQ 0 then begin
print,'CALL SEQUENCE - rbp_var,image_c,dist,flux,eflux,dlow,dhigh'
print,',cbv=cbv,bbv=bbv,ebv=ebv,image_t=image_t,block=block,xc=xc,yc=yc'
print,',radiuslow=radiuslow,radiushi=radiushi,plotoff=plotoff ,image_b=image_b'
print,',ctonth=ctonth,stonth=stonth,ee=ee,al=al'
retall
endif
;---------------------------------------------------------------
if n_elements(block) eq 0 then block=!block
sz=size(image_c) 
szmin=(sz(1) < sz(2))
if n_elements(radiuslow) eq 0 then rlow=0. $
	else rlow=radiuslow/(!size_pixel*block)

if n_elements(radiushi) eq 0 then rhi=0.5*szmin $
 else begin
   rhi=radiushi/(!size_pixel*block)
   if rhi gt szmin then begin
     rhi=szmin
     print,'radius exceeds the half size of the image and is set to be dim/2'
   endif
endelse

;if n_elements(ctonth) eq 0 then begin
;	ctonth=10
;	print,'ctonth is assumed to be ',ctonth
;endif

if n_elements(xc) EQ 0 then xc=(sz(1)-1)/2.
if n_elements(yc) EQ 0 then yc=(sz(2)-1)/2.
if n_elements(image_b) eq 0  then image_b=fltarr(sz(1),sz(2))
if n_elements(image_t) eq 0 then begin 
	image_t=fltarr(sz(1),sz(2))+1. 
	print,'All pixels are considered to be good.'
	print,'The effective exposures in the pixels have been set to be 1'
endif
;
if keyword_set(plotoff) eq 0 then begin
  if rlow ne 0 then tvcircle,rlow,xc,yc    
  tvcircle,(rhi < 20),xc,yc      ;draws a circle around the source
  tvcircle,rhi,xc,yc   
endif
;
if n_elements(ee) ne 0 then dist_ellipse,circle,szmin,xc,yc,ee,al $
	else  dist_circle,circle,szmin,xc,yc
good=where((circle ge rlow) and (circle LT rhi) and (image_t gt 0.), n_bin)

if n_bin eq 0 then $
	stop,'No data covered in the radius. Is the radius too small?'

good=good(sort(circle(good)))
rdist=circle(good)
rcount=float(image_c(good))
rback=image_b(good)
rexpt=image_t(good)
dis_flux_1d_var,rdist,rcount,rback,rexpt,flux,eflux,dist,dlow,dhigh,cbv=cbv,bbv=bbv,ebv=ebv,ctonth=ctonth,stonth=stonth,block=block

if keyword_set(plotoff) eq 0 then begin
   	erase
	ploterr,dist,flux,eflux, Xtitle='Radius (arcmin)', $
	Ytitle='Flux (Counts/s arcmin^2',Title='Radial Flux Distribution'
endif

return
end 
