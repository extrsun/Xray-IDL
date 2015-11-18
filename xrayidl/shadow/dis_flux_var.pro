pro dis_flux_var,count,back,expt,dist,flux,eflux,dlow,dhigh $
,angle=angle,xshift=xshift,yshift=yshift $
,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,block=block,ctonth=ctonth,stonth=stonth,im=im,exptth=exptth,cbv=cbv,bbv=bbv,ebv=ebv,xpref=xpref,ypref=ypref,xabs=xabs
;+
; Calculating the linear surface brightness distribution of an image.
; the default direction is the positive X axis. 
; However, the image may be rotated by ANY angle.
;
; count, back, expt - count, background, and exposure images
; angle - rotaion angle (deg, anti-clockwise)
; xshift,yshift - center position of the rotation axis (image bin)
; xmin, xmax - the lower and upper boundaries of the strip of the image (bin)
; ymin, ymax - the lower and upper pixel limits after the rotation (bin)
; block - bin size in units of !size_pixel
; stonth - signal-to-noise ratio for calculating the flux
; ctonth - count-to-noise ratio for calculating the flux, which replaces stonth
;		if given
;
; dist - output vector containing count-weighted distances of output flux bins
; 	(in units of image bins, starting at the edge of the image -0.5)
; dlow,dhigh - lower and upper limits of the distance intervals of the bins
; flux, eflux - output vectors containing count fluxes and flux errors 
; 	(in units of counts/s arcmin^2)
; im - output image for viewing image bins that are used in the
;      calculation.
; exptth - lower exposure threshold used to select the pixels for the
;          calcuation
; xpref, ypref -  x and y image pixel locations for the rotation.
;
;*Note:
;
; writen by WQD 6/17/93
; add ymin and ymax keywords, WQD, Sept 18 1993
; rot is changed into rotate since rot could resulting more than one
;	bins in the original image into one location in the new image.
; For an arbitrary rotation angle use the angle keyword in get_image.pro
; written by wqd, March 8, 1996
; a part of the original program is extracted to be a new program
; dis_flux_1d_var.pro. Add a new keyword stonth. wqd, May 22, 2003
; 
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- dis_flux_var,count,back,expt,
print,',angle=angle,xshift=xshift,yshift=yshift'
print,',xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,block=block,ctonth=ctonth,stonth=stonth,im=im,exptth=exptth,cbv=cbv,bbv=bbv,ebv=ebv,xpref=xpref,ypref=ypref,xabs=xabs'
return
endif
sz=size(count)
if n_elements(ymin) eq 0 then ymin=0
if n_elements(ymax) eq 0 then ymax=sz(2)
if n_elements(xmax) eq 0 then xmax=sz(1)
if n_elements(xmin) eq 0 then xmin=0
;if n_elements(ctonth) eq 0 then ctonth=5.

loc=lindgen(sz(1)*sz(2))
y = loc/long(sz(1))
x=loc mod sz(1)

if n_elements(block) eq 0 then block=!block
if n_elements(xshift) eq 0 then xshift=0
if n_elements(yshift) eq 0 then yshift=0
rot_xy,x,y,angle,xshift=xshift,yshift=yshift,block=block,xpref=xpref,ypref=ypref,/xyreal

if n_elements(exptth) eq 0. then exptth=0.
c=where(expt(loc) gt exptth and y ge ymin and y lt ymax and x ge xmin and x lt xmax,nc)
if nc eq 0 then stop,'stop: exposure equals to zero'
im=count*0
im(c)=count(c)
x=x(c)
if keyword_set(xabs) ne 0 then x=abs(x-xpref/block) else x=x-xpref/block
ss=sort(x)
x=x(ss)
c=c(ss)
rcount=count(c)
rback=back(c)
rexpt=expt(c)
if !debug eq 3 then stop
dis_flux_1d_var,x,rcount,rback,rexpt,flux,eflux,dist,dlow,dhigh,cbv=cbv,bbv=bbv,ebv=ebv,xshift=xshift,ctonth=ctonth,stonth=stonth,block=block
if !debug eq 3 then stop
return
end






