pro dis_flux,count,back,expt,dist,flux,eflux,dlow,dhigh $
,angle=angle,xshift=xshift,yshift=yshift $
,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,block=block,ctonth=ctonth,im=im
;-
; Calculating the linear surface brightness distribution of an observation
; the default direction is the positive X axis after the rotation. 
; However, the image may be 
; rotated by ANY angle.
; count, back, expt - count, background, and exposure images
; angle - rotaion angle (deg, anti-clockwise)
; xshift,yshift - center position of the rotation axis (image bin)
; xmin, xmax - the lower and upper boundaries of the strip of the image (bin)
; ymin, ymax - the lower and upper pixel limits after the rotation (bin)
; block - bin size in units of SASS 0.5" pixel
; ctonth - count-to-noise ratio for calculating the flux
;
; dist - output vector containing count-weighted distances of output flux bins
; 	(in units of image bins, starting at the edge of the image -0.5)
; ,dlow,dhigh - lower and upper limits of the distance intervals of the bins
; flux, eflux - output vectors containing count fluxes and flux errors 
; 	(in units of counts/s arcmin^2)
; im - output image for viewing image bins that are used in the calculation.
; writen by WQD 6/17/93
; add ymin and ymax keywords, WQD, Sept 18 1993
; rot is changed into rotate since rot could resulting more than one
;	bins in the original image into one location in the new image.
; For an arbitrary rotation angle use the angle keyword in get_image.pro
; written by wqd, March 8, 1996
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- dis_flux,count,back,expt,dist,flux,eflux,dlow,dhigh'
print,',angle=angle,xshift=xshift,yshift=yshift'
print,',xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,block=block,ctonth=ctonth,im=im'
return
endif
if n_elements(block) eq 0 then block=!block
sz=size(count)
if n_elements(ymin) eq 0 then ymin=0
if n_elements(ymax) eq 0 then ymax=sz(2)
if n_elements(xmax) eq 0 then xmax=0
if n_elements(xmin) eq 0 then xmin=sz(1)
if n_elements(ctonth) eq 0 then ctonth=5.
cth=ctonth^2
trans=(60./block/!size_pixel)^2

loc=lindgen(sz(1)*sz(2))
y = loc/long(sz(1))
x=loc mod sz(1)

rot_xy,x,y,angle,xshift=xshift,yshift=yshift,block=block,/absshift

c=where(expt(loc) gt 0. and y ge ymin and y le ymax and x ge xmin and x le xmax,nc)
if nc eq 0 then stop,'stop: exptosure equals to zero'

c=c(sort(x(c)))
im=count*0
im(c)=count(c)
x=x(c)
rcount=count(c)
rback=back(c)
rexpt=expt(c)
flux=[-999]
eflux=[-999]
dist=[-999]
dlow=[x(0)-0.5]
x=[x,x(nc-1)+0.5]
dhigh=[-999]
db=0. & cb=0 & bb=0. & eb=0.
for k=0, nc-1 do begin
	db=db+x(k)*rcount(k)
	cb=cb+rcount(k)
	bb=bb+rback(k)
	eb=eb+rexpt(k)
	if cb gt cth or (k eq nc-1 and eb ne 0.) then begin
		div=(x(k)+x(k+1))*0.5
		dhigh=[dhigh,div]
		dlow=[dlow,div]
		dist=[dist,db/cb]
		flux=[flux,(cb-bb)/eb]
		eflux=[eflux,sqrt(cb > bb)/eb]
		db=0 & cb = 0 & bb=0 & eb=0
	endif
endfor
dlow=dlow(0:n_elements(dist)-2)/(60./block/!size_pixel)
dhigh=dhigh(1:*)/(60./block/!size_pixel)
dist=dist(1:*)/(60./block/!size_pixel)
flux=flux(1:*)*trans
eflux=eflux(1:*)*trans
end
