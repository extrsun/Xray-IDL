pro dis_flux2,count,back,expt,dist,flux,eflux,rd=rd,binint=binint $
,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,nbin=nbin,block=block,sn=sn,im=im
;-
; Calculating the linear surface brightness distribution of an observation
; the default direction is the positive X axis. However, the image may be 
; rotated by 90, 180, 270 deg (anti-clockwise).
; count, back, expt - count, background, and exposure images
; xmin, xmax - the lower and upper boundaries of the strip of the image
;	after the rotation and rebin
; ymin, ymax - the lower and upper pixel limits after the rotation, BUT 
;		before the rebin
; rd - the rotation direction: 1: 90, 2: 180, 3: 270 deg (anti-clockwise;
;	although clockwise is said in the IDL manual)
;
; writen by WQD 6/17/93
; add ymin and ymax keywords, WQD, Sept 18 1993
; rot is changed into rotate since rot could resulting more than one
;	bins in the original image into one location in the new image.
; For an arbitrary rotation angle use the angle keyword in get_image.pro
; wqd, June 23, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- dis_flux,count,back,expt,dist,flux,eflux'
print,',rd=rd,binint=binint,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,nbin=nbin,block=block,sn=sn,im=im'
return
endif

if n_elements(sn) eq 0 then sn=2.
if n_elements(binint) eq 0 then binint=1.
if n_elements(block) eq 0 then block=!block
trans=(60./block/!size_pixel)^2*1.e4

if n_elements(rd) ne 0 then begin
	rcount=rotate(count,rd)
	rback=rotate(back,rd)
	rexpt=rotate(expt,rd)
endif else begin
	rcount=count
	rback=back
	rexpt=expt
endelse
sz=size(rcount)
	if !debug eq 1 then stop,'after the rotation of the images'
sz=size(count)
y = lindgen(sz(1)*sz(2))/(long(sz(2))) ;*long(binint))
if n_elements(ymin) eq 0 then ymin=min(y)
if n_elements(ymax) eq 0 then ymax=max(y)
c=where(rexpt gt 0. and y ge ymin and y le ymax,nc)
if nc eq 0 then stop,'stop: exptosure equals to zero'
im=rcount*0
im(c)=rcount(c)
rcount=rcount(c)
rback=rback(c)
rexpt=rexpt(c)

xdist = (c mod sz(1))
x=xdist/long(binint)
xdist=(float(xdist)+0.5)/float(binint)
if n_elements(xmax) eq 0 then xmax=max(x)
if n_elements(xmin) eq 0 then xmin=min(x)

print,'max and min x are: ',xmin,xmax
flux=fltarr(xmax+1)
bflux=flux
eflux=flux
dist=(0.5+findgen(xmax+1))*binint
nbin=lonarr(xmax+1)
if !debug eq 1 then stop
for k=xmin,xmax do begin
	sel=where(x eq k ,nsel)
	if nsel ne 0 then begin
		flux(k)=total(rcount(sel)-rback(sel))/total(rexpt(sel))
		bflux(k)=total(rback(sel))/total(rexpt(sel))
		eflux(k)=sqrt(total(rcount(sel)>rback(sel)))/total(rexpt(sel))
	  	nbin(k)=nsel
	endif
endfor
c=where(eflux gt 0. and eflux lt sn*(flux+bflux)) ; sn is also a choice 
					;to get rid of those bad bins
flux=flux(c)*trans
eflux=eflux(c)*trans
dist=dist(c)/(60./block/!size_pixel)
nbin=nbin(c)
if !debug eq 1 then stop
end
