pro flux_1d_var,count,back,expt,dist,flux,eflux,dlow,dhigh,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,block=block,ctonth=ctonth,im=im,cbv=cbv,bbv=bbv,ebv=ebv,lpam=lpam
;+
; Calculating the linear surface brightness distribution of an image in 
; the positive X axis. For arbitrary rotation, use dis_flux_var.pro.
;
; count, back, expt - count, background, and exposure images
; xmin, xmax - the lower and upper boundaries of the strip of the image (bin)
; ymin, ymax - the lower and upper pixel limits after the rotation (bin)
; block - bin size in units of !size_pixel
; ctonth - signal-to-noise ratio for calculating the flux
;
; dist - output vector containing count-weighted distances of output flux bins
; 	(in units of image bins, starting at the edge of the image -0.5)
; dlow,dhigh - lower and upper limits of the distance intervals of the bins
; flux, eflux - output vectors containing count fluxes and flux errors 
; 	(in units of counts/s arcmin^2)
; im - output image for viewing image bins that are used in the calculation.
; lpam - the linear conversion of the units (def=(block*!size_pixel)/60.)
;
; writen by WQD 12/30/2002 
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- flux_1d_var,count,back,expt,dist,flux,eflux'
print,',dlow,dhigh,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,block=block'
print,',ctonth=ctonth,im=im,cbv=cbv,bbv=bbv,ebv=ebv,lpam=lpam'
return
endif
if n_elements(block) eq 0 then block=!block
sz=size(count)
if n_elements(ymin) eq 0 then ymin=0
if n_elements(ymax) eq 0 then ymax=sz(2)-1
if n_elements(xmax) eq 0 then xmax=sz(1)-1
if n_elements(xmin) eq 0 then xmin=0
if n_elements(ctonth) eq 0 then ctonth=5.
if n_elements(lpam) eq 0 then lpam=(block*!size_pixel)/60.
pam=(1./lpam)^2
rcount=count & rback=back 
ss=where(expt le 0.,nss)
if nss ne 0 then begin
	rcount(ss)=0 & rback(ss)=0
endif
rcount=total(rcount(xmin:xmax,ymin:ymax),2)
rback=total(rback(xmin:xmax,ymin:ymax),2)
rexpt=total(expt(xmin:xmax,ymin:ymax),2)
nc=xmax-xmin+1
x=findgen(nc)+xmin

flux=[-999]
eflux=[-999]
dist=[-999]
cbv=[-999]
bbv=[-999]
ebv=[-999]

dlow=[x(0)-0.5]
dhigh=[-999]
db=0. & cb=0 & bb=0. & eb=0.
for k=0L, nc-1 do begin
	db=db+x(k)*rcount(k)
	cb=cb+rcount(k)
	bb=bb+rback(k)
	eb=eb+rexpt(k)
	if (cb-bb)/sqrt(cb > bb > 1) gt ctonth then begin 
;		dhigh=[dhigh,x(k)]
;		dlow=[dlow,x(k)+1]
		dhigh=[dhigh,x(k)+0.5]
		dlow=[dlow,x(k)+0.5]
		dist=[dist,db/cb]
		flux=[flux,(cb-bb)/eb]
		eflux=[eflux,sqrt(cb > bb > 1)/eb]
		cbv=[cbv,cb]
		bbv=[bbv,bb]
		ebv=[ebv,eb]
		db=0 & cb = 0 & bb=0 & eb=0
	endif
endfor
dlow=dlow(0:n_elements(dist)-2)*lpam
dhigh=dhigh(1:*)*lpam
;dlow(1:*)=dlow(1:*)+1
dist=dist(1:*)*lpam
flux=flux(1:*)*pam
eflux=eflux(1:*)*pam
cbv=cbv(1:*)
bbv=bbv(1:*)
ebv=ebv(1:*)
return
end






