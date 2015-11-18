pro surb_1d,rdist,rcount,rback,rexpt,flux,eflux,dist,dlow,dhigh,cbv=cbv,bbv=bbv,ebv=ebv,xshift=xshift,ctonth=ctonth,stonth=stonth,block=block,nounit=nounit,rsig2=rsig2,pixelsize=pixelsize
;+
; Calculating the adaptive linear brightness distribution.
;
; rdist, rcount, rback, rexpt - dist, count, background, and exposure vectors
;	sorted spatially
; xshift - spatial position shift (bin)
; block - bin size in units of !size_pixel
; stonth - signal-to-noise ratio for calculating the flux
; ctonth - count-to-noise ratio for calculating the flux, which replaces stonth
;		if given
;
;*OUTPUTS
; dist - output vector containing count-weighted distances of output flux bins
; 	(in units of image bins, starting at the edge of the image -0.5)
; dlow,dhigh - lower and upper limits of the distance intervals of the bins
; flux, eflux - output vectors containing count fluxes and flux errors
; 	(in units of counts/s arcmin^2)
; cbv,bbv,ebv - the adaptively binned count, background, exposure vectors
; nounit - if set, no unit conversion will be performed.
;
; writen by WQD 5/22/03
; modified by ljt 6/24/08, add pixelsize keyword
; modified by ljt 01/06/09, delete trans, so this procedure can be used to all instruments, and the output unit is the same as the input unit.
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- dis_flux_1d_var,rdist,rcount,rback,rexpt,flux,eflux'
print,',dist,dlow,dhigh,cbv=cbv,bbv=bbv,ebv=ebv,xshift=xshift'
print,',ctonth=ctonth,stonth=stonth,block=block,nounit=nounit'
return
endif
if n_elements(stonth) eq 0 then stonth=3
if n_elements(block) eq 0 then block=!block
if n_elements(xshift) eq 0 then xshift=0
if n_elements(rsig2) eq 0 then rsig2=rcount+rback
if n_elements(pixelsize) eq 0 then pixelsize=!size_pixel
;trans=(60./block/pixelsize)^2
;if n_elements(ctonth) eq 0 then begin
;	ctonth=10
;	print,'ctonth is assumed to be ',ctonth
;endif

if ctonth ne 0 then cthyes=1 else begin
	cthyes=0
	ctonth=stonth
endelse
flux=[-999]
eflux=[-999]
dist=[-999]
cbv=[-999]
bbv=[-999]
ebv=[-999]
nc=n_elements(rdist)
odist=rdist
rdist=[rdist,rdist(nc-1)+0.5]-xshift ;with rdist=0 at the rotation center
dlow=[rdist(0)-0.5]
dhigh=[-999]
db=0. & cb=0 & bb=0. & eb=0. &sb=0.
for k=0L, nc-1 do begin
	db=db+rdist(k)*rcount(k)
	cb=cb+rcount(k)
	bb=bb+rback(k)
	eb=eb+rexpt(k)
        sb=sb+rsig2(k)
	if cthyes then ston=sqrt(cb > 0.1) else $
		ston=abs(cb-bb)/sqrt((cb > bb) > 5.)
	;if ston gt ctonth or (k eq nc-1 and ston gt 0.7*ctonth) then begin
	if ston gt ctonth or (k eq nc-1) then begin

	        div=(rdist(k)+rdist(k+1))*0.5
		dhigh=[dhigh,div]
		dlow=[dlow,div]
		dist=[dist,db/cb]
		flux=[flux,(cb-bb)/eb]
print,'cb/eb=',cb/eb,'      bb/eb=',bb/eb, '        cb=',cb,'        bb=',bb,'       eb=',eb
		eflux=[eflux,sqrt(sb)/eb]
;ljt modify:
;		eflux=[eflux,sqrt(sb-bb)/eb]
		cbv=[cbv,cb]
		bbv=[bbv,bb]
		ebv=[ebv,eb]
		db=0 & cb = 0 & bb=0 & eb=0 &sb=0
	endif
endfor
print,dlow/(60./block/pixelsize)
dlow=dlow(0:n_elements(dist)-2)
dhigh=dhigh(1:*)
dist=dist(1:*)
flux=flux(1:*)
eflux=eflux(1:*)
cbv=cbv(1:*)
bbv=bbv(1:*)
ebv=ebv(1:*)
if keyword_set(nounit) ne 1 then begin
    dlow=dlow/(60./block/pixelsize)
    dhigh=dhigh/(60./block/pixelsize)
    dist=dist/(60./block/pixelsize)
    flux=flux;*trans
    eflux=eflux;*trans
endif
rdist=odist
return
end

