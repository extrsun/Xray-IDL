pro dis_flux,count,back,expt,dist,flux,eflux,eflux_s,angle=angle,binint=binint $
,xmin=xmin,xmax=xmax,nbin=nbin,block=block
;-
; Calculating the linear surface brightness distribution of an observation
; the default direction is the positive X axis. However, the image may be 
; rotated
; writen by WQD 6/17/93
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- dis_flux,count,back,expt,dist,flux,eflux,eflux_s'
print,',angle=angle,binint=binint,xmin=xmin,xmax=xmax,nbin=nbin,block=block'
return
endif

if n_elements(binint) eq 0 then binint=1.
if n_elements(block) eq 0 then block=!block
trans=(60./block/!size_pixel)^2
f=imdiv((count -back ),expt )*trans
ef=imdiv(sqrt(count  > 1.),expt )*trans ; 1 is an arbitrary choice
if n_elements(angle) ne 0 then begin
	f=rot(f,angle)
	ef=rot(ef,angle,missing=0.)
	if !debug eq 1 then stop,'after the rotation of the images'
endif
c=where(ef gt 0.,nc)
if nc eq 0 then stop,'exptosure equals to zero'
f=f(c)
ef=ef(c)

stop

sz=size(count)
x = (c mod sz(1))/long(binint)
if n_elements(xmax) eq 0 then xmax=max(x)
if n_elements(xmin) eq 0 then xmin=min(x)
print,'max and min x are: ',xmin,xmax
flux=fltarr(xmax+1)
eflux=flux
eflux_s=flux
dist=(0.5+findgen(xmax+1))*binint
nbin=lonarr(xmax+1)
for k=xmin,xmax do begin
	sel=where(x eq k ,nsel)
	if nsel ne 0 then avg_least,f(sel),ef(sel),mflux,mfluxe,mfluxe_s
	;if nsel ne 0 then avg_median,f(sel),mflux,mfluxe,mfluxe_s
	flux(k)=mflux
	eflux(k)=mfluxe
	eflux_s(k)=mfluxe_s
	nbin(k)=nsel
endfor
c=where(eflux gt 0. and eflux lt 2.*flux) ; 2 is also an arbitrary choice 
					;to get rid of those bad bins
flux=flux(c)
eflux=eflux(c)
eflux_s=eflux_s(c)
dist=dist(c)
nbin=nbin(c)
if !debug eq 1 then stop
end
