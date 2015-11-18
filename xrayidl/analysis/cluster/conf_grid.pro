pro conf_grid,ff,pp,chn=chn
;+
; still has the problem with the monotonic requirement of the interp.pro
;-
sz=size(ff)
ndim=sz(0)

;clim=fltarr(ndim)
if n_elements(chn) eq 0 then chn=1
case chn of
 	0: clim=1. & 1: clim=2.71 & 2: clim=4. & 3: clim=6.63 
	;68.3, 90, 95.4, and 99% see Press p536
endcase

plo=fltarr(ndim)
phi=plo

ind=indgen(ndim)
ppind=0
fft=ff
for k=1,ndim do begin
	;convert to 2-D array
	nff=fltarr(sz(k),sz(sz(0)+2)/sz(k))
	nff(0:*)=fft

	min_image,nff,minloc,minval
	minminv=min(minval,minminl)
stop
	val=minval(0:minminl)
	linterp,val,pp(ppind:ppind+minminl),clim,plo(k) 
		;lower limit
	val=minval(minminl:sz(k)-1)
	if sor
	linterp,val,pp(ppind+minminl:ppind+sz(k)-1) $
		,clim,phi(k) ;higher limit
	ppind=ppind+sz(k)

	ind=shift(ind,-1)
	fft=transpose(ff,ind)
stop	
endfor
print,'plo = ',plo
print,'phi = ',phi
stop
return
end
