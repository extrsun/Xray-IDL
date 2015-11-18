pro fft_mult,tt,dt,fr,psdm,ntdiv=ntdiv,nfrlowr=nfrlowr,tmax=tmax
;-
; calculate fft and pds by dividing a long light curve into individual
; segments.
; tt - light curve
; dt - delta t for the lowest period of 2*dt
; fr - output frequency vector
; psdm - the weighted mean psd 
; tintv - pairs of time intervals as is in *_actime.dat
; 	the pairs must have an equal length to have equal frequency bins
; ntdiv - number of division of the light curve if tintv is not provided
;		the crudest division
; nfrlowr - the number of lower frequency bins to be removed in the final
;		output, def = 5
; written by wqd, 2/96
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - fft_mult,tt,dt,fr,psdm,tintv=tintv,ntdiv=ntdiv'
print,',nfrlowr=nfrlowr'
return
endif
if n_elements(nfrlowr) eq 0 then nfrlowr=5
nsth=20 ; minimum counts for calculating the fft
if n_elements(tintv) eq 0 then begin
	if n_elements(tmax) eq 0 then tmax=max(tt)
	if n_elements(ntdiv) eq 0. then stop,'no ntdiv is given'
	tlength=tmax/ntdiv
	tintv=dblarr(2,ntdiv)
	tintv(0,*)=dindgen(ntdiv)*tlength
	tintv(1,*)=tintv(0,*)+tlength
endif
sel=lonarr(2,ntdiv)
tintvn=tintv*0.0d
ns0=0
kk=0
for k=0,ntdiv-1 do begin
	s=where(tt ge tintv(0,k) and tt lt tintv(1,k),ns)
	if ns ge nsth then begin
		sel(*,kk)=[0,ns-1]+ns0
		tintvn(*,kk)=tintv(*,k)
		kk=kk+1
	endif 
	ns0=ns0+ns
endfor
ntdiv=kk

for k=0,ntdiv-1 do begin
	th=byte(histogram(tt(sel(0,k):sel(1,k)), $
		min=tintvn(0,k),max=tintvn(1,k),bins=dt))
	if k eq 0 then begin
		nbin=n_elements(th)
		nbin=(nbin/2)*2
		if nbin gt 2^22 then stop,'nbin = ',nbin, $
				' really want to go ahead'	
		nbinout=nbin/2-nfrlowr
		psdv=fltarr(nbinout,ntdiv)
		ww=fltarr(ntdiv)
		if !debug eq 1 then stop
	endif
	fft_psd,th(0:nbin-1),psd,fr,dt=dt,nfrlowr=nfrlowr
	ww(k)=(nbinout-1.)/total(psd-avg(psd)^2)
	psdv(*,k)=psd*ww(k)
endfor

	psdm=total(psdv,2)/total(ww)
	get_stat,psdm,psdmm,psde
	s=where(psdm ge (2*psde+psdmm),ns)
	plot,/ylog,fr(s),psdm(s)
;	plot,/ylog,fr,psdm
	maxpsd=max(psdm,posi)
	maxfr=fr(posi)
	print,'max psd peak is ',(maxpsd-psdmm)/psde, 'sigma,'
	print,' at period = ',1./maxfr
stop
return
end
		