pro fft_psd,ak,psd,fr,dt=dt,noplot=noplot,nfrlowr=nfrlowr
;-
; conduct fft and calculate psd of the transform.
; written by wqd, 2/96
;+ 
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - fft_psd,ak,psd,fr,dt=dt,noplot=noplot,nfrlowr=nfrlowr'
return
endif
if n_elements(nfrlowr) eq 0 then nfrlowr=10
nbin=n_elements(ak)
power2=alog(nbin)/alog(2.)
if fix(power2) ne power2 then stop,'nbin = ',nbin,' is not an integer power of 2'
ak=abs(fft(ak,-1,/overwrite))
if !debug eq 1 then stop
rnbin2=1./float(nbin)^2
;psd=fltarr(nbin/2+1)
;psd(0)=abs(ak(0))^2*rnbin2
;psd(1:nbin/2-1)=(abs(ak(1:nbin/2-1))^2+abs(akhalf)^2)*rnbin2
;psd(nbin/2)=abs(ak(nbin/2))^2*rnbin2

psd=fltarr(nbin/2) ;the zero frequency bin is not included
;psd(0)=ak(0)^2*rnbin2
psd(0:nbin/2-2)=(ak(1:nbin/2-1)^2+(rotate(ak(nbin/2+1:nbin-1),2))^2)*rnbin2
psd(nbin/2-1)=ak(nbin/2)^2*rnbin2

psd=psd(nfrlowr:*)
if n_elements(dt) ne 0 then begin
	fr=(findgen(nbin/2-nfrlowr)+1+nfrlowr)/(nbin*dt)
;	fr=findgen(nbin/2+1)/(nbin*dt)
	if n_elements(noplot) eq 0 then begin
		get_stat,psd,psdm,psde
		s=where(psd ge (2*psde+psdm),ns)
		if ns eq 0 then stop,'there is no psd bin above 2 sigma'
		plot,/ylog,fr(s),psd(s)
	endif
endif
if !debug eq 1 then stop
return
end