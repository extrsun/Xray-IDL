pro fftps,tt,nd,ps,ff,tmin=tmin,tmax=tmax
;+
; calculate the power spectrum of the time series
; written by wqd, 10/23.98
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- fftps,tt,nd,ps,ff,tmin=tmin,tmax=tmax'
return
endif

if n_elements(tmin) eq 0 then tmin=min(tt)
if n_elements(tmax) eq 0 then tmax=max(tt)
dt=(tmax-tmin)/double(nd)
ff=bytarr(nd)
loc=long((tt-tmin)/dt)
h=histogram(loc)
ff(lindgen(n_elements(h))+min(loc))=h
loc=0
h=0
ps=fft(ff,-1)
ps=[(abs(ps(0)))^2,(abs(ps(1:nd/2-1)))^2+(abs(reverse(ps(nd/2+1:*))))^2,(abs(ps(nd/2)))^2]
ff=findgen(nd/2+1)/float(tmax-tmin)
return
end
