pro tpa_model,fmin,fmax,tao,tpa,fv,nphbin=nphbin,pvd=pvd
;-
; calculate the exposure correction in individual phase bins to be used
; by pfold_t, based on observing time intervals 
;
; fmin, fmax - the lower and upper limits of the frequency to be searched
; tao - 2xNt array of time intervals (begining,ending)
;*Outputs:
; tpa - output exposure correction array
; fv - frequency vector for which the correction is calculated
;*Options:
; nphbin - number of bins per phase with def=10
; pvd - output phase bin size
;*Note: 
;	The value of the accumulated function at any phase can be calculated
;	by interplating the output sv as a discrete function of pv 
;
; written by wqd, 4/9/00
;+
if n_params() eq 0 then begin
print,'CALLING SEQ ---  tp_model,fmin,fmax,tao,tpa,nphbin=nphbin,pvd=pvd
return
endif
if n_elements(nphbin) eq 0 then nphbin=10
if n_elements(phd) eq 0 then phd=1./nphbin
nt=n_elements(tao)
print,'total tao = ',total((tao(1,*)-tao(0,*)))
print,'total duration = ',(tao(nt-1)-tao(0))/3600.,' hrs'
df=phd/(tao(nt-1)-tao(0))
nf=nint((fmax-fmin)/df)
print,'df, nf = ',df,nf
fv=fmin+df*dindgen(nf+1)
pvd=(findgen(nphbin-1)+1.)/nphbin
tpa=fltarr(nphbin,nf)
norm=1./(total(tao(1,*)-tao(0,*))*fv)
for k=0l,nf-1 do begin
	tp_model,tao*fv(k),tp,pvd=pvd
	tpa(*,k)=tp*norm(k)
;stop
endfor
return
end

