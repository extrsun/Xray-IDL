pro fft_multi,tt,t1,t2,nbin,ps,ffm,ffv
nt=n_elements(t1)
nstatus=0
for k=0,nt-1 do begin
	tts=where(tt ge t1(k) and tt lt t2(k),nsel)
	if nsel ne 0 then begin
		fftps,tts,nbin,ps,ff
		if nstatus eq 0 then begin
			ffv=fltarr(n_elements(ps),nt-k)
			nstatus=1
		endif 
		ffv(*,k)=ff
	endif
endfor
ffm=total(ffv,1)
stop
return
end