pro fft_equal,tt,t1,t2,nbin,ff,psm,psv
nt=n_elements(t1)
nstatus=0
for k=0,nt-1 do begin
	sel=where(tt ge t1(k) and tt lt t2(k),nsel)
	if nsel ne 0 then begin
		tts=tt(sel)
		fftps,tts,nbin,ps,ff,tmin=t1(k),tmax=t2(k)
		if nstatus eq 0 then begin
			psv=fltarr(long(nbin/2)+1,nt-k)
			nstatus=1
		endif 
		psv(*,k)=ps
	endif
endfor
psm=total(psv,2)
;stop
return
end