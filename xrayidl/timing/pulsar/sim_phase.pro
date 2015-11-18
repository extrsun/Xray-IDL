pro sim_phase,rph,nsim=nsim,nrc=nrc,seed=seed,gaup=gaup,ph=ph
;+
; generate random counts in phase according to an a
if n_elements(nsim) eq 0 then nsim=1
if n_elements(gaup) ne 0 then begin
	if n_elements(nrc) eq 0 then $
		stop,'please give the number of random counts (nrc): '
	rph=randomn(seed,nrc,nsim)*gaup(1)+gaup(0)
	noutind=where(rph lt 0. or rph gt 1.,nout)
	if nout ne 0 then begin 
		rph(noutind)=randomu(seed,nout)	;approximate treatment
		print,nout, ' is out of the 0-1 range, randomly reassigned'
	endif
endif else begin
	if n_elements(ph) ne 0 then begin
		if n_elements(nrc) eq 0 then nrc=n_elements(ph)
		rph=ph(long(randomu(seed,nrc,nsim)*n_elements(ph)))
	endif else begin
		if n_elements(nrc) eq 0 then $
		stop,'please give the number of random counts (nrc): '
		rph=randomu(seed,nrc,nsim)
	endelse
endelse
if !debug eq 1 then stop
return
end

