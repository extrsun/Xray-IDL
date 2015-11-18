pro sim_lc,tbeg,tend,ff,ffd,rph,nsim=nsim,nrc=nrc,gaup=gaup,ph=ph
;
; simulate light curves of a pulsar
;
sim_phase,rph,nsim=nsim,nrc=nrc,seed=seed,ph=ph,gaup=gaup
gtimes_nph,tbeg,tend,ff,ffd,phn,nph
seed=seed*randomu(seed) ;apparently there is a correlation between the
; ram generator in sim_phase and the following. So the above is neccessary.
print,'seed= ',seed
rph=rph+double(phn(long(randomu(seed,nrc,nsim)*nph)))
for k=0,nsim-1 do rph(*,k)=rph(sort(rph(*,k)),k)
; converting the phase into arrival time
rph=(2.0d/ff)*rph/(sqrt(1.0+2.0d*ffd/ff^2*rph)+1.0d) ; the form is to avoid
;the calculation of the difference between two big numbers
return
end
