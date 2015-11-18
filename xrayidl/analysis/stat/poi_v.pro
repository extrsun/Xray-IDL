pro poi_v,mu,mincv,maxcv,prob
;+ 
;*Name:
;     poi_v - calculate the Poisson probability for a series of
;             observed counts
;
;*Inputs:
; mu - expected number of counts (scalar)
; mincv and maxcv - minmum and maximum counts (inclusive) of the the
;               series for which the probability is to be calculated.
; 
;*Outputs:
; probv - the probilities
; 
; Written by wqd, Oct 19, 2003
;-
if n_params() eq 0 then begin
print,'Calling Seq - poi_v,mu,mincv,maxcv,pro'
return
endif 

nbin=maxcv-mincv+1
prob=dblarr(nbin)
if (mu eq 0) then return
lmu=alog(mu)
prob(0)=mincv*lmu-mu-LNGAMMA(mincv+1)
if nbin gt 1 then for k=1,nbin-1 do prob(k)=prob(k-1)+lmu-alog(mincv+k)
prob=exp(prob)

return
end
