pro getobievr,kbeg,kend,rate1,rate2,rate3,rate4,rate5
;
;  procedure to get intervals of nonzero event rates
;  may choose up to 5 event rates
;
npar = n_params(0)
rsum = abs(rate1)
if (npar ge 4) then rsum = rsum + abs(rate2)
if (npar ge 5) then rsum = rsum + abs(rate3)
if (npar ge 6) then rsum = rsum + abs(rate4)
if (npar ge 7) then rsum = rsum + abs(rate5)
;
kmin = min(where(rsum ne 0))
kmax = max(where(rsum ne 0))
rsum = smooth(float(rsum),31)
ind = where(rsum le 0.5)
rsum(ind) = ind*0
kup=where( (rsum gt 0) and (shift(rsum,1) eq 0))
kdown=where( (rsum eq 0) and (shift(rsum,1) gt 0))
;
nup = n_elements(kup)
kbeg = kup
kbeg(0) = (kup(0)<kmin)>0
ndown = n_elements(kdown)
;if (ndown gt 1) then kend=[kdown(1:*),kmax] else kend = [kdown(0)>kmax]
if (ndown gt 1) then kend=[kdown(0:ndown-2),kdown(ndown-1)>kmax] else $
  kend = [kdown(0)>kmax]
;      
return       ;pro getobievr
end
