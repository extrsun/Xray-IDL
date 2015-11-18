;====================================================================
pro prob_poi_gau,ev,cv,probv,poi_nlimit=poi_nlimit
;+ 
;*Name:
;     prob_poi_gau - calculate counting probability within 
;                    a series of count intervals 
;
;*Inputs:
; ev - expected number of counts (scalar)
; cv - count intervals defined by a vector of count boundaries with
;      its size = 1+ the number of intervals
; poi_nlimit - count threshold above which the Poisson distribution is
;              approximated as a Normal distribution (def=36)
; 
;*Outputs:
; probv - the integrated probabilities within individual count intervals
; 
;*Notes:
; The Poisson distribution is discrete. Therefore the probability is
; not continously distributed.
;
; Written by wqd, Oct 19, 2003
;-
if n_params() eq 0 then begin
print,'Calling Seq - prob_poi_gau,ev,cv,probv,poi_nlimit=poi_nlimit'
return
endif 

if n_elements(poi_nlimit) eq 0 then poi_nlimit=36
if ev lt poi_nlimit then begin
    ncv=n_elements(cv)-1 ;number of intervals
    probv=dblarr(ncv)
    icv=fix(cv)
    mincv=min(icv,max=maxcv)
    icvlo=icv(0:ncv-1)+(1-mincv) ;which always at 1
    icvhi=icv(1:*)-mincv
    ss=where(icvhi ge icvlo,nss) ;index contains at least one integer
    if nss gt 0 then begin
        poi_v,ev,mincv,maxcv,prob
        for k=0,nss-1 do begin
            kk=ss(k)
            probv(kk)=total(prob(icvlo(kk):icvhi(kk)))
        endfor
    endif 
endif else begin
    ncv=n_elements(cv)
    prob=errorf((cv-ev)/sqrt(2.0d*ev))
    probv=(prob(1:*)-prob(0:ncv-2))*0.5 ;error function is for both sides
endelse
return
end
