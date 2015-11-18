pro median_comp,cb,tb,tbs,frac,mf,lmin=lmin,sf=sf
if n_elements(sf) eq 0 then sf=0.
if n_elements(lmin) eq 0 then lmin=1
cbo=cb & tbo=tb & tbso=tbs
image_comp3,cb,tb,tbs,frac=frac
image_median,3,imdiv(cb-sf*tbs,tbs),tb,tbs,mf,lmin=lmin,binmin=9
cb=cbo & tb=tbo & tbs=tbso
return
end