pro get_ram,nc,ph,frac,seed=seed
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_ram,nc,ph,frac,seed=seed'
return
endif
xx=findgen(100)/99.
y=randomu(seed,nc)
linterp,yvalue(xx,frac),xx,y,ph
return
end


function yvalue,x,frac
if n_elements(frac) eq 0 then frac=1.
p=2.*!pi
return,x+(1.-cos(p*x))*(frac/p)
end