pro get_time,nc,npc,time,frac
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_time,nc,npc,time,frac'
return
endif
y=long(randomu(seed,nc)*npc)
get_ram,nc,ph,frac,seed=seed
time=float(y)+ph
return
end

