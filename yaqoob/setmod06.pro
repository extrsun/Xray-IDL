pro setmod06,pfix,pnames,statnames,parfnames,outname
if n_params(0) eq 0 then begin
 print,' setmod06,pfix,pnames,statnames,parfnames,outname '
 print,' Setup some stuff to input into XSTATS for mod01 simulations '
 retall
end
npar=5
pfix=intarr(npar) & pfix=[1,1,1,0,1]
pnames=strarr(npar)
pnames(0)=' Column density (1E22 /cm2)  '
pnames(1)=' R-S Temp kT (keV)           '
pnames(2)=' Abundance                   '
pnames(3)=' Redshift                    '
pnames(4)=' R-S normalization           '
nfiles=1
statnames=strarr(nfiles) & parfnames=statnames
statnames(0)='bba0fk_m06_t1e2_200a.stat'
parfnames(0)='bba0fk_m06_t1e2_200a.pars'
outname='bba0fk_m06_t1e2_200a'
return
end
