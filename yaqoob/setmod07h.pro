pro setmod07h,pfix,pnames,statnames,parfnames,outname
if n_params(0) eq 0 then begin
 print,' setmod07h,pfix,pnames,statnames,parfnames,outname '
 print,' Setup some stuff to input into XSTATS for mod01 simulations '
 retall
end
npar=3
pfix=intarr(npar) & pfix=[1,1,1]
pnames=strarr(npar)
pnames(0)=' Column density (1E22 /cm2)  '
pnames(1)=' Power law photon index      '
pnames(2)=' Power law normalization     '
nfiles=1
statnames=strarr(nfiles) & parfnames=statnames
statnames(0)='bba0fk_m7h_t1e5c_200a.stat'
parfnames(0)='bba0fk_m7h_t1e5c_200a.pars'
outname='bba0fk_m7h_t1e5c_200a'
return
end
