pro setmod01_1a,pfix,pnames,statnames,parfnames,outname
if n_params(0) eq 0 then begin
 print,' setmod01_1a,pfix,pnames,statnames,parfnames,outname '
 print,' Setup some stuff to input into XSTATS for mod01 simulations '
 retall
end
npar=6
pfix=intarr(npar) & pfix=[1,1,1,1,1,1]
pnames=strarr(npar)
pnames(0)=' Column density (1E22 /cm2)  '
pnames(1)=' Power law photon index      '
pnames(2)=' Power law normalization     '
pnames(3)=' Gaussian line energy (keV)  '
pnames(4)=' Line sigma width (keV)      '
pnames(5)=' Line intensity [ph/cm2/s]   '
nfiles=2
statnames=strarr(nfiles) & parfnames=statnames
statnames(0)='bba0fk_m01_t1e1_200a.stat'
statnames(1)='bba0fk_m01_t1e1_800a.stat'
parfnames(0)='bba0fk_m01_t1e1_200a.pars'
parfnames(1)='bba0fk_m01_t1e1_800a.pars'
outname='dummya'
return
end
