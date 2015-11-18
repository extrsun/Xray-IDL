pro setmod07m,pfix,pnames,statnames,parfnames,outname
if n_params(0) eq 0 then begin
 print,' setmod07m,pfix,pnames,statnames,parfnames,outname '
 print,' Setup some stuff to input into XSTATS for mod01 simulations '
 retall
end
npar=2
pfix=intarr(npar) & pfix=[1,1]
pnames=strarr(npar)
pnames(0)=' Power law photon index      '
pnames(1)=' Power law normalization     '
nfiles=1
statnames=strarr(nfiles) & parfnames=statnames
statnames(0)='bba0fk_m7m_t1e5c_200a.stat'
parfnames(0)='bba0fk_m7m_t1e5c_200a.pars'
outname='bba0fk_m7m_t1e5c_200a'
return
end
