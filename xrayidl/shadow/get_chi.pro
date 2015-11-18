pro get_chi,nhvc,countvc,backvc,timevc,xflux,band,flux,eflux,mflux,chi $
,opfile=opfile,wclimit=wclimit
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_chi,nhvc,countvc,backvc,timevc,xflux,band'
print,',mflux,chi,opfile=opfile,wclimit=wclimit'
return
endif
if n_elements(wclimit) eq 0 then wclimit=10.
;if n_elements(opfile) eq 0 then opfile='~wqd/rosat/shadow/phoct_dat.normal.'
sh_opac,nhvc*0.01,opacity,band,opfile=opfile
mflux=xflux(0)+xflux(1)*exp(xflux(2)*opacity)
ww=1./(countvc > wclimit)
nbin=n_elements(countvc)
countr=countvc-backvc-mflux*timevc
chi=total(countr*countr*ww)
print,'chi^2, nbin-3 = ',chi,nbin-3
flux=imdiv(countvc-backvc,timevc)
eflux=imdiv(sqrt(countvc > wclimit),timevc)
end
