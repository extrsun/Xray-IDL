pro remove_ol,lin,lout,radel=radel

ra=lin.ra
dec=lin.dec
ns=n_elements(ra)
raint=(ra(ns-1)-ra(0))*(3600./radel)
ra=(ra-ra(0))*raint
get_posi,ra,loci,kdup,nloci
kk=0
for k=0,nloci do begin
 if kdup(k) ne 0 then begin
	kkhi=kk+kdup-1
	sdec=dec(kk:kkhi)
	sloc=sort(sdec)
	ndec=sdec(sloc)
	defdec=ndec(1:kdup-1)-ndec(0:kdup-2)
	sel=where(defdec lt delth,nsel)
	if nsel ne 0 then begin
		
	kk=kkhi
 endif
endfor
return
end


