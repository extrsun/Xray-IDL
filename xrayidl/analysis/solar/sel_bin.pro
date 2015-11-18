pro sel_bin,counto,backo,expto,count,back,expt,countmin=countmin
if n_elements(countmin) eq 0 then countmin=2
;
fluxm=total(count-back)/total(expt)
print,'1\% of the the mean flux = ',fluxm,' is used as sminset'
sminset=0.01*fluxm
;
count=counto
back=backo
expt=expto
sel=where(expt gt 0. and expt le  countmin/fluxm,nsel)
if nsel ne 0 then begin
	expt(sel)=0.
	back(sel)=0.
	count(sel)=0.
endif
sel=where(expt ne 0.,nsel)
nb=count*0
nb(sel)=1
sel=where(total(nb,2) ge 2),nsel)
if nsel ne 0 then begin
	expt=expt(sel)
	back=back(sel)
	count=count(sel)
endif else stop,'There is no overlapping bins in the array'
;
end

	
