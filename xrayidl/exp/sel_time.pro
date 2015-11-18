pro sel_time,listh,time,time_bad,step=step,emin=emin,emax=emax,tfile=tfile
;
if n_params() eq 0 then begin
	print,'CALLING SEQUENCE - sel_time,listh,time,time_bad,step=step,emin=emin'
	print,',emax=emax,tfile=tfile'
	return
endif
if n_elements(step) eq 0 then step=5
if n_elements(emin) eq 0 then emin=8
if n_elements(emax) eq 0 then emax=201
if n_elements(tfile) eq 0 then tfile=!seq_no+'_vg0202.dat'
;
getlistimage,list,emin=emin,emax=emax,tfile=tfile
list=list.time
listh=histogram(list,binsize=step)
nbin=n_elements(listh)
time=min(list)+lindgen(nbin)*step+0.5*step
sel=where(listh gt 0)
listh=listh(sel)
time=time(sel)
;
for k=0,2 do begin
	countm=avg(listh)
	sel_bad=where(listh gt countm+3*sqrt(listh),nsel)
	if nsel ne 0 then begin
		remove,sel_bad,listh,time
		time_bad=time(sel_bad)
	endif
endfor
print,'nbin, countm = ',n_elements(time),countm
end