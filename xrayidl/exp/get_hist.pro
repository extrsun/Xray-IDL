pro get_hist,listh,time,time_bad,step=step,emin=emin,emax=emax,tfile=tfile $
,filter=filter,tmin=tmin,tmax=tmax
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_hist,listh,time,time_bad,step=step,emin=emin'
print,',emax=emax,tfile=tfile,tmin=tmin,tmax=tmax'
return
endif
if n_elements(step) eq 0 then step=10
if n_elements(emin) eq 0 then emin=11
if n_elements(emax) eq 0 then emax=201
if n_elements(tfile) eq 0 then begin
	tfile=!seq_no+'_vg0303.dat'
	print,'defaut tfile is used:',tfile
endif
;
getlistimage,list,emin=emin,emax=emax,tmin=tmin,tmax=tmax,tfile=tfile,filter=filter
list=list.time
if keyword_set(tmax) eq 0 and $
	max(list(1:*)-list(0:n_elements(list)-2)) gt 1.e6 then begin
	print,'the histogram will be too large because of a large interval'
	print,'between two separate observations'
	print,'please give tmin and tmax to reduce the total time interval:'
	read,tmin,tmax
endif else begin
	tmin=min(list) & tmax=max(list)
endelse
listh=histogram(list,binsize=step,min=tmin,max=tmax)
nbin=n_elements(listh)
time=min(list)+lindgen(nbin)*step+0.5*step
sel=where(listh gt 0)
listh=listh(sel)
time=time(sel)
end