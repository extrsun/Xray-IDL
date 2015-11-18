pro chop_peak,listh,listh_err,time,nbin,cntrm,chi2,sigma=sigma
;+
; get rid of bins with small-scale sharp peaks 
;-
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - chop_peak,listh,listh_err,time,nbin,cntrm,chi2'
return
endif
;
badsel=where(listh eq 0,nbadsel)
if nbadsel ne 0 then	remove,badsel,listh,listh_err,time

if n_elements(sigma) eq 0 then sigma=3.
nbadbin=0
for k=0,3 do begin
	sel=where(listh_err ne 0.)
	avg_least,listh(sel),listh_err(sel),cntrm,cntrme
	sel_bad=where(listh gt cntrm+sigma*listh_err,nsel)
	if nsel ne 0 then begin
		remove,sel_bad,listh,listh_err,time
		time_bad=time(sel_bad)
		nbadbin=nbadbin+nsel
	endif
endfor
sel=where(listh_err ne 0,nbin)
print,nbadbin,' bad bins have been chopped off'
avg_least,listh(sel),listh_err(sel),cntrm,cntrme
print,'nbin, cntrm = ',nbin,cntrm
chi2=total((listh(sel)-cntrm)*(listh(sel)-cntrm)/(listh_err(sel)*listh_err(sel)))
print,'chi^2,nbin = ',chi2,nbin
;
end