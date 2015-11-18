pro sel_int,time,timeint,ntimeint,listh,dev=dev,filter=filter $
,step=step,emin=emin,emax=emax,tmin=tmin,tmax=tmax,sigma=sigma,tfile=tfile,outfile=outfile,keeppeak=keeppeak,ymax=ymax,ymin=ymin,inlist=inlist,tint=tint,list=list
;+
; chopping off time intervals with enhanced count rates and output the 
; good time intervals into a file (*_actime.dat)
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sel_int,time,timeint,ntimeint,listh,dev=dev,filter='
print,',step=step,emin=emin,emax=emax,tmin=tmin,tmax=tmax'
print,',sigma=sigma,tfile=tfile,outfile=outfile,keeppeak=keeppeak,ymax=ymax,ymin=ymin,list=list'
return
endif
;
print,'obtain the count histogram and the corresponding time sequence'
;
if !instr eq 'h' then begin
	emin=0 & emax=0
endif
if n_elements(tfile) eq 0 then tfile=!seq_no+'_actime.dat'
get_histm,listh_o,listh_err_o,time_o,list,step=step,emin=emin,emax=emax,tfile=tfile $
,filter=filter,tmin=tmin,tmax=tmax,inlist=inlist,tint=tint
;
answer = 'Y'
yesno, answer
while (answer eq 1) do begin
;
listh=listh_o
listh_err=listh_err_o
time=time_o
print,'chopping off the bad time intervals'
;stop,'before chopping'
chop_int,listh,listh_err,time,dev=dev,ymax=ymax,ymin=ymin
;
if keyword_set(keeppeak) eq 0 then begin
	print,'chopping off peaks'
	chop_peak,listh,listh_err,time,nbin,cntrm,chi2,sigma=sigma
;endif else  begin
;	sel=where(listh eq 0,nsel) ;remove those bins between two orbits
;	if nsel ne 0 then remove,sel,listh,listh_err,time
;endelse 
endif
sel=where(listh eq 0,nsel) ;remove those bins between two orbits
; and bad time intervals without actually counts (i.e., rp150010)
; wqd, Dec. 1, 1993
if nsel ne 0 then remove,sel,listh,listh_err,time


;
stop,'before the final plotting. To continue, type .c'
ploterr,listh,listh_err

print,'get the selected  time intervals: '
get_interval,step,time,timeint,ntimeint
;
print,'Do you want to recalculate the time intervals? y or n: '
answer='Y'
read,answer
yesno,answer
endwhile
	
output_int,timeint,outfile=outfile
;
end