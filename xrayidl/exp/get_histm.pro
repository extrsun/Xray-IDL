pro get_histm,listh_t,listh_err_t,time_t,list,step=step,emin=emin,emax=emax, $
tfile=tfile,filter=filter,tmin=tmin,tmax=tmax,inlist=inlist,tint=tint
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_hist,listh,time,step=step,emin=emin'
print,',emax=emax,tfile=tfile,tmin=tmin,tmax=tmax,inlist=inlist,tint=tint'
return
endif
if n_elements(step) eq 0 then step=20. else step=double(step)
if n_elements(emin) eq 0 then emin=11
if n_elements(emax) eq 0 then emax=201
if n_elements(tfile) eq 0 then tfile=!seq_no+'_vg0303.dat' 
if n_elements(tmin) eq 0 then tmin=0 ;to select all time intervals
;
if n_elements(tint) eq 0 then begin
print,'Open time interval file ',tfile
openr,un,!data_dir+tfile,/get_lun
ninterval=0
readf,un,ninterval
tint=lonarr(2,ninterval)
readf,un,tint
free_lun,un
endif

listh_t=fltarr(10000)
listh_err_t=fltarr(10000)
time_t=dblarr(10000)
if keyword_set(inlist) eq 0 then $
 	getlistimage,list,emin=emin,emax=emax,tmin=tmin,tmax=tmax,filter=filter,/xytpionly
time=list.time
kk=0
for k=0,ninterval-1 do begin
	tmin=double(tint(0,k))
	tmax=double(tint(1,k))
	sel=where(time ge tmin and time le tmax,nsel)
	if nsel eq 0 then goto,next
	time_rel=time(sel)
;	print,min(time_rel)-tmin ;the tint(0,K) somehow typically 
;sigificantly less than min(time_rel); the difference is typically 17s
;	tmin=min(time_rel) ;commented out Dec 1 1993, but need to check
	listh=histogram(time_rel,binsize=step,min=tmin,max=tmax)
; the output tmin and tmax are roundup for time_rel > 1.e8; the intrinsic 
; problem with histogram.
	nbin=n_elements(listh)
	laststep=tmax-tmin-(nbin-1)*step
	listh_err=sqrt(listh > 6.)
	listh=listh/step
	listh_err=listh_err/step
	if laststep gt 0.5*step then begin ;the time interval at least half of
						;the step
		listh(nbin-1)=listh(nbin-1)*(step/laststep)
		listh_err(nbin-1)=listh_err(nbin-1)*(step/laststep)
	endif else nbin=nbin-1
	if nbin ne 0 then begin
	time_t(kk:kk+nbin-1+2)=tmin+(lindgen(nbin+2)+0.5)*step
		; a space is added to distinguish different orbits
	listh_t(kk:kk+nbin-1)=listh(0:nbin-1)
	listh_err_t(kk:kk+nbin-1)=listh_err(0:nbin-1)
	kk=kk+nbin+2 ; a space is added to distinguish different orbits
	endif
next:
endfor
time_t=time_t(0:kk-2)
listh_t=listh_t(0:kk-2)
listh_err_t=listh_err_t(0:kk-2)
if !debug eq 1 then stop
end