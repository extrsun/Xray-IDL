pro source_thist,time,tminv,tmaxv,cr,cr_err,t1,t2,step=step,te=te $
,break=break,relax=relax
;+
; calculate count rates as a function of time with user supplied time step
;
;*INPUTS:
; time - time sequence of events
; tminv,tmaxv - vectors containing minimum and maximum timing values for
;		individual time intervals
; step - step length of the output time intervals (in units of sec)
; te - margin of time (sec) used for REMOVING data at the begining and
;	ending of individual (e.g., potentially bad) time segements. Def=0
; relax - if set, time interval at the end of time segment may be smaller than
;	step
;*OUTPUTS:
; cr, cr_err - vectors containing count rates and their errors 
; t1, t2 - vectors containing lower and upper limits of time used to 
;		calculated cr and cr_err
;
; written by wqd, May 11, 1994
; modified to remove the instrument-dependent call of source_cntr. 
; change list into time sequence of events (i.e., list --> time)
; wqd, Sept 8, 2002
;-
; 
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_thist,time,tminv,tmaxv,cr,cr_err,t1,t2,step=step,te=te,break=break,relax=relax'
return
endif

if n_elements(step) eq 0 then step=400. else step=double(step)
if n_elements(te) eq 0 then te=0
cr=fltarr(10000)
cr_err=fltarr(10000)
t1=dblarr(10000)
t2=dblarr(10000)
ninterval=n_elements(tminv)
kk=0
for k=0,ninterval-1 do begin
	tmin=double(tminv(k))
	tmax=double(tmaxv(k))
	if keyword_set(relax) then nt=nint((tmax-tmin-2*te)/step,/long) $
		;the time interval at least half of the step!
	else nt=long((tmax-tmin-2*te)/step) 
	for n=0,nt-1 do begin
		t1(kk)=tmin+n*step+te
		t2(kk)=t1(kk)+step < (tmax-te)
		etime=t2(kk)-t1(kk)
		sel=where(time ge t1(kk) and time lt t2(kk),nsel)
		if nsel eq 0 then begin
				cntr=0.
				cntre=sqrt(10.)/etime
		endif else begin
		 ;source_cntr,xdv(sel),ydv(sel),piv(sel),etime,cntr,cntre
		 cntr=float(nsel)/etime
		 cntre=sqrt(float(nsel))/etime
		endelse
		cr(kk)=cntr
		cr_err(kk)=cntre
		kk=kk+1
	endfor
        if kk ne 0 and keyword_set(break) ne 0 then begin
		if t1(kk-1) ne t2(kk-1) then begin
			t1(kk)=t2(kk-1)
			t2(kk)=t1(kk)
			kk=kk+1		
			; a space is added to distinguish different orbits
		endif
	endif
;stop
endfor
kk=kk-1
t1=t1(0:kk)
t2=t2(0:kk)
cr=cr(0:kk)
cr_err=cr_err(0:kk)
return
end