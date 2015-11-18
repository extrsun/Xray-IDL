pro time_break,tminv,tmaxv,t1,t2,npcr,step=step,te=te $
,break=break,relax=relax
;-
; break observing time into segments of a fixed step
;
;*INPUTS:
; tminv,tmaxv - vectors containing minimum and maximum timing values for
;		individual time intervals 
; step - step length of the output time intervals (in units of sec)
; te - margin of time (sec) used for removing data at the begining and
;	ending of individual time segements.
; relax - if set, time interval at the end of time segment may be smaller than
;	step
;*OUTPUTS:
; t1, t2 - vectors containing lower and upper limits of time used to 
;		calculated cr and cr_err
; all the above three vectors are in same length
; writen by wqd, May 11, 1994
;+
; 
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_thist,list,tminv,tmaxv,npv,cr,cr_err,t1,t2,npcr,step=step,te=te,break=break,relax=relax'
return
endif
if n_elements(step) eq 0 then step=800. else step=double(step)
if n_elements(te) eq 0 then te=0
t1=dblarr(10000)
t2=dblarr(10000)
time=list.time ;list is supposed be sorted relative to time
xdv=list.dx
ydv=list.dy
ninterval=n_elements(tminv)
if n_elements(npv) eq 1 then npv=replicate(npv,ninterval)
kk=0
for k=0,ninterval-1 do begin
	tmin=double(tminv(k))
	tmax=double(tmaxv(k))
	if keyword_set(relax) ne 0 then nt=nint((tmax-tmin-2*te)/step,/long) $
		;the time interval at least half of the step
	else nt=long((tmax-tmin-2*te)/step) 
	for n=0,nt-1 do begin
		t1(kk)=tmin+n*step+te
		t2(kk)=t1(kk)+step < (tmax-te)
;		print,t1(kk)+step,tmax-te
		kk=kk+1
	endfor
        if kk ne 0 and keyword_set(break) ne 0 then begin
		if t1(kk-1) ne t2(kk-1) then begin
			t1(kk)=t2(kk-1)
			t2(kk)=t1(kk)
		endif
	endif
endfor
kk=kk-1
t1=t1(0:kk-2)
t2=t2(0:kk-2)
if !debug eq 1 then stop
return
end