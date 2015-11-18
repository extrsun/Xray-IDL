pro time_equal,ts,te,t1,t2,tdmin=tdmin,tdch=tdch
;-
; ts -  start and end time of input initial time intervalsfrom obtime_asca
; t1, t2 - output start and end time of equal time intervals
; tdmin - the minimum initial time intervals which will be included
; tdch - the output equal time interval
; written by wqd, 1/27/00
;+

if n_elements(tdmin) eq 0 then tdmin=300

dt=te-ts
sel=where(dt gt tdmin,nsel)
if nsel ne 0 then begin
	dt=dt(sel)
	ee=te(sel)
	ss=ts(sel)
endif else stop,'No interval exceeds tdmin'

if n_elements(tdch) eq 0 then tdch=min(dt)

t1=dblarr(1000)
t2=t1
tid=intarr(1000)
kk=0
t1(kk)=ss(0)
for k=0,nsel-1 do begin
again:	if ee(k)-t1(kk) gt tdmin then begin
		if ee(k)-t1(kk) gt tdch then begin
			t2(kk)=t1(kk)+tdch 
			t1(kk+1)=t2(kk)
			tid(kk)=k
			kk=kk+1
			goto,again
		endif else begin
			t2(kk)=ee(k) ;overlap with the previous interval
			t1(kk)=t2(kk)-tdch ;which may go over the lower limit
			tid(kk)=k
			kk=kk+1
		endelse
	endif 
	if (k+1) lt nsel then t1(kk)=ss(k+1)
;stop
endfor
t1=t1(0:kk-1)
t2=t2(0:kk-1)
tid=tid(0:kk-1)
for k=0,kk-1 do begin
	print,t1(k),t2(k),tid(k)
endfor
return
end
