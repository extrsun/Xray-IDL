pro af_model,ta,pv,sv
;-
; calculate the expected accumulated distribution, based a normalized series of
; observing time intervals 
; ta - 2xNt array of time intervals (begining,ending)
; pv,sv - output phase and distribution vectors 2*Nt+2 elements each, 
; 	including phase = 0 and 1 at the ends, whereas sv 
; 	monotonically increases from 0 to total(ta(1,*)-ta(0,*)) 
;*Note: 
;	The value of the accumulated function at any phase can be calculated
;	by interplating the output sv as a discrete function of pv 
; written by wqd, 4/9/00
;+
sz=size(ta)
nsv=sz(4)+1

tpa=ta mod 1
ss=sort(tpa)
pv=[0.,tpa(ss),1.] ;locations of all ta elements
taloc=lonarr(sz(4))
taloc(ss)=lindgen(nsv-1)+1 ;the array positions of ta elements in pv

ft=total(long((ta(1,*)-ta(0,*)))) 
	;total number of phases full covered by the time intervals 
if ft ne 0 then sv=ft*pv else sv=0.

for k=0,sz(2)-1 do begin
 pvilo=taloc(k*2)+1 ;1 right offset
 pvihi=taloc(k*2+1) 
 if pvihi lt (pvilo-1) then begin
	sv(pvilo:nsv)=sv(pvilo:nsv)+pv(pvilo:nsv)-pv(pvilo-1)
	sv(1:pvihi)=sv(1:pvihi)+pv(1:pvihi)
	sv(pvihi+1:nsv)=sv(pvihi+1:nsv)+pv(pvihi) 
print,'t1',k,sv
 endif else begin
	sv(pvilo:pvihi)=sv(pvilo:pvihi)+pv(pvilo:pvihi)-pv(pvilo-1)
 	sv(pvihi+1:nsv)=sv(pvihi+1:nsv)+(pv(pvihi)-pv(pvilo-1)) 
print,'t2',k,sv
 endelse
endfor
stop
return
end