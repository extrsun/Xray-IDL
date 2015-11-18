pro tp_model,ta,tp,pv,sv,asv,pvd=pvd
;-
; calculate the expected accumulated distribution, based on a normalized 
; series of observing time intervals 
; ta - 2xNt array of time intervals (start and stop time)
; pv,sv - output phase and distribution vectors 2*Nt+2 elements each, 
; 	including phase = 0 and 1 at the ends, whereas sv 
; 	monotonically increases from 0 to total(ta(1,*)-ta(0,*)) 
;*Note: 
;	The value of the accumulated function at any phase can be calculated
;	by interplating the output sv as a discrete function of pv 
; written by wqd, 4/9/00
;+
if n_params() eq 0 then begin
print,'CALLING SEQ ---  tp_model,ta,tp,pv,sv,asv,pvd=pvd
return
endif
sz=size(ta)
nsv=sz(4)

tpa=float(ta mod 1)
ss=sort(tpa)
pv=[tpa(ss),1.] ;locations of all ta elements
taloc=lonarr(nsv)
taloc(ss)=lindgen(nsv) ;the array positions of ta elements in pv

;print,'total ta = ',total((ta(1,*)-ta(0,*)))
ft=total(long((ta(1,*)-ta(0,*)))) 
	;total number of phases fully covered by the time intervals 
sv=replicate(ft,nsv+1)

for k=0,sz(2)-1 do begin
 pvilo=taloc(k*2) ;start with 1 bin right
 pvihi=taloc(k*2+1) 
 if pvihi lt pvilo then begin
	sv(pvilo+1:nsv)=sv(pvilo+1:nsv)+1.
	sv(0:pvihi)=sv(0:pvihi)+1.
 endif else begin
	sv(pvilo+1:pvihi)=sv(pvilo+1:pvihi)+1.
 endelse
;print,sv
endfor
asv=fltarr(nsv+1)
asv(0)=sv(0)*pv(0)
for k=1,nsv do begin
	asv(k)=asv(k-1)+sv(k)*(pv(k)-pv(k-1))
endfor
;print,'max(asv) = ',asv(nsv)

if n_elements(pvd) eq 0 then pvd=(findgen(9.)+1.)*0.1
tabinv,[0.,pv],pvd,eind
asvd=interpolate([0.,asv],eind)
tp=[asvd,asv(nsv)]-[0.,asvd]
return
end