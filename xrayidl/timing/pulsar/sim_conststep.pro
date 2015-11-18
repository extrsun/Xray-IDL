pro sim_conststep,nc,rt
;-
; simulate a constant light curve based on input time intervals
; written by wqd, 2/96
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sim_conststep,nc,rt'
return
endif
if n_elements(fname) eq 0 then fname='test.dat'
input_int,timeint,ntimeint,inf=fname
timeint=timeint-timeint(0,0)
tdif=timeint(1,*)-timeint(0,*)
ttime=total(tdif)
tratio=tdif/ttime
atime=dblarr(ntimeint+1)
for k=1,ntimeint do atime(k)=total(tratio(0:k-1))

r=randomu(seed,nc)
r=r(sort(r))
rt=dblarr(nc)
ns0=0
for k=0,ntimeint-1 do begin
	s=where(r(ns0:*) le atime(k+1),ns)
	if ns ne 0 then begin
		rt(ns0:ns0+ns-1)=(r(ns0:ns0+ns-1)-atime(k))*ttime+timeint(0,k)
		ns0=ns0+ns
	endif
endfor
return
end
