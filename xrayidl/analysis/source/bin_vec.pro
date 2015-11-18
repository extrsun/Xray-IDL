pro bin_vec,phase,count,time,nbin,crn,crne,tt,pm
;-
; calculate a light curve in phase
; writen by wqd, 1995, Jan 12
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - bin_vec,phase,count,time,nbin,crn,crne,tt,pm'
return
endif
s=sort(phase)
phasen=phase(s)
countn=count(s)
timen=time(s)
nt=n_elements(countn)
;if n_elements(prange) eq 0 then 
prange=[min(phasen),max(phasen)]
step=(prange(1)-prange(0))/float(nbin)
p1=prange(0)+(1+findgen(nbin-1))*step
;p2=p1+step
tabinv_m,phasen,p1,p1ind
;tabinv_m,phasen,p2,p2ind
p1ind=[0,long(p1ind+0.99999)]
p2ind=[p1ind(1:nbin-1)-1,nt-1]
nb=p2ind-p1ind+1 
s=where(nb ne 0,ns)
crn=fltarr(ns)
crne=crn
pm=crn
tt=crn

for nn=0,ns-1 do begin
	n=s(nn)
	tc=total(countn(p1ind(n):p2ind(n)))
	tt(nn)=total(timen(p1ind(n):p2ind(n)))
	pm(nn)=avg(phasen(p1ind(n):p2ind(n)))
	crn(nn)=tc/tt(nn)
	crne(nn)=sqrt(tc)/tt(nn)
endfor
end