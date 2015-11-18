pro pr,time,pmax,pdot,pmin=pmin,phd=phd,prob=prob,phase=phase,pv=pv,dv=dv,pbest=pbest,tmin=tmin
;-
; Using the Reyleigh test to search for periodic signal.
; written by wqd, April, 1996
; time - a list of photon arriving time
; tmin - minimum time of the observation.
; pmax - maximum period to be searched
; dd - period direvative, not included yet
; pmin - minimum period to be searched, def = 0.01
; phd - the delta phase change of the maximum time bin, def = 0.25
; prob - minimum probability of the search criterion to be used for outputing
; phase - output phases of the photon arriving time
; pv - period vector containing periods with probability less than prob
; dv - containing Z values
; pbest - period with lowest statistical probability 
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- pr,time,pmax,dd,pmin=pmin,plot=plot,phd=phd'
print,',prob=prob,phase=phase,pv=pv,dv=dv,tmin=tmin'
return
endif
if n_elements(prob) eq 0 then prob=0.00135 ;3 sigma
if n_elements(pmin) eq 0 then pmin=0.01
if n_elements(tmin) eq 0 then tmin=time(0)
nc=n_elements(time)
rnc=1./nc
tmid=(tmin+time(nc-1))*0.5
mtime=time-tmid
tmax=mtime(nc-1)
zth=-alog(prob)*nc
print,'probth, zth*nc, rnc = ',prob,zth,nc
plot=1
dv=[-999]
pv=[-999]
if n_elements(phd) eq 0 then phd=0.25
rmt=phd/tmax
mtime=mtime
mtimes=mtime^2

rpmin=1./pmin
rp=1./pmax
while rp lt rpmin do begin
;	pht=(mtime*rp-(pdot*rp^2)*mtimes)*(2.*!pi)
	pht=(mtime*rp-(0.5*pdot*rp^2)*mtimes)*(2.*!pi)
	Z=((total(cos(pht)))^2+(total(sin(pht)))^2)
;	print,Z
	if Z gt zth then begin
		pv=[pv,1./rp]
		dv=[dv,Z]
	endif
	rp=rp+rmt
endwhile

if n_elements(pv) le 1 then stop,'no significant bin'
dmax=max(dv,zmax)
pbest=pv(zmax)
rp=1./pbest
phase=mtime*rp-(0.5*pdot*rp^2)*mtimes
pht=phase*(2.*!pi)
Z=((total(cos(pht)))^2+(total(sin(pht)))^2)*rnc
probmin=exp(-z)
print,'pbest,z,probmin = ',pbest,z,probmin
pv=pv(1:*)
dv=dv(1:*)
plot,pv,dv
phase=phase mod 1
count=histogram(phase,bin=0.05,max=1,min=0.)
bin=(findgen(20)+0.5)/20.
stop,'want to ploterr,bin,count,sqrt(count)?'
return 
end