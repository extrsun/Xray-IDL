pro pr,time,pmax,pdot,pmin=pmin,phd=phd,prob=prob,phase=phase,pv=pv,dv=dv,fbest=fbest,tmin=tmin
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
; fbest - frequency with lowest statistical probability 
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- pr,time,pmax,dd,pmin=pmin,plot=plot,phd=phd'
print,',prob=prob,phase=phase,pv=pv,dv=dv,tmin=tmin'
return
endif
if n_elements(prob) eq 0 then prob=0.01 ;0.00135 ;3 sigma
if n_elements(pmin) eq 0 then pmin=0.01
if n_elements(tmin) eq 0 then tmin=time(0)
nc=n_elements(time)
rnc=1./nc
tmid=(tmin+time(nc-1))*0.5
mtime=time-tmid
tmax=mtime(nc-1)
zth=-alog(prob)*nc
plot=1
dv=[-999]
pv=[-999]
if n_elements(phd) eq 0 then phd=0.25
rmt=phd/tmax
print,'probth, zth*nc, nc, dp, ndp = '
print,prob,zth,nc,(pmax+pmin)^2/4.*rmt,(pmax-pmin)*4./rmt/(pmax+pmin)^2

mtime=mtime
;mtimes=mtime^2*(2.*!pi)
mtime=mtime*(2.*!pi)
hpdot=0.5d*pdot/(2.*!pi)
rpmin=1./pmin
rp=1./pmax
while rp lt rpmin do begin
;	pht=mtime*rp-(hpdot*rp^2)*mtimes
	pht=mtime*(rp-(hpdot*rp^2)*mtime)
	Z=(total(cos(pht)))^2+(total(sin(pht)))^2
;	print,Z
	if Z gt zth then begin
		pv=[pv,rp]
		dv=[dv,Z]
	endif
	rp=rp+rmt
endwhile

if n_elements(pv) le 1 then stop,'no significant bin'
dmax=max(dv,zmax)
fbest=pv(zmax)
phase=mtime*(fbest-(hpdot*fbest^2)*mtime)
pht=phase
Z=((total(cos(pht)))^2+(total(sin(pht)))^2)*rnc
probmin=exp(-z)
print,'pbest,z,probmin = ',1./fbest,z,probmin
pv=pv(1:*)
dv=dv(1:*)
plot,1./pv,dv
phase=phase/(2.*!pi) mod 1
count=histogram(phase,bin=0.05,max=1,min=0.)
bin=(findgen(20)+0.5)/20.
stop,'type .c to ploterr,bin,count,sqrt(count)?'
ploterr,bin,count,sqrt(count)
return 
end