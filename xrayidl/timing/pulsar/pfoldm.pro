pro pfold,time,fmin,fmax,fdot,pmin=pmin,pmax=pmax,pdot=pdot,phd=phd $
,prob=prob,phase=phase,fv=fv,dv=dv,fbest=fbest,dmax=dmax,tmin=tmin $
,plotoff=plotoff,proc_min=proc_min,ddf=ddf,ffar=ffar,tfar=tfar
;-
; Using the Reyleigh test to search for periodic signal.
; written by wqd, April, 1996
; time - a list of photon arriving time
; fmin, fmax - the lower and upper limits of the frequency to be searched
; fdot - the assumed frequency derivative
; pmin, pmax - lower and upper limits of the period to be searched. If set,
; 	fmin and fmax will not be used
;
; pdot - period derivative. If set, fdot will not be used.
; phd - the delta phase change of the maximum time bin, def = 0.25
; prob - minimum probability of the search criterion to be used for outputing
; phase - output phases of the photon arriving time
; fv - vector containing frequency values with probability less than prob
; dv - containing Z values
; fbest - frequency with lowest statistical probability 
; dmax - containing the Z vector
; tmin - minimum time of the observation.
; plotoff - if set, no plot
;
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- pfold,time,fmin,fmax,fdot,pmin=pmin,pmax=pmax'
print,',pdot=pdot,phd=phd,prob=prob,phase=phase,fv=fv,dv=dv'
print,',fbest=fbest,dmax=dmax,plotoff=plotoff,proc_min=proc_min'
return
endif
if n_elements(phd) eq 0 then phd=0.25
if n_elements(prob) eq 0 then prob=0.01 ;0.00135 ;3 sigma
nc=n_elements(time)
zth=-alog(prob)*nc


if n_elements(pmin) ne 0 then fmax=1.0d/pmin
if n_elements(pmax) ne 0 then fmin=1.0d/pmax
fac=1.0d
if n_elements(pdot) ne 0 then hfdot=-0.5d*pdot*fmin^2*time else begin
	hfdot=0.5d*fdot*time
;	if n_elements(ddf) ne 0 then hfdot=hfdot+ddf*time^2/6.0d
	if n_elements(ffar) ne 0 then begin
		hfdot=hfdot+(ffar-fdot*tfar)/3.0d*(time/tfar)^2
		fac=1.0-(time/tfar)^2/3.0d
	endif
	;now only for pdot=0 case
endelse
if n_elements(tmin) eq 0 then tmin=min(time)
df=phd/(time(nc-1)-tmin)
nf=nint((fmax-fmin)/df)
print,'probth, zth*nc, nc, df, nf = '
print,prob,zth,nc,df,nf
dv=fltarr(nf+1)
fv=fmin+df*dindgen(nf+1)
mtime=time*(2.*!dpi)

for k=0,nf do begin
	ff=fv(k)
;	if n_elements(pdot) ne 0 then $
;		pht=mtime*(ff+hfdot*(ff/fmin)^2) else $
		pht=mtime*(ff*fac+hfdot)
	dv(k)=(total(cos(pht)))^2+(total(sin(pht)))^2
endfor
dmax=max(dv,zmax)
fbest=fv(zmax)
if keyword_set(proc_min) ne 0 then return
dv=dv/nc
if n_elements(pdot) ne 0 then $
	phase=mtime*(fbest+hfdot*(fbest/fmin)^2) else $
	phase=mtime*(fbest+hfdot)
Z=((total(cos(phase)))^2+(total(sin(phase)))^2)/nc
probmin=exp(-z)
print,'fbest,z,probmin:'
print,fbest,z,probmin,format='(d20.10,2e20.3)'

if keyword_set(plotoff) then return
plot,1.0d/fv-1.0d/fbest,dv ;plot uses only float precision!!!
phase=phase/(2.*!dpi) mod 1
count=histogram(phase,bin=0.05,max=1,min=0.)
bin=(findgen(20)+0.5)/20.
stop,'type .c to ploterr,bin,count,sqrt(count)?'
ploterr,bin,count,sqrt(count)
return 
end