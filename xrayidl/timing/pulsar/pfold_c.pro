pro pfold_c,time,fmin,fmax,fdot,pdot=pdot,phd=phd $
,prob=prob,phase=phase,fv=fv,dv=dv,fbest=fbest,dmax=dmax,df=df,tmin=tmin $
,plotoff=plotoff,proc_min=proc_min,ddf=ddf,cdv=cdv $
,filter=filter,efilter=efilter,ffix=ffix,kind=kind
;-
; Using the Reyleigh test to search for periodic signal.
; written by wqd, April, 1996
; time - a list of photon arriving time
; fmin, fmax - the lower and upper limits of the frequency to be searched
; fdot - the assumed frequency derivative
;
; pdot - period derivative. If set, fdot will not be used.
; phd - the delta phase change of the maximum time bin, def = 0.25
; prob - minimum probability of the search criterion to be used for outputing
; phase - output phases of the photon arriving time
; fv - vector containing frequency values with probability less than prob
; dv - containing Z values
; fbest - frequency with lowest statistical probability 
; dmax - containing the Z vector
; df - the size of frequency step (delta frequency).
; tmin - minimum time of the observation, if df is set, tmin is not used.
; plotoff - if set, no plot
; filter - a filter used to smooth dv
; efilter - one sigma error of off-peak region in smooth dv (calculated 
;		previously)
; cdv - smoothed dv
;
; modified to use Von Mires profile to mimic a pulse profile (16ms pulsar)
; the adevatage has however not really revealed yet. More test is needed.
; wqd 3/19/00
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- pfold_c,time,fmin,fmax,fdot,pmin=pmin,pmax=pmax'
print,',pdot=pdot,phd=phd,prob=prob,phase=phase,fv=fv,dv=dv'
print,',fbest=fbest,df=df,dmax=dmax,plotoff=plotoff,proc_min=proc_min,plotoff'
return
endif
if n_elements(phd) eq 0 then phd=0.25
nc=n_elements(time)

if n_elements(df) eq 0 then begin
	if n_elements(tmin) eq 0 then tmin=min(time)
	df=phd/(time(nc-1)-tmin)
endif

if n_elements(filter) ne 0 and keyword_set(ffix) eq 0 then begin
	edge=fix(n_elements(filter)/2)
	fmin_n=fmin-edge*df
	fmax_n=fmax+edge*df
endif else begin
	fmin_n=fmin
	fmax_n=fmax
	edge=0
endelse
nf=nint((fmax_n-fmin_n)/df)
print,'nc, df, nf = ',nc,df,nf

if n_elements(pdot) ne 0 then hfdot=-0.5d*pdot*fmin_n^2*time else begin
	hfdot=0.5d*fdot*time
	if n_elements(ddf) ne 0 then hfdot=hfdot+ddf*time^2/6.0d
endelse

dv=fltarr(nf+1)
fv=fmin_n+df*dindgen(nf+1)
if n_elements(nphbin) eq 0 then nphbin=nint(1./phd)
phbinsz=1./nphbin
mphc=nc/float(nphbin)
;------------
if N_elements(kindx) eq 0 then kind=10.
if n_elements(ftt_pp) eq 0 then begin
	gg=exp(cos(2.*!pi*findgen(nphbin)/float(nphbin))*kind)
	gg=gg/total(gg)-1./nphbin
	fft_pp=fft(gg,-1)
endif
;--------------------
for k=0l,nf do begin
	ff=fv(k)
	if n_elements(pdot) ne 0 then $
		pht=time*(ff+hfdot*(ff/fmin_n)^2) else $
		pht=time*(ff+hfdot)
;	dv(k)=(total(cos(pht)))^2+(total(sin(pht)))^2
;	dv(k)=total((histogram(pht mod 1,bin=phbinsz)-mphc)^2)
	cc=float(fft(fft(histogram(pht mod 1,bin=phbinsz),-1)*fft_pp,1))
	dv(k)=max(cc)
endfor
if n_elements(filter) ne 0 then begin
	cdv=convol(dv-nphbin+1.,filter)
	dmax=max(cdv,zmax)
endif else dmax=max(dv,zmax)

fbest=fv(zmax)

if keyword_set(proc_min) ne 0 then return
dv=dv/mphc
if n_elements(pdot) ne 0 then $
	phase=time*(fbest+hfdot*(fbest/fmin_n)^2) else $
	phase=time*(fbest+hfdot)
phase=phase mod 1
count=histogram(phase,bin=phbinsz)
z=total((count-mphc)^2)/mphc-(nphbin-1)
if n_elements(filter) ne 0 then probmin=z/efilter else $
	probmin=z/sqrt(2.*(nphbin-1))
	
print,'fbest,z,probmin:'
print,fbest,z,probmin,format='(d20.10,2e20.3)'

if keyword_set(plotoff) then return
;plot,1.0d/fv-1.0d/fbest,dv ;plot uses only float precision!!!
if n_elements(filter) ne 0 then begin
	cfv=fv(edge:nf-edge-1)
	cdv=cdv(edge:nf-edge-1)/mphc
	plot,cfv-fbest,cdv 
endif else $
	plot,fv-fbest,dv ;plot uses only float precision!!!
;phase=phase/(2.*!dpi) mod 1

if keyword_set(plotoff) eq 0 then begin
	bin=(findgen(nphbin)+0.5)/nphbin
	stop,'type .c to ploterr,bin,count,sqrt(count)?'
	ploterr,bin,count,sqrt(count)
endif
if !debug eq 1 then stop,'at the end of pfold_chi'
return 
end