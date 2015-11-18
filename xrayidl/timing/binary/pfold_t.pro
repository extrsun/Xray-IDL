pro pfold_t,time,fv,tpa,nphbin=nphbin,phd=phd,prob=prob,phase=phase,dv=dv,fbest=fbest,dmax=dmax,df=df,plotoff=plotoff
;-
; Using Chi^2 to search for periodic signal with exposure correction.
; No background correction has been included yet.
;*Inputs:
; time - a list of photon arriving time
; fv - vector containing frequency values with probability less than prob
; tpa - input exposure corrections calculated by tpa_model
;*Options:
; nphbin - number of bins per phase, def = 10.
; phd - output delta phase change of the maximum time bin
; phase - output phases of the photon arriving time
; dv - containing the Chi^2 values
; fbest - frequency with lowest statistical probability 
; dmax - containing the Chi^2 value at fbest
; df - the size of frequency step (delta frequency).
; plotoff - if set, no plot
; 
; written by wqd, Apr 10, 2000
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- pfold_t,time,fv,tpa,nphbin,phd=phd,prob=prob'
print,',phase=phase,dv=dv,fbest=fbest,dmax=dmax,df=df,plotoff=plotoff'
return
endif
if n_elements(nphbin) eq 0 then nphbin=10
nf=n_elements(fv)
dv=fltarr(nf)
phbinsz=1./nphbin
mh=tpa*n_elements(time)
semh=mh > 6. ;assumed minimum count number for gaussian error

; count distribution
ch=fltarr(nphbin,nf)
for k=0l,nf-1 do ch(*,k)=histogram(time*fv(k) mod 1,bin=phbinsz,min=0.,max=1.)

; calculating Chi^2
dv=total((ch-mh)^2/semh,1)
dmax=max(dv,zmax)
fbest=fv(zmax)

if keyword_set(proc_min) ne 0 then return
phase=time*fbest mod 1
count=histogram(phase,bin=phbinsz,min=0.,max=1.)
z=dv(zmax)-(nphbin-1)
probmin=z/sqrt(2.*(nphbin-1))
	
print,'fbest,z,probmin:'
print,fbest,z,probmin,format='(d20.10,2e20.3)'
print,'best period = ',1./fbest/(24.*3600.),' days'

if keyword_set(plotoff) then return
plot,fv-fbest,dv ;plot uses only float precision!!!
if keyword_set(plotoff) eq 0 then begin
	bin=(findgen(nphbin)+0.5)/nphbin
	stop,'type .c to ploterr,bin,count?'
	plot,bin,count,psym=4
	oploterr,bin,mh(*,zmax),sqrt(semh(*,zmax))
	stop,'type .c to ploterr,bin,ratio?'
	plot,bin,count/mh(*,zmax),psym=4
endif
if !debug eq 1 then stop,'at the end of pfold_t'
return 
end