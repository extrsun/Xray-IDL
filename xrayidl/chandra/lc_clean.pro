pro lc_clean,fname,fm,flo,fhi
;+
; plot the output from the program lc_clean.e during the calibration of 
; ACIS data.
; fname - input file name (e.g., lc_dirty.lc)
; fm, flo, fhi - the mean, lower, and upper limits of the count rates of the
; 	 the light curve from the screen output of lc_clean.e
;*example:
;	lc_clean,'lc_dirty.lc',1.088,0.893,1.282
;-
if n_params() eq 0 then begin
print,'Calling Seq. - lc_clean,fname,fm,flo,fhi'
return
endif
openr,un,fname,/get
tt=0.
ff=0.
nbin=10000
time=dblarr(nbin)
flux=fltarr(nbin)
k=0
while not eof(un) do begin
	readf,un,tt,ff
	time(k)=tt
	flux(k)=ff
	k=k+1
endwhile
time=time(0:k-1)
time=time-min(time)
flux=flux(0:k-1)
plot,time,flux,xtit='Time (s)',ytit='Count Rate (counts/s)'
if n_elements(fm) ne 0 then begin
tlohi=minmax(time)
	oplot,tlohi,[1,1]*fm
	oplot,tlohi,[1,1]*flo
	oplot,tlohi,[1,1]*fhi
endif
return
end