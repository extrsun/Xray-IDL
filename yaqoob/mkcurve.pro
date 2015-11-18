pro mkcurve,time,tint,binsize,nbins,tcen,tbounds,teff,ctspsec,ctserr,plc=plc,refbnds=refbnds
if n_params(0) eq 0 then begin
  print,' MKCURVE,time,tint,binsize,nbins,tcen,tbounds,teff,ctspsec,ctserr,plc=plc,refbnds=refbnds'
  print,' make a light curve from a photon list '
  print,' ** INPUTS ** : '
  print,' TIME	 	 - list of event times '
  print,' TINT(*,2) 	 - good time intervals array '
  print,' BINSIZE 	 - bin width in seconds '
  print,' REFBNDS	 - array(2) use these start and stop times '
  print,'		   for lightcurve if set'
  print,' ** OUTPUTS ** '
  print,' NBINS 	 - total number of bins in output light curve '
  print,' TCEN 	 - centre of time bins '
  print,' TBOUNDS(*,2) - lower and upper times of bins '
  print,' TEFF 	 - half of the effective exposure for each bin '
  print,' CTSPEC 	 - counts per second '
  print,' CTSERR	 - Poisson error on ctspsec '
  print,' PLC		 - plot IDL lightcurve if set '
  retall
end
np=(size(time))(1)
if n_elements(refbnds) eq 0 then begin
 	tmin = min(time) & tmax=max(time)
endif else begin
	tmin=refbnds(0) & tmax=refbnds(1)
endelse
print,' MIN and MAX times',tmin,tmax
nbins=long((tmax-tmin)/binsize)+1l
print,'NBINS ',nbins
tbounds=dblarr(nbins,2) & tcen=dblarr(nbins) & teff=tcen 
ctspsec=fltarr(nbins) & ctserr=ctspsec
tbounds(*,0)=tmin+binsize*findgen(nbins) & tbounds(*,1)=tbounds(*,0)+binsize
tcen=(tbounds(*,0)+tbounds(*,1))/2.0
;dummy array for computing exposure of each bin
tint_bin=dblarr(1,2)
;now compute the proper ct-rate for each bin
for j=0l,nbins-1 do begin
  wbin=where((time ge tbounds(j,0) and time lt tbounds(j,1)),wc)
  ctspsec(j)=float(wc)
  if ctspsec(j) gt 0.0 then begin
	tint_bin(0,0)=tbounds(j,0) & tint_bin(0,1)=tbounds(j,1)
	combtint,tint_bin,tint,tout & ntout=(size(tout))(1)
	if ntout gt 0 then begin
	  for k=0l,ntout-1 do teff(j)=teff(j)+(tout(k,1)-tout(k,0))
	  ctserr(j)=sqrt(ctspsec(j))/teff(j)
	  ctspsec(j)=ctspsec(j)/teff(j)
	endif
  endif
  if ctspsec(j) le 0.0 then ctspsec(j) = -10.0
endfor
teff=0.5*teff
loc=where(ctspsec gt 0.0)
pcts = ctspsec(loc) & pctserr=ctserr(loc) & ptcen=tcen(loc)
if keyword_set(plot) then begin
window,0
ploterr,ptcen,pcts,pctserr,psym=1
endif
return
end
