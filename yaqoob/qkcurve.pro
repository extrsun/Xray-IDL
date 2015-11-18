pro qkcurve,time,gti,binsize,nbins,tcen,tbounds,cts,ctserr,wmsk,nmsk
if n_params(0) eq 0 then begin
 print,'qkcurve,time,gti,binsize,nbins,tcen,tbounds,cts,ctserr,wmsk,nmsk'
 print,'Make a quick lightcurve using only full exposure bins'
 retall
end
tmin=min(time) & tmax=max(time)
tim=time-tmin
tmpcts=histogram(tim,binsize=binsize)
nbins=(size(tmpcts))(1) 
print,'Number of bins ',nbins
tbounds=dblarr(nbins,2)
tl=tmin+binsize*dindgen(nbins)
th=tmin+binsize*(dindgen(nbins)+1.0d0)
tbounds(0:nbins-1,0)=tl
tbounds(0:nbins-1,1)=th
tcen=0.5d0*(tbounds(*,0)+tbounds(*,1))
tl=tbounds(0:nbins-1,0) & th=tbounds(0:nbins-1,1)
cts=fltarr(nbins) & ctserr=cts
mask=lonarr(nbins)
ngti=(size(gti))(1)
for k=0l,ngti-1l do begin
 wnz=where((tl ge gti(k,0) and tl le gti(k,1) and th ge gti(k,0) and th le gti(k,1)),nnz) 
 if nnz gt 0 then mask(wnz)=1
endfor
wmsk=where((mask gt 0 and tmpcts gt 0.),nmsk)
if nmsk gt 0 then begin
 cts(wmsk)=tmpcts(wmsk)
 ctserr(wmsk)=sqrt(cts(wmsk))
 cts=cts/binsize
 ctserr=ctserr/binsize
endif
return
end
