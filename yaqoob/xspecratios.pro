pro xspecratios,specnames,arfnames,outname=outname
if n_params(0) eq 0 then begin
 print,'xspecratios,specnames,arfnames,outname=outname'
 print,'Take a set of QDP spectral files made in XSPEC '
 print,'and create a QDP file which contains spectral ratios '
 print,' of each spectrum relative to the first '
 print,' The columns of the input QDP files must be :'
 print,'COL 1: energy  COL 2: energy bin half width '
 print,'COL 3: cts/s/(keV)  COL 4: error on count rate '
 print,' The ARF correction is made if ARF files are specified '
 print,'** INPUTS ** '
 print,'SPECNAMES 	- array of input QDP filenames '
 print,'ARFNAMES 	- array of corresponding ARF filenames'
 print,'** OUTPUTS ** '
 print,'OUTNAME		- Name of output QDP file '
 retall
end
nspec=(size(specnames))(1)
if nspec lt 2 then begin
 print,'Must have at least 2 input spectra ! '
 print,' ** ABORTING ** '
 return
endif
if n_elements(arfnames) gt 1 then doarf=1 else doarf = 0
if n_elements(outname) eq 0 then begin
 outname=' '
 read,'Enter name of output QDP file ',outname
endif
chmx=10000l
spectra=fltarr(chmx,nspec) & specerr=spectra
engout=fltarr(chmx)
ratio=fltarr(chmx,nspec-1)
for k=0l,nspec-1 do begin
 readcol,specnames(k),energy,enerr,cts,ctserr
 npts=(size(energy))(1)
 spectra(0:npts-1,k:k)=cts & specerr(0:npts-1,k:k)=ctserr
endfor
dspectrum=spectra(0:npts-1,0:0)
wzd=where((dspectrum le 0),nwzd)
if nwzd gt 0 then dspectrum(wzd)=-1.0
print,npts, ' Raw Channels '
cgrp=1l & bstrt=1l & bend=512l
read,'Enter channel No. to start ratio computation (count from 1)',bstrt
read,'Enter channel No. to end ratio computation ',bend
read,'Enter No. of channels to include per group (default 1)',cgrp
fcgrp=float(cgrp)
nchout=(bend-bstrt+1l)/cgrp 
rem= (bend-bstrt+1l)-cgrp*nchout
print,nchout,' full output channels '
print,'Remainder = ',rem
openw,1,outname
printf,1,'READ SERR 1 2'
printf,1,'SKIP SINGLE '
printf,1,'PLOT VERTICAL'
for i=1l,nspec-1l do begin
 for j=0l,nchout-1l do begin
 	lo = bstrt + j*cgrp & hi = lo+ cgrp -1l
	lo = lo-1l & hi=hi-1l
	ecen = total(energy(lo:hi))/fcgrp
	ewid = (energy(hi)+enerr(hi)-energy(lo)+enerr(lo))/2.
	wbz=where((dspectrum(lo:hi) gt 0),nb) & fnb=float(nb)
 if fnb gt 0.0 then rat = total(spectra(lo:hi,i:i)/dspectrum(lo:hi))/fnb else $
  rat=0.0
	rerr=0.0
    for k=lo,hi do begin
	if (spectra(k:k,i:i))(0) gt 0 and (dspectrum(k:k))(0) gt 0 then begin
	rbit = ((specerr(k:k,i:i)/spectra(k:k,i:i))+ $      
	(specerr(k:k,0:0)/dspectrum(k:k)))*$
	(spectra(k:k,i:i)/dspectrum(k:k))
	rerr = rerr+rbit*rbit
	endif
    endfor
	raterr = sqrt(total(rerr*rerr))/fcgrp
	printf,1,ecen,ewid,rat,raterr
 endfor
 printf,1,'no no no no '
endfor
close,1
return
end
