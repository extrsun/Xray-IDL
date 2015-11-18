pro timanal,plist,gti,pilo,pihi,binwidths,sigma,frac,fracerr,qdpname=qdpname
if n_params(0) eq 0 then begin
 print,'timanal,plist,gti,pilo,pihi,binwidths,sigma,frac,fracerr,qdpname=qdpname'
 print,'Take a photon list (plist) and an array of bin widths'
 print,'and for each binsize make light curves keeping only '
 print,'complete exposure bins. Step through each bin and note '
 print,'the number of occasions that the count rate in this and '
 print,'the next ADJACENT bin is SIGMA away from this one. Finally'
 print,'store the fraction of the total number of pairs in FRAC.'
 print,'The error, FRACERR, is formed from the Poisson error on the '
 print,'number of bin pairs.'
 print,'The PLIST is first filtered on PI channels PILO and PIHI '
 print,'inclusive.'
 retall
end
print,'Size of PLIST on entry ',(size(plist))(1)
wp=where(plist.pi ge pilo and plist.pi le pihi)
plist=plist(wp)
print,'Size of PLIST after PI filtering ',(size(plist))(1)
openw,1,qdpname
openw,2,'frac'+qdpname
dummy=0.0
printf,1,'READ SERR 1 2'
printf,2,'READ SERR 1 2'
printf,1,'SKIP SINGLE'
printf,2,'SKIP SINGLE'
printf,1,'la x TIME (s)'
printf,2,'la x BINWIDTH (s)'
printf,1,'la y cts/s '
printf,2,'la y Fraction of Bin Pairs Separated'
printf,2,'MARK 17 on 1 2'
ncurves=(size(binwidths))(1)
frac=fltarr(ncurves) & fracerr=fltarr(ncurves)
pairs=frac
sigrms=fltarr(ncurves)
sigrmserr=fltarr(ncurves)
septot=fltarr(ncurves) & seperr=fltarr(ncurves)
halfbins=binwidths/2.
time=plist.time
plc=0
for j=0l,ncurves-1l do begin
 binsize=binwidths(j)
 thalf=halfbins(j) 
 MKCURVE,time,gti,binsize,nbins,tcen,tbounds,teff,ctspsec,ctserr,plc=plc 
 wnz=where((teff eq thalf ),nnz)
 print,'Number of full bins ',nnz 
 printf,1,'!Binsize ',binsize
 printf,1,'!Number of full bins ',nnz
 printf,1,'!Separation condition SIGMA = ',sigma
 if nnz gt 1 then begin
        tmpcts=fltarr(nbins) & tmpctserr=fltarr(nbins)
	tmpcts(wnz)=ctspsec(wnz) & tmpctserr(wnz)=ctserr(wnz)
;mean count rate
	mu=total(tmpcts)/float(nnz)
	printf,1,'! mean count rate ',mu
	sigsq=tmpctserr*tmpctserr
	sigrms(j)=total((tmpcts(wnz)-mu)*(tmpcts(wnz)-mu)-sigsq(wnz))/float(nnz)/mu/mu
	tmperr=((tmpcts(wnz)-mu)*(tmpcts(wnz)-mu)-sigsq(wnz))-sigrms(j)*mu*mu
	sigrmserr(j)=total(tmperr*tmperr)/float(nnz-1) 	
	if sigrmserr(j) gt 0. then begin
	 sigrmserr(j)=sqrt(sigrmserr(j))/mu/mu/sqrt(float(nnz))
	endif
	print,'Var. Amplitude : ',sigrms(j),sigrmserr(j)
	septot(j)=0.0
	seperr(j)=0.0
	for i=0l,nbins-1l do begin
  if tmpcts(i) gt 0. then printf,1,tcen(i),teff(i),tmpcts(i),tmpctserr(i)
          if i lt nbins-1l then begin
            if tmpcts(i) gt 0. and tmpcts(i+1) gt 0. then begin
		pairs(j)=pairs(j)+1.
		sep = abs(tmpcts(i)-tmpcts(i+1))
		sepsq=sep*sep
		septot(j)=septot(j)+sepsq
		seperr(j)=seperr(j)+sepsq*(tmpctserr(i)*tmpctserr(i)+$
tmpctserr(i+1)*tmpctserr(i+1))
		sepcon = sigma*(tmpctserr(i)+tmpctserr(i+1)) 
	 	if sep gt sepcon then frac(j)=frac(j)+1.
	    endif
	  endif
	endfor
  endif	
  if pairs(j) gt 0. and frac(j) gt 0. then begin
	fracerr(j)=sqrt(frac(j))
	frac(j)=frac(j)/pairs(j)
	fracerr(j)=fracerr(j)/pairs(j)
	if septot(j) gt 0. then begin
	septot(j)=sqrt(septot(j)/pairs(j))/mu
	endif
	if seperr(j) gt 0. then begin
	 seperr(j)=2.*sqrt(seperr(j)/pairs(j))/mu
	endif
  endif
  printf,1,'! Number of pairs tested ',pairs(j)
  printf,1,'! Fraction (and error) separated by sigma: ',frac(j),fracerr(j)
  printf,1,'NO NO NO NO '
  printf,2,binwidths(j),dummy,frac(j),fracerr(j)
  print,binwidths(j),dummy,frac(j),fracerr(j)
endfor
printf,2,'!Following is the rms var. amplitude'
printf,2,'NO NO NO NO '
for j=0l,ncurves-1l do printf,2,binwidths(j),dummy,sigrms(j),sigrmserr(j)
printf,2,'NO NO NO NO '
for j=0l,ncurves-1l do printf,2,binwidths(j),dummy,septot(j),seperr(j) 
close,1
close,2
return
end
