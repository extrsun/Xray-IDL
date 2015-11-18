pro timsim,tcen,scr,scr_err,binwidths,xmean,ttotal,efficiency,sigma,qdpname=qdpname,samp=samp,sfreq=sfreq,gti=gti
if n_params(0) eq 0 then begin
 print,'timsim,tcen,scr,scr_err,binwidths,xmean,ttotal,efficiency,sigma,qdpname=qdpname,samp=samp,sfreq=sfreq,gti=gti'
 print,'Simulate a lightcurve as a function of binsize '
 print,'and compute the fraction of bin pairs separated by more '
 print,'than SIGMA*(sum of error bars)'
 print,'BINWIDTHS	-array of binwidths in secs'
 print,'XMEAN		-mean count rate of simulated lightcurves'
 print,'TTOTAL		-total length of lightcurves in days'
 print,'EFFICIENCY	-only a fraction EFFICIENCY of the simulated'
 print,'		 light curve bins are accepted'
 print,'SIGMA		- pair sep cond. SIGMA*(sum of error bars)'
 print,'QDPNAME		-output filename'
 print,'SAMP		-sinewave amplitude'
 print,'SFREQ		-sinewave PERIOD'
 retall
end
if n_elements(qdpname) eq 0 then begin
	qdpname=' '
	read,'Enter name of QDP filename for output ',qdpname
endif
openw,1,qdpname
printf,1,'READ SERR 1 2'
printf,1,'skip single'
printf,1,'plot vert'
printf,1,'mark 17 on 1..3'
printf,1,'win 1'
printf,1,'loc 0.1 0.65 0.9 0.95'
printf,1,'view 0 0 0 0'
xmin=min(binwidths)*.9 & xmax=max(binwidths)*1.1
printf,1,'r x ',xmin,xmax
printf,1,'r y 0.1 1.'
printf,1,'la nx off '
printf,1,'la y EPF'
printf,1,'win 2'
printf,1,'loc 0.1 0.35 0.9 0.65'
printf,1,'view 0 0 0 0'
printf,1,'r x ',xmin,xmax
printf,1,'r y 1 1000'
printf,1,'la nx off '
printf,1,'la y N(pairs)'
printf,1,'win 3'
printf,1,'loc 0.1 0.1 0.9 0.35'
printf,1,'view 0 0 0 0'
printf,1,'r x ',xmin,xmax
printf,1,'r y 0 10'
printf,1,'la x BINWIDTH(s) '
printf,1,'la y % deviation from \gp'
dummy=0.0
nb=(size(binwidths))(1)
frac=fltarr(nb) & pairs=fltarr(nb)
fracerr=frac 
if n_elements(gti) gt 0 then gtiz=gti-gti(0,0)
for i=0l,nb-1l do begin
	if n_elements(gti) gt 0 then begin
	 ngti=(size(gti))(1)
	 nbins=long(gtiz(ngti-1,1)/binwidths(i))
	endif else begin
	 nbins=long(ttotal*86400./binwidths(i))
	endelse
	tcen=binwidths(i)*(0.5+findgen(nbins))
	if n_elements(samp) and n_elements(sfreq) gt 0 then begin
	 cmean=binwidths(i)*(xmean + samp*sin(2.*!pi*tcen/sfreq))
	endif else begin
 	 cmean=fltarr(nbins)+xmean*binwidths(i)
	endelse
	cts=poidev(cmean,seed=seed)	
	ctserr=fltarr(nbins) & scr=ctserr & scr_err=ctserr
	wnz=where((cts gt 0.),nwz)
	if nwz gt 0 then ctserr(wnz) =sqrt(cts(wnz))  
	cr=cts/binwidths(i)
	cr_err=ctserr/binwidths(i)
;now only select a fraction of these bins for analysis
	if n_elements(gti) eq 0 then begin
		v=randomu(seed,nbins)
		ws=where((v le efficiency),nws)
	endif else begin
	 for k=0l,ngti-1l do begin
		wt=where((tcen ge gtiz(k,0) and tcen le gtiz(k,1)),nwt)
		if nwt gt 0 then begin 
		 if n_elements(ws) eq 0 then ws=wt else ws=[ws,wt]
		endif
	 endfor
	 if n_elements(ws) gt 0 then nws=(size(ws))(1) else nws=0
	endelse
	if nws gt 0 and nbins gt 1 then begin
	 scr(ws)=cr(ws) & scr_err(ws)=cr_err(ws)
 	 for j=0l,nbins-2l do begin
	  if scr(j) gt 0. and scr(j+1) gt 0. then begin	
		pairs(i)=pairs(i)+1.
		sep=abs(scr(j+1)-scr(j)) 
	 	sepcond=sigma*(scr_err(j)+scr_err(j+1))
;		if sep gt sepcond then frac(i)=frac(i)+1.	
;experiment
		if sep ge sepcond then frac(i)=frac(i)+1.	
	  endif
	 endfor
	endif
	if pairs(i) gt 0. and frac(i) gt 0. then begin
		fracerr(i)=sqrt(frac(i))
		frac(i)=frac(i)/pairs(i)
		fracerr(i)=fracerr(i)/pairs(i)
	endif
;	printf,1,'! Number of pairs tested ',pairs(i)
	printf,1,binwidths(i),dummy,frac(i),fracerr(i)
	print,binwidths(i),dummy,frac(i),fracerr(i)
endfor
printf,1,'NO NO NO NO'
for i=0l,nb-1l do printf,1,binwidths(i),dummy,pairs(i),dummy
printf,1,'NO NO NO NO'
wpz=where((frac gt 0.),nwpz)
devpi=fltarr(nb)
if nwpz gt 0 then begin
 devpi(wpz)=100.*(!pi-(1./(2.*frac(wpz))))/!pi
 for i=0l,nb-1l do printf,1,binwidths(i),dummy,devpi(i),dummy
endif
close,1
return
end
