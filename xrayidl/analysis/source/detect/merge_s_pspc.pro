pro merge_s,cra,cdec,sra,sdec,text,blow=blow,bhigh=bhigh,radius=radius $
,sfrac=sfrac,fname=fname,sradius=sradius,dis=dis,ston=ston,dfac=dfac,rfix=rfix

; get a rough estimate of error circle for each source
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg
dis=sqrt(xp*xp+yp*yp)*(!size_pixel/60.)
if n_elements(radius) ne 0 then begin ;any vectors passed on later need to be
	sel=where(dis le radius)	; included here!!!!
	text=text(sel)
	sra=sra(sel)
	sdec=sdec(sel)
	ston=ston(sel)
	dis=dis(sel)
endif
if n_elements(rfix) ne 0 then begin
	core=dis*0.+rfix 
endif else begin
 if !instr eq 'p' then begin
	detect_params,dis,core,blow=blow,bhigh=bhigh,gaus=gaus $
	,perclimit=sfrac,psffile=fname
 endif else begin 
	if !instr eq 'h' then begin
	core=(2.35*0.5*(0.74^2+1.0+(1.3+0.0205*dis^2.349)^2)^0.5 > 3.)/60.
	;the 50% radius of the RHRI (p13 of the Guest Observing prog) 
	; in units of pixels
	endif else stop,'which instrument is this?'
 endelse
if n_elements(dfac) ne 0 then core=dfac*core
endelse
spixmin=(!size_pixel/60.)^2
score=core^2/spixmin
if n_elements(sradius) ne 0 then sr=sradius^2/spixmin
; exclude duplicate sources
ns=n_elements(sra)
for k=ns-1,1,-1 do begin 
	;assuming that data quality decreases with k
	trans_dist,sra(k),sdec(k),sra(0:k-1),sdec(0:k-1),px,py,/deg
	sep=(px^2+py^2)
	seperr=score(0:k-1) < score(k)
	if n_elements(sradius) ne 0 then seperr=seperr > sr
;	sel= where(sep lt seperr, nsel) ; and (dis(k) ge dis(0:k-1)),nsel)
	sel= where((sep lt seperr) and (ston(0:k-1) ge ston(k)), nsel) 
	if nsel ne 0 then begin
		print,'k= ',text(k),sqrt(sep(sel)),sqrt(seperr(sel))
		print,text(sel)
		remove,[k],text,sra,sdec,dis,ston 
		;sra,text, and dis are to be used in source_merge
	endif
endfor
return
end