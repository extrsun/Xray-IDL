pro merge_s,cra,cdec,sra,sdec,text,psffile=psfile,radius=radius $
,sfrac=sfrac,sradius=sradius,dis=dis,dfac=dfac,rfix=rfix,psfwing=psfwing,cntr=cntr,perr=perr

; get a rough estimate of error circle for each source
if n_elements(cra) ne 0 then begin
	trans_dist,cra,cdec,slist.ra,slist.dec,xp,yp,/deg
	dis=sqrt(xp*xp+yp*yp)*(!size_pixel/60.)
endif
if n_elements(radius) ne 0 then begin 
	sel=where(dis le radius,nsel)	
	if nsel eq 0 then begin
		print,'no source within the radius!!!'
		return
	endif
	slist=slist(sel)
endif
if n_elements(srch) eq 0 then srch=1
case srch of
	1: rs=slist.perr 
	2: psf_params,dis,rs,cntr $
		,perclimit=sfrac,psffile=psffile,psfwing=psfwing
	else: rs=dis*0.+rfix ;rfix is needed
endcase
if n_elements(dfac) ne 0 then rs=dfac*rs
srs=rs^2
if n_elements(sradius) ne 0 then sr=sradius^2 ;systematic error

; exclude duplicate sources
ns=n_elements(sra)
rsel=intarr(ns)
for k=ns-1,1,-1 do begin 
	;assuming that data quality decreases with k
	trans_dist,sra(k),sdec(k),sra(0:k-1),sdec(0:k-1),px,py,/deg
	sep=px^2+py^2
	seperr=srs(0:k-1)+srs(k)
	if n_elements(sradius) ne 0 then seperr=seperr+sr ;systematic error
	sel= where(sep lt seperr, nsel) 
	if nsel ne 0 then begin
		print,'k= ',text(k)
		print,sqrt(sep(sel)),sqrt(seperr(sel))
		print,text(sel)
		rsel(k)=1
		;sra,text, and dis are to be used in source_merge
	endif
endfor
sel=where(rsel eq 1,nsel)
if nsel ne 0 then remove,sel,text,sra,sdec,dis
return
end