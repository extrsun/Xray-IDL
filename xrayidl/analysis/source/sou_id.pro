pro sou_id,cra,cdec,xl,ol,xsel,osel,nosel,xosep,radius=radius,dfac=dfac,rfix=rfix,rmin=rmin
;+
; Find matchs between x-ray source list and optical objects
;
; cra, cdec - the RA and Dec of a center of an image, used for filtering
;	and calculating pixel deviations
; xl, ol - x-ray and optical source structures
; xsel, osel - indeces of the matched X-ray and optical source structures.
; nosel - number of IDs for each match
; xosep - separation between X-ray and optical sources for each match (arcsec)
; radius - if given (arcmin), only sources and objects within it will be used.
; dfac - the factor for calculating the x-ray source radius, using
;	thr position error radius
; rfix - if given (arcsec), all sources have the same match radius as rfix
; rmin - minumum match radius (e.g., systematic error; units arcsec)
; 
; written by wqd, 6/15/2002
;-
if n_params() eq 0 then begin
print,'CALLING Seq. - sou_id,cra,cdec,xl,ol,xsel,osel,nosel,xosep'
print,',radius=radius,dfac=dfac,rfix=rfix,rmin=rmin'
return
endif
; get a rough estimate of error circle for each source
trans_dist,cra,cdec,xl.ra,xl.dec,xp,yp,/deg,/das
trans_dist,cra,cdec,ol.ra,ol.dec,oxp,oyp,/deg,/das

;select only sources within an off-center radius, if given:
if n_elements(radius) eq 0 then radius=13.
xsel=where(sqrt(xp^2+yp^2) le radius*60.,nxsel)	
if nxsel eq 0 then stop,'no X-ray source in the radius!'
nxl=xl(xsel)
xp=xp(xsel)
yp=yp(xsel)

osel=where(sqrt(oxp^2+oyp^2) le radius*60.,nosel)	
if nosel eq 0 then stop,'no object in the radius!'
nol=ol(osel)
oxp=oxp(osel)
oyp=oyp(osel)

;define the correlation radius of sources:
if n_elements(rfix) ne 0 then rs=xp*0.+rfix else begin
	if n_elements(dfac) eq 0 then dfac=3.
	rs=nxl.perr*dfac
endelse
if n_elements(rmin) ne 0 then rs=rs > rmin
srs=rs^2

osels=[-999]
xsels=[-999]
nosel=[-999]
xosep=[-999]
ns=n_elements(xp)
for k=0,ns-1 do begin 
	sep=(xp(k)-oxp)^2+(yp(k)-oyp)^2
	sels= where(sep lt srs(k), nsels) 
	if nsels ne 0 then begin
		osels=[osels,sels]
		xsels=[xsels,k]
		nosel=[nosel,nsels]
		xosep=[xosep,sep(sels)]
		print,'k, X-ray source, rs = ',k, nxl(k).iauid,sqrt(srs(k))
;		print,'optical object = ',nol(sels).id
		print,'optical object = ',nol(sels).sn
		print,'rs(k), sqrt(sep(sels))= ',rs(k),sqrt(sep(sels))
	endif else print,'k,nxl(k), rs(k), sqrt(min(sep)) = ',k,nxl(k).iauid,rs(k),sqrt(min(sep))
endfor
if n_elements(xsels) gt 1 then begin
	osel=osel(osels(1:*))
	xsel=xsel(xsels(1:*))
	nosel=nosel(1:*)
	xosep=sqrt(xosep(1:*))
endif else print,'no counterpart is found!'
if !debug eq 2 then stop
return
end
