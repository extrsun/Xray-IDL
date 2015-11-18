pro get_offset_opt,cra,cdec,ora,odec,xra,xdec,val,xperr=xperr,operr=operr,noangle=noangle
;+
; Calculate the x, y and angular offsets of an image relative
; to known source positions.
;
; cra, cdec - RA and Dec (deg) of the current image center
; ora,odec - vectors containing the ra and dec (deg) of 
;	reference objects (supposedly with accurate positions)
; xra, xdec - vectors containing the ra and dec positions (deg) of 
;  	X-ray sources obtained from the current image
; xperr,operr - vectors containing x-ray source and optical object position
;		errors (arcsec)
; val - output vector containing the x, y (arcsec) and angular (deg) shifts
; noangle - if set, only the x and y shifts are calculated
;
; written by wqd, Dec 4, 1994
; simplied by wqd, June 16, 2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_offset_opt,cra,cdec,ora,odec,xra,xdec,val'
print,',xperr=xperr,noangle=noangle'
return
endif
;
;print,'xra = ',xra,xdec
;print,'ora,odec = ',ora,odec
if n_elements(operr) eq 0 then begin
	operr=ora*0.
	print,'assuming operr = ',0
endif
if n_elements(xperr) eq 0 then xperr=xra*0.+1.
nsel=n_elements(xra)
trans_dist,cra,cdec,xra,xdec,xobs,yobs,/deg,/das ;dist in units of arcsec
trans_dist,cra,cdec,ora,odec,xopt,yopt,/deg,/das
wvec=1./(operr*operr+xperr*xperr)
chi=total(((xobs-xopt)*(xobs-xopt)+(yobs-yopt)*(yobs-yopt))*wvec)
print,'Original Chi Sq. and ndf = ',chi,2*nsel

if keyword_set(noangle) eq 0 then $
	offset_fit,xobs,yobs,xopt,yopt,wvec,val $
else begin
	val=fltarr(3)
	val(0)=total((xopt-xobs)*wvec)/total(wvec)
	val(1)=total((yopt-yobs)*wvec)/total(wvec)
	val(2)=0.
endelse
print,'Before correction: xobs,yobs (arcsec) = ',xobs,yobs
xobsn=val(0)+xobs-yobs*val(2)
yobsn=val(1)+yobs+xobs*val(2)
print,'Displacements from optical position (arcsec) before the shifts:'

dr=sqrt((xobsn-xopt)*(xobsn-xopt)+(yobsn-yopt)*(yobsn-yopt))
edr=sqrt(1./wvec)

print,'absolute shifts from the X-ray position (arcsec):'
print,dr
print,'dispersions (arcsec):'
print,'significance (sigma) of the shift:'
print,dr/edr

avg_least,dr,edr,mdr,medr
print,'Mean shift = ',mdr,' +- ',medr,' arcsec'
print,'Displacements from optical position (arcsec) after the shifts:'
print,sqrt((xobsn-xopt)*(xobsn-xopt)+(yobsn-yopt)*(yobsn-yopt))

print,'Displacements from optical position (arcsec) after the shifts:'
print,'xobsn-xopt = ', xobsn-xopt
print,'yobsn-yopt = ',yobsn-yopt

chi=total(dr^2*wvec)
if keyword_set(noangle) eq 0 then ndf=2*nsel-3 else ndf=2*nsel-2
print,'Chi sq and ndf = ',chi, ndf
Val(2)=val(2)*180./!pi
print,'required X and Y (arcsec), and angular (degree) shifts of the X-ray image : ',val
return
end
