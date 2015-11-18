pro pixel_scale,ih,im,is,jd,jm,js,xp,yp,xpnew,ypnew,mra,mdec,mpscales=mpscales,getmedain=getmedian
; 
; program to calculate the mean pixel scale 
;
; ih,im,is,jd,jm,js - ra and dec positions of stars
; xp, yp - pixel positions of the stars
; mpscales, mpscales_e - the output mean pixels size in units of arcsec/pixel
; wqd, Dec. 4, 1994
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - pixel_scale,ih,im,is,jd,jm,js,xp,yp,mpscales,mpscales_e'
return
endif
trans_radian,ih,im,is,jd,jm,js,ra,dec,/deg
trans_dist,ra(0),dec(0),ra(1:*),dec(1:*),xs,ys,/deg,/das
diss=sqrt(xs^2+ys^2)
disp=sqrt((xp(0)-xp(1:*))^2+(yp(0)-yp(1:*))^2)
pscales=diss/disp
print,'pscales = ', pscales
if keyword_set(getmedian) ne 0 then mpscales=median(pscales) else $
	get_stat,pscales,mpscales,mpscales_e
print,'mpscales = ',mpscales

if n_elements(xpnew) ne 0 then begin
	trans_loct,xpnew-xp,ypnew-yp,ra,dec,pix=mpscales,sra,sdec,/deg
	; only an approximation in a small field of view.
	print,'sra and sdec = ', sra,sdec
	get_stat,sra,mra
	get_stat,sdec,mdec
endif
end