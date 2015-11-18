pro get_offset_opt,imhdr,ih,im,is,jd,jm,js,xp,yp,val,xperr=xperr,operr=operr,equi=equi,index=index
;-
; main program for obtaining x, y and angular offsets of an image relative
; to known source positions.
;
; imhdr - a fits header discribe the current image 
; ih,im,is,jd,jm,js - vectors containing the ra and dec positions of
;  	the sources
; xp,yp - vectors containing the pixel positions of the sources in the image
; 	in the IDL coordinate system (i.e., the lower left pixel is (0,0)).
; val - output vector containing the x, y and angular shifts
;
; writen by wqd, Dec 4, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_offset_opt,imhdr,ih,im,is,jd,jm,js,xp,yp,val,xperr=xperr,operr=operr,equi=equi'
return
endif
;
get_headinfo,imhdr,xsize,ysize,cra,cdec,cpx,cpy,delx,dely,equi=equi
if equi eq 0 then read,'please give the equinox of the output file',equi
if abs(delx) ne dely then print,'delx ne dely'
pixel=dely

trans_radian,ih,im,is,jd,jm,js,ora,odec,/deg
trans_loct,xp-(cpx-0.5),yp-(cpy-0.5),cra,cdec,xra,xdec,pix=pixel*3600.,/deg
; assume the header has the fortran convetion about cpx and cpy
print,'xra = ',xra,xdec
print,'ora,odec = ',ora,odec
if n_elements(operr) eq 0 then begin
	operr=ora*0.+1.
	print,'operr = ',operr
endif
if n_elements(xperr) eq 0 then xperr=xra*0.+1.
nsel=n_elements(xra)
trans_dist,cra,cdec,xra,xdec,xobs,yobs,/deg,/das ;dist in units of arcsec
trans_dist,cra,cdec,ora,odec,xopt,yopt,/deg,/das
wvec=1./(operr*operr+xperr*xperr)

chi=total(((xobs-xopt)*(xobs-xopt)+(yobs-yopt)*(yobs-yopt))*wvec)
print,'before scale correction:'
print,'original Chi sq and ndf = ',chi,2*nsel
xobsn=xobs
yobsn=yobs
xobso=xobs
yobso=yobs
n=0
	ANSWER='Y'
        YESNO,ANSWER
        WHILE (ANSWER EQ 1) DO BEGIN

offset_fit,xobs,yobs,xopt,yopt,wvec,val
print,'Before correction: xobs,yobs (arcsec) = ',xobs,yobs
xobsn=val(0)+xobs-yobs*val(2)
yobsn=val(1)+yobs+xobs*val(2)
print,'Displacements from optical position (arcsec) before the shifts:'
print,sqrt((xobs-xopt)^2+(yobs-yopt)^2)
print,'dispersions (arcsec):'
print,sqrt(1./wvec)

print,'absolute shifts from the X-ray position (arcsec):'
print,sqrt((xobsn-xobs)^2+(yobsn-yobs)^2)

print,'Displacements from optical position (arcsec) after the shifts:'
print,sqrt((xobsn-xopt)*(xobsn-xopt)+(yobsn-yopt)*(yobsn-yopt))

chi=total(((xobsn-xopt)*(xobsn-xopt)+(yobsn-yopt)*(yobsn-yopt))*wvec)
print,'Chi sq and ndf = ',chi,2*nsel-3
Val(2)=val(2)*180./!pi
print,'required X and Y (arcsec), and angular (degree) shifts of the X-ray image : ',val

r=sqrt(xopt^2+yopt^2)/60.
if n_elements(index) eq 0 then index=2
scale_fit,r,xobsn,yobsn,xopt,yopt,wvec,val,index=index
cc=1.+(val(0)+val(1)*r^index)
xobsn=xobsn*cc
yobsn=yobsn*cc
chi=total(((xobsn-xopt)*(xobsn-xopt)+(yobsn-yopt)*(yobsn-yopt))*wvec)
print,'after scale correction of scale (cc, val) = ',cc,val
print,'Chi sq and ndf = ',chi,2*nsel
	xobs=xobso*cc
	yobs=yobso*cc
answer=''
PRINT,'Do again? ;
READ,ANSWER
YESNO,ANSWER
;if answer eq 1 then begin
;	xobs=xobs*cc
;	yobs=yobs*cc
;endif
endwhile
stop
end