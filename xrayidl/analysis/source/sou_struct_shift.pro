pro sou_struct_shift,cra,cdec,slist,xshift,yshift,ncra,ncdec,nslist,ashift=ashift
;+
; Apply X, Y, and angular shifts of a source list
; cra, cdec - center RA and Dec of the image in which sources are detected
; slist - source list
; xshift, yshift, ashift - X, Y, and angular shifts (arcsec, anticlockwise deg)
; nra, ndec - corrected center RA and Dec of the image
; nslist - corrected source list
;
; writen by wqd, June 16, 2002
;-
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - sou_struct_shift,cra,cdec,slist,xshift,yshift'
print,',ncra,ncdec,nslist,ashift=ashift'
return
endif
trans_dist,cra,cdec,slist.ra,slist.dec,xp,yp,/deg,/das
if n_elements(ashift) ne 0 then begin
	ash=ashift*!pi/180.
	xx=x*cos(ash)-y*sin(ash)
	yy=x*sin(ash)+y*cos(ash)
endif else begin 
	xx=xp
	yy=yp
endelse
ncra=cra-xshift/(3600.*cos(cdec*!pi/180.))
ncdec=cdec+yshift/(3600.)
trans_loct,xx,yy,ncra,ncdec,ra,dec,/deg,/das
nslist=slist
nslist.ra=ra
nslist.dec=dec

radec_out,ra,dec,dig=dig,iauname=iauid
nslist.iauid=iauid
return
end
