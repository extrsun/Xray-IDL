pro sourceid_get,source_id,source_ra,source_dec,sourceid,ra=ra,dec=dec
;
; Find the right source ID from RA and DEC (user input)
;
if (n_elements(ra) eq 0 or n_elements(dec) eq 0) then begin
;get information about sources in the image
;
print,'give RA(hour,min,sec) and Dec(deg,arcmin,arcsec) of the source.'
;
read,hour,min,sec,deg,arcmin,arcsec
trans_radian,hour,min,sec,deg,arcmin,arcsec,ra,dec
endif
;
trans_dist,ra,dec,source_ra,source_dec,xp,yp
dist=(xp*xp+yp*yp)
mindis=min(dist,id)
if mindis ne 0. then mindis=sqrt(mindis)
print,'The source (mplsx) id = ',source_id(id),'; distance = ',mindis, ' pixels;' 
print,'xp = ',xp(id);' and yp = ',yp(id)
print,''
;
print,'The source ',source_id(id),' is selected'
sourceid=id
end