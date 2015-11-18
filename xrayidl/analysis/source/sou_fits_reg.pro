pro sou_fits_reg,hdr,outfile,slist,infile=infile,slow=slow,flow=flow,probth=probth,radius=radius,factor=factor,perclimit=perclimit,norso=norso,backreg=backreg,infactor=infactor,aroutmin=aroutmin
;+
; Produce a regional file from a source fits file (my source list)
; 
; hdr - the raw count image fits header, produced by file_params
; outfile - output regional file
; slist - source structure
; infile - the name of the source file (if slist is not provided)
; slow, flow, probth - signal-to-noise, cntr, and probability thresholds for 
; selecting the sources from infile
; radius - off-axis radius threshold
; factor - the factor for scaling the source radii (def=2)
; norso - if set, the PSF radius will be calculated
;                  (perclimit is then needed)
; backreg - keyword if set, background annulus will be output
; infactor - multiple factor of the source radius for calculating the
;            inner radius of the annulus (def=2)
; aroutmin - the outer radius of the annulus (def=100 pixel), if
;            greater than 2x the inner radius.
;
; writen by WQD 12/23/2002
; revised to include the output of the annulus and source name, wqd, 7/18/07
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sou_fits_reg,hdr,outfile,slist,infile=infile,slow=slow,flow=flow,probth=probth,radius=radius,factor=factor,perclimit=perclimit,norso=norso,backreg=backreg,infactor=infactor,aroutmin=aroutmin'
return
endif
crpix1=sxpar(hdr,'crpix1')
crpix2=sxpar(hdr,'crpix2')
crval=sxpar(hdr,'crval*')
cra=crval(0) & cdec=crval(1)
cdeltx=abs(sxpar(hdr,'cdelt1'))*3600.
cdelty=abs(sxpar(hdr,'cdelt2'))*3600.
if abs(1-cdeltx/cdelty) gt 0.1 then $
	stop,'abs(1-cdeltx/cdelty) gt 0.1 arcsec?! They need to be the same!'

if n_elements(slist) eq 0 then begin
 if n_elements(slow) eq 0 then slow=0
 sou_fits_info,infile,slist,slow=slow,flow=flow,row=row,probth=probth,/all,nsel=ns
 if ns eq 0 then return
endif

if n_elements(factor) eq 0 then factor=2

sname=slist.iauid
sra=slist.ra & sdec=slist.dec & sr=slist.sradius*factor ;in units of pixels
trans_dist,cra,cdec,sra,sdec,xd,yd,/deg,pixsize=cdeltx,angle=dis
;xd=nint(crpix1+xd)
;yd=nint(crpix2+yd)
xd=float(crpix1+xd) ;at least keep two digits
yd=float(crpix2+yd)

; recalculate aperture sizes ;added on 7/29/03
rso=sr
if keyword_set(norso) eq 0 then $
  psf_params,dis,sr,slist.cntr,rso=rso,perclimit=perclimit $ ;dis is not used
else  begin
    dis=dis/60. ;in units of arcmin
    psf_params,dis,sr,slist.cntr,perclimit=perclimit
    sr=sr*factor
endelse
;sr=nint(sr+0.5)
if keyword_set(backreg) ne 0 then begin
    if n_elements(infactor) eq 0 then infactor=2.
    if n_elements(aroutmin) eq 0 then aroutmin=100.
    arin=sr*infactor
    arout=arin*2. > aroutmin
endif
openw,outn,outfile,/get
ns=n_elements(sra)
for k=0,ns-1 do begin
	printf,outn,'#'+strtrim(sname(k),2)+'!!' ;!! for parsing in sou_reg_spec
	printf,outn,'circle('+strtrim(xd(k),2)+','+strtrim(yd(k),2)+','+strtrim(sr(k),2)+')'
        if keyword_set(backreg) ne 0 then $
         printf,outn,'annulus('+strtrim(xd(k),2)+','+strtrim(yd(k),2)+',' $
          +strtrim(arin(k),2)+',' +strtrim(arout(k),2)+')'
endfor

print,'A regional file '+ outfile +' is produced!'
print,ns,' sources are included'
free_lun,outn
return
end
