pro cursor_posi,corner,hdr,srav,sdecv,cra=cra,cdec=cdec,xarcmin=xarcmin,yarcmin=yarcmin $
,equi1=equi1,equi2=equi2,xnbin=xnbin,ynbin=ynbin,down=down
;-
; get cursor positions (RA and Dec) in an image displayed by cont_grey.pro
; cra,cdec - the center coordinates of the image (in radian)
; xarcmin,yarcmin - the dimension of the image (arcmin)
; corner - a vector containing xmin, xmax, ymin,ymax of the image 
;  	(normalized coordinates from cont_grey.pro)
; equi1 - the equinox of the image coordinate
; equi2 - the equinox desired for the output RA and Dec
; xnbin,ynbin - the assumed x and y dimension of the image. They can be
;		different from the actual ones, depending on the assumed block.
; writen by WQD 4/23/93
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - cursor_posi,corner,hdr,srav,sdecv,cra=cra,cdec=cdec'
print,',xarcmin=xarcmin,yarcmin=yarcmin'
print,'equi1=equi1,equi2=equi2,xnbin=xnbin,ynbin=ynbin,down=down'
return
endif
if n_params() gt 1 then begin
	crval=sxpar(hdr,'crval*')
if n_elements(cra) eq 0 then cra=crval(0) 
if n_elements(cdec) eq 0 then  cdec=crval(1)
if n_elements(xarcmin) eq 0 then $
	xarcmin=sxpar(hdr,'naxis1')*abs(sxpar(hdr,'cdelt1'))*60.
if n_elements(yarcmin) eq 0 then $
	yarcmin=sxpar(hdr,'naxis2')*abs(sxpar(hdr,'cdelt2'))*60.
endif
if n_elements(equi1) eq 0 then begin 
	equi1=2000
	print,'the input equinox = 2000 is assumed'
endif
if n_elements(equi2) eq 0 then equi2=2000
xref=sxpar(hdr,'crpix1')
yref=sxpar(hdr,'crpix2')
xpixpix=xarcmin*120./(corner(1)-corner(0))/!d.x_vsize
ypixpix=yarcmin*120./(corner(3)-corner(2))/!d.y_vsize
print,'pixel per pix = ',xpixpix,ypixpix
if n_elements(xnbin) eq 0 then xnbin=sxpar(hdr,'naxis1')
if n_elements(ynbin) eq 0 then ynbin=sxpar(hdr,'naxis2')
xmid=(corner(0)+(corner(1)-corner(0))*(xref/xnbin))*!d.x_vsize
ymid=(corner(2)+(corner(3)-corner(2))*(yref/ynbin))*!d.y_vsize

binpixel=xnbin/(corner(1)-corner(0))/!d.x_vsize
xbinmid=xref-1.
ybinmid=yref-1.
print,'Press the left button to get the info about the closest source'
print,'Press the right button to select the source and  exit'
;
;while (!err ne 4) do begin
print,'    xbin, ybin;  RA (',strtrim(equi2,2),')    Dec (',strtrim(equi2,2),')'
cr=string("15b)
srav=[-999]
sdecv=[-999]
LOOP:
cursor,xpix,ypix,1,/device,down=down ;/change ;,/down
if !ERR eq 4 then goto,done
xp=(xpix-xmid)*xpixpix
yp=(ypix-ymid)*xpixpix
trans_loct,xp,yp,cra,cdec,sra,sdec,/deg
;sra=sra*180./!pi  & sdec=sdec*180./!pi
precess,sra,sdec,equi1,equi2
trans_degree,sra,sdec,ih,im,is,jd,jm,js,/deg
if n_elements(xnbin) ne 0 then begin
	xbin=(xpix-xmid)*binpixel+xbinmid
	ybin=(ypix-ymid)*binpixel+ybinmid
endif else begin
	xbin=xpix
	ybin=ypix
endelse
print,form="($,4x,2(f5.1,1x),a1,2(2I4,f6.2),a)",xbin,ybin,';',ih,im,is,jd,jm,js,cr
srav=[srav,sra]
sdecv=[sdecv,sdec]
goto,LOOP
done:
srav=srav(1:*)
sdecv=sdecv(1:*)
print,''
stop
return
end
