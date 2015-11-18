pro list_self,ra,dec,infile=infile,outfile=outfile,sortl=sortl,slow=slow,flow=flow,imcra=imcra,imcdec=imcdec,xshift=xshift,yshift=yshift,ashift=ashift,radecshift=radecshift,cntrerr=cntrerr,deg=deg
;-
; read a standard SASS source file *_src.fits into a ASCII data file
; imcra, imcdec - ra and dec of the image center in units of deg
;	for calculating source positions using directly their pixel positions
; radecshift - if set, the RA and Dec of the source positions will also
; 		be output after shift corrections. Otherwise, only the 
;		pixel coordinates will be shifted.
; cntrerr - if set, count rate err will be output in the position of S/N
; add keywords imcra and imcdec for greater accuracy of source positions
; wqd, June 18, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_self,ra,dec,infile=infile,outfile=outfile'
print,',slow=slow,flow=flow,sortl=sortl,imcra=imcra,imcdec=imcdec'
print,',xshift=xshift,yshift=yshift,ashift=ashift,deg=deg'
return
endif
if n_elements(slow) eq 0 then slow=0
if n_elements(flow) eq 0 then flow=0
if n_elements(infile) eq 0 then infile=!data_dir+strtrim(!seq_no,2)+'_src.fits'
if n_elements(outfile) eq 0 then outfile=!data_dir+strtrim(!seq_no,2)+'_sou_sass.dat'
stop,'the input file is ',infile,' type c. to proceed'
;	rs_sources,infile,hdr,s
;	star_number=s.mplsx_src
	tab=readfits(infile,h,ext=1)
	if !proc eq 'RDF' then begin
		star_number=fits_get(h,tab,'SDS_NUM')
		cntr=fits_get(h,tab,'NET_RT')
		source_sn=fits_get(h,tab,'NET_RT_ERR')
		if keyword_set(cntrerr) eq 0 then $
			source_sn=imdiv(cntr,source_sn)
	endif else begin
	 if !instr eq 'h' then begin
		star_number=fits_get(h,tab,'src')
		cntr=fits_get(h,tab,'net/1000')
		source_sn=fits_get(h,tab,'net_err')
		if keyword_set(cntrerr) eq 0 then $
			source_sn=imdiv(cntr,source_sn)

	 endif else begin
		star_number=fits_get(h,tab,'mplsx_src') 
			;which is used by source_info
		cntr=fits_get(h,tab,'ccts')
		source_sn=imdiv(cntr,fits_get(h,tab,'ccts_err'))
	 endelse
	endelse
;	x_core=long(s.im_x+0.5)
;	y_core=long(s.im_y+0.5)
	x_core=fits_get(h,tab,'x') ;hri src file contains only this position
	y_core=fits_get(h,tab,'y')
	if n_elements(imcdec) eq 0 then begin
		ra=fits_get(h,tab,'ra')
		dec=fits_get(h,tab,'dec')
	endif else begin
		pixel_radec,imcra,imcdec,x_core,y_core,ra,dec
	endelse
	shift_xya,x_core,y_core,xshift=xshift,yshift=yshift,ashift=ashift
	if keyword_set(radecshift) ne 0 then $
		pixel_radec,imcra,imcdec,x_core,y_core,ra,dec
	x_core=long(x_core+0.5)
	y_core=long(y_core+0.5)
	nsource=n_elements(star_number)
if keyword_set(sortl) ne 0 then begin
	so=sort(ra)
	star_number=star_number(so)
	ra=ra(so)
	dec=dec(so)
	cntr=cntr(so)
	source_sn=source_sn(so)
	x_core=x_core(so)
	y_core=y_core(so)
endif
trans_degree,ra,dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
if outfile eq 'none' then goto,done
stop,'stop: the output file is: ',outfile
openw,unout,outfile,/get
cntr=cntr*1.e-3
for k=0,nsource-1 do begin
	if source_sn(k) ge slow and cntr(k) gt flow  then begin
	 print, star_number(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
	' |',cntr(k),' |', x_core(k),' |',y_core(k),' |' $
	,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(I8,a2))'

	 printf,unout, star_number(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
	' |',cntr(k),' |', x_core(k),' |',y_core(k),' |' $
	,format='(i3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(i8,a2))'
	endif
endfor
if keyword_set(deg) eq 0 then begin
	ra=ra/!radeg
	dec=dec/!radeg
endif
free_lun,unout
done:
end
