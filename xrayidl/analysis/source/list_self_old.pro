pro list_self,ra,dec,infile=infile,outfile=outfile,sortl=sortl,slow=slow,flow=flow
;-
; read a standard SASS source file *_src.fits into a ASCII data file
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_self,ra,dec,infile=infile,outfile=outfile'
print,',slow=slow,flow=flow,sortl=sortl'
return
endif
if n_elements(slow) eq 0 then slow=0
if n_elements(flow) eq 0 then flow=0
if n_elements(infile) eq 0 then infile=!data_dir+strtrim(!seq_no,2)+'_src.fits'
if n_elements(outfile) eq 0 then outfile=!data_dir+strtrim(!seq_no,2)+'_sou_sass.dat'
stop,'the input file is ',infile,' type c. to proceed'
	rs_sources,infile,hdr,s
;	star_number=s.mplsx_src
	star_number=s.src
	ra=s.ra/!radeg
	dec=s.dec/!radeg
	cntr=s.ccts
	source_sn=imdiv(cntr,s.ccts_err)
	x_core=long(s.im_x+0.5)
	y_core=long(s.im_y+0.5)
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
trans_degree,ra,dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec

stop,'stop: the output file is: ',outfile
openw,unout,outfile,/get

for k=0,nsource-1 do begin
	if source_sn(k) ge slow and cntr(k) gt flow  then begin
	 print, star_number(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
	' |',cntr(k),' |', x_core(k),' |',y_core(k),' |' $
	,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(I4,a2))'

	 printf,unout, star_number(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
	' |',cntr(k),' |', x_core(k),' |',y_core(k),' |' $
	,format='(i3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(i4,a2))'
	endif
endfor
free_lun,unout
end
