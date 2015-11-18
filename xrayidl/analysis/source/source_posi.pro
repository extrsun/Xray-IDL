pro source_posi,ra,dec,perr,infile=infile,sou_sel=sou_sel,hdr=hdr $
	,imcra=imcra,imcdec=imcdec
;-
; get source positions and errors of a selected number of sources from 
; a standard SASS source file *_src.fits 
; writen by wqd, May 4, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_posi,ra,dec,perr,infile=infile'
print,',sou_sel=sou_sel,imcra=imcra,imcdec=imcdec'
return
endif
if n_elements(infile) eq 0 then infile=!data_dir+strtrim(!seq_no,2)+'_src.fits'
stop,'the input file is ',infile,' type c. to proceed'
	tab=readfits(infile,h,ext=1)
	star_number=fits_get(h,tab,'mplsx_src')
	perr=fits_get(h,tab,'pix_err')*15. ;no in units of arcsec
	x_core=fits_get(h,tab,'x') ;hri src file contains only this position
	y_core=fits_get(h,tab,'y')
	if n_elements(imcra) ne 0 then $
		pixel_radec,imcra,imcdec,x_core,y_core,ra,dec else begin
		ra=fits_get(h,tab,'ra') ;this is only accurate to 0.001
		dec=fits_get(h,tab,'dec') ;for ra > 100 deg
	endelse

		;recalculate the ra and dec based on x_core and Y_core
if n_elements(sou_sel) ne 0 then begin
match,sou_sel,star_number,c,sel,count=nsel
if nsel ne n_elements(sou_sel) then stop,'no all sources are found'
sel=sel(sort(c))
ra=ra(sel)
dec=dec(sel)
perr=perr(sel)
endif
stop
end
