pro source_info,source_id,xp,yp,pixel=pixel,sn=seq_no,dir=dir,self=self $
,file=file
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_info,source_id,xp,yp,[pixel,sn,dir,self=]'
retall
endif
;
if n_elements(dir) eq 0 then dir=!data_dir
if n_elements(seq_no) eq 0 then seq_no=!seq_no

if keyword_set(self) ne 0 then begin

	if n_elements(file) eq 0 then fname=dir+seq_no+'_sou.dat' else $
	fname=dir+strtrim(file,2)
	openr,un,fname,/get_lun
	
	n_source=500
	source_id=intarr(n_source)
	ra_hour=intarr(n_source)
	ra_min=intarr(n_source)
	ra_sec=fltarr(n_source)
	dec_deg=intarr(n_source)
	dec_min=intarr(n_source)
	dec_sec=fltarr(n_source)

	k=0
	while not EOF(un) do begin
	   readf,un, source_ids, ra_hours,ra_mins,ra_secs $
           ,dec_degs,dec_mins,dec_secs,format='(I3,2(2i4, f7.2))'
	   source_id(k)=source_ids & ra_hour(k)=ra_hours & ra_min(k)=ra_mins 
	   ra_sec(k)=ra_secs  & dec_deg(k)=dec_degs & dec_min(k)=dec_mins 
	   dec_sec(k)=dec_secs
	   k=k+1
;	,source_sn(k),cntr(k), x_core(k),y_core(k)
	endwhile
	free_lun,un

	n_source=k
	source_id=source_id(0:n_source-1)
	ra_hour=ra_hour(0:n_source-1)
	ra_min=ra_min(0:n_source-1)
	ra_sec=ra_sec(0:n_source-1)
	dec_deg=dec_deg(0:n_source-1)
	dec_min=dec_min(0:n_source-1)
	dec_sec=dec_sec(0:n_source-1)
	
	trans_radian,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,xp,yp

endif else begin

	fname=dir+strtrim(seq_no,2)+'_src.fits'
	rs_sources,fname,hdr,s
	source_id=s.mplsx_src
	if keyword_set(pixel) eq 0 then begin
	xp=s.ra/!radeg
	yp=s.dec/!radeg
	endif else begin
	xp=s.im_x
	yp=s.im_y
	endelse

endelse

if !debug eq 2 then stop
return
end