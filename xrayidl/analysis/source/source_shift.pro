pro source_shift,cra,cdec,infile,outfile,xshift=xshift,yshift=yshift,ashift=ashift,pixsize=pixsize
;-
; performing position shifts of a source list
; writen by wqd, March 9, 1995
;+
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - source_shift,cra,cdec,infile,outfile'
print,',xshift=xshift,yshift=yshift,ashift=ashift,pixsize=pixsize'
return
endif
source_info,souno,star_ra,star_dec,text=text $
	,ns=ns,soufile=infile,slow=0.,flow=0.,/deg $
	,xshift=xshift,yshift=yshift,ashift=ashift,crval=[cra,cdec] ;,/self
if n_elements(pixsize) ne 0 then begin
	trans_dist,cra,cdec,star_ra,star_dec,xp,yp,/deg
	trans_loct,xp,yp,cra,cdec,star_ra,star_dec,/deg,pixsize=pixsize
endif
trans_degree,star_ra,star_dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec $
	,/deg
openw,un,outfile,/get_lun
for k=0,(ns-1) do begin
	texts=strmid(text(k),39,115 < (strlen(text(k))-1))
 	print, souno(k), ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k), texts, $
		format='(I3, 2(2i4, f7.2), a115)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, souno(k),' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',texts  $
	  ,format='(I3,a2,2(2i4, f7.2,a2), a115)'
endfor
free_lun,un
end
