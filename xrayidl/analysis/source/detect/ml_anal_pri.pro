;-----------------------------------------------
pro ml_anal_pri,ra_dist,dec_dist,sse,snr,cntr,dsv,dis,rs,sz,scntr,gcntr
;
;record these source into output file:
trans_loct,ra_dist,dec_dist,cra,cdec,star_ra,star_dec,/deg
trans_degree,star_ra,star_dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
kk=0
for k=0,(ns-1) do begin
	if snr(k) ge threshold then begin
	kk=kk+1
 	print, kk, ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k),snr(k),cntr(k),scntr(k), $
 	dsv(k),dis(k),sse(k,0),sse(k,1),rs(k),sigma(k),sse(k,2) $
	,sz(k)-sigma(k),gcntr(k), $
		format='(I3, 2(2i4, f7.2), f9.2,2f9.5,1x,8f8.2,f9.5)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, kk,' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',snr(k), $
   	  ' |',cntr(k),' |',scntr(k),' |', dsv(k),' |', dis(k),' |' $
		,sse(k,0),' |',sse(k,1),' |',rs(k),' |',sigma(k),' |' $
		,sse(k,2),' |',sz(k)-sigma(k),' |',gcntr(k),' |'  $
	  ,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, 2(f9.5,a2),8(f8.2,a2),f9.5,a2)'
	endif
endfor
free_lun,un
return
end