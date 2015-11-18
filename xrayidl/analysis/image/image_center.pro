pro image_center,ra,dec,proc=proc,deg=deg,pri=pri
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - image_center,ra,dec,proc=proc,/deg,/pri'
return
endif
if n_elements(proc) eq 0 then proc=strtrim(!proc,2)
;if proc eq 'MPE' then extyp='map.ifits' else extyp='.fits'
;sxhread,!data_dir+!seq_no+'_mex'+extyp,hdr
case proc of 
'US':  begin
	hdr=headfits(!data_dir+!seq_no+'.fits',ext=3)
	ra=sxpar(hdr,'crval1') 
	dec=sxpar(hdr,'crval2') 
	end
'MPE' : begin
	hdr=headfits(!data_dir+!seq_no+'_events.tfits',ext=1)
	getnomasp,hdr,ra,dec,proc=proc
	ra=ra/7200.
	dec=dec/7200.
	end
'MPEUS': begin
	hdr=headfits(!data_dir+!seq_no+'.fits',ext=1)
	ra=sxpar(hdr,'RA_NOM')
	dec=sxpar(hdr,'DEC_NOM')
	end
'RDF': begin
	hdr=headfits(!data_dir+!seq_no+'_bas.fits',ext=0)
	ra=sxpar(hdr,'ra_nom') 
	dec=sxpar(hdr,'dec_nom') 
       end
else : stop,'The PROC is not a valid ROSAT processing format.'
endcase
if keyword_set(pri) ne 0 then begin
	trans_degree,ra,dec,ih,im,is,jd,jm,js,/degree
	print,ih,im,is,jd,jm,js
endif
if keyword_set(deg) eq 0 then begin
	ra=ra/!radeg & dec=dec/!radeg
endif
end