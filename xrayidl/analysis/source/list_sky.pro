pro list_sky,ra,dec,infile=infile,outfile=outfile,sortl=sortl
;-
; read a standard SASS sky file *_sky.fits into a ASCII data file
; wqd, Sept. 4, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_sky,ra,dec,infile=infile,outfile=outfile,sortl=sortl'
return
endif
if n_elements(infile) eq 0 then infile=!data_dir+strtrim(!seq_no,2)+'_sky.fits'
if n_elements(outfile) eq 0 then outfile=!data_dir+strtrim(!seq_no,2)+'_sky.dat'
stop,'the input file is ',infile,' type c. to proceed'

	tab=readfits(infile,h,ext=1)
	star_number=fits_get(h,tab,'src')
	ra=fits_get(h,tab,'ra')
	dec=fits_get(h,tab,'dec')
	obj=fits_get(h,tab,'obj') 
	id=fits_get(h,tab,'id') 
	vmag=fits_get(h,tab,'vmag')
	bmag=fits_get(h,tab,'bmag')
	spmor=fits_get(h,tab,'spmor') ;hri src file contains only this position
	ruxvn=fits_get(h,tab,'ruxvn')
	ref=fits_get(h,tab,'ref')
	dis=fits_get(h,tab,'dist')
	nsource=n_elements(star_number)
if keyword_set(sortl) ne 0 then begin
	so=sort(ra)
	star_number=star_number(so)
	ra=ra(so)
	dec=dec(so)
	id=id(so)
	obj=obj(so)
	bmag=bmag(so)
	spmor=spmor(so)
	ruxvn=ruxvn(so)
	ref=ref(so)
	dis=dis(so)
endif
trans_degree,ra,dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
if outfile eq 'none' then goto,done
stop,'stop: the output file is: ',outfile
openw,unout,outfile,/get

for k=0,nsource-1 do begin
 print, star_number(k),' |', ra_hour(k),ra_min(k) $
 ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',obj(k),' |',id(k),' |',bmag(k), $
 ' |',vmag(k),' |', spmor(k),' |',ruxvn(k),' |',ref(k),' |',dis(k),' |' $
 ,format='(I3,a2,2(2i4, f7.2,a2),a15,a2,i2,a2,2(a7,a2),2(a5,a2),i2,a2,a5,a2)'

	 printf,unout, star_number(k),' |', ra_hour(k),ra_min(k) $
 ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',obj(k),' |',id(k),' |',bmag(k), $
 ' |',vmag(k),' |', spmor(k),' |',ruxvn(k),' |',ref(k),' |',dis(k),' |' $
 ,format='(I3,a2,2(2i4, f7.2,a2),a15,a2,i2,a2,2(a7,a2),2(a5,a2),i2,a2,a5,a2)'
endfor
free_lun,unout
done:
end
