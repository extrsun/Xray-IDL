pro read_list_iras,rao,deco,seqno,dir=dir,outfile=outfile,screen=screen
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - read_list,rao,deco,seqno,dir=dir,fname=fname,outfile=outfile,delbrigh=delbright,screen=screen'
retall
endif
;
if n_elements(dir) eq 0 then dir='/cdrom/images/'
if n_elements(outfile) eq 0 then outfile='~/rosat/shadow/iras_file.dat'

match_files,dir,'i*','b4h0.fit',seqno,nf
ra=fltarr(nf)
dec=fltarr(nf)
for k=0,nf-1 do begin
	irh=headfits(seqno(k))
	ra(k)=sxpar(irh,'crval1')
	dec(k)=sxpar(irh,'crval2')
endfor
	seqno=strmid(seqno,strlen(dir),8)
	sra=sort(ra)
	seqno=seqno(sra)
	ra=ra(sra)
	dec=dec(sra)
	rao=ra
	deco=dec
	precess,rao,deco,1950,2000
	euler,ra,dec,ddl,ddla,1

	sel=where(abs(ddla) le 90. and abs(ddla) gt 20.,nsel)
;	euler,ra,dec,del,dela,3
;	sel=where(abs(dela) gt 50. and abs(ddla) le 20.,nsel) ;abs(ddla) gt 20.,nsel)

	if nsel ne 0 then begin
		seqno=seqno(sel)
		ra=ra(sel)
		dec=dec(sel)
		rao=rao(sel)
		deco=deco(sel)
		ddla=ddla(sel)
		ddl=ddl(sel)
		nf=nsel
	endif
	if keyword_set(screen) ne 0 then return
	trans_degree,rao*(!pi/180),deco*(!pi/180),iho,imo,iso,jdo,jmo,jso
	trans_degree,ra*(!pi/180),dec*(!pi/180),ih,im,is,jd,jm,js
	print,'Total number of files selected = ',nf
	openw,unout,outfile,/get_lun
	for k=0, nf-1 do begin
	     print,seqno(k),iho(k),imo(k),iso(k),jdo(k),jmo(k),jso(k),ddl(k),ddla(k) $
	,ih(k),im(k),is(k),jd(k),jm(k),js(k),format='(a8,2(2i4,f6.2),2f9.3,2(2i4,f6.2))'
	printf,unout,seqno(k),iho(k),imo(k),iso(k),jdo(k),jmo(k),jso(k),ddl(k),ddla(k) $
	,ih(k),im(k),is(k),jd(k),jm(k),js(k),format='(a8,2(2i4,f6.2),2f9.3,2(2i4,f6.2))'
	endfor
	free_lun,unout


if !debug eq 2 then stop
return
end