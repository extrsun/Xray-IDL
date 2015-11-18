pro read_list_mpe,rao,deco,text,dir=dir,fname=fname,outfile=outfile,explimit=explimit,delbrigh=delbright,screen=screen,equi=equi
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - read_list_mpe,rao,deco,text,dir=dir,fname=fname,outfile=outfile,explimit=explimit,delbrigh=delbright,screen=screen,equi=equi'
retall
endif
;
if n_elements(explimit) eq 0. then explimit=5000.
if n_elements(dir) eq 0 then dir='/data1/wqd/archives/'
if n_elements(outfile) eq 0 then outfile=dir+'file_obs.dat'
if n_elements(fname) eq 0 then fname='public_mpe.dat'
fname=dir+fname

	openr,un,fname,/get_lun

	n_source=5000
	id=strarr(n_source)
	text=strarr(n_source)
	time=lonarr(n_source)
	radecv=strarr(n_source)
	ra=fltarr(n_source)
	dec=fltarr(n_source)

	k=0
	str=''
	while not EOF(un) do begin
	   readf,un, str
	   text(k)=str
	   bstr=byte(str)
	   id(k)=string(bstr(11:13))
	   radecv(k)=string(bstr(54:84))
	   time(k)=long(string(bstr(85:92)))
;stop
	   if time(k) ge explimit then begin
		radec=radecv(k)
		remcharv,radec,['h','m','s','d']
		stringad,radec,rascale,decscale
		ra(k)=rascale & dec(k)=decscale
	   	k=k+1
	   endif
	endwhile
;
 	kk=k-1
	text=text(0:kk)
	ra=ra(0:kk)
	dec=dec(0:kk)
	id=id(0:kk)
	rao=ra
	deco=dec
	precess,ra,dec,2000,1950
	euler,ra,dec,ddl,ddla,1
	euler,ra,dec,del,dela,3
	sel=where(abs(dela) gt 0. and abs(ddla) le 90.,nsel) ;abs(ddla) gt 20.,nsel)
;	sel=where(abs(dela) gt 50. and abs(ddla) le 20.,nsel) ;abs(ddla) gt 20.,nsel)
	if nsel ne 0 then begin
		ra=ra(sel)
		dec=dec(sel)
		rao=rao(sel)
		deco=deco(sel)
		ddla=ddla(sel)
		ddl=ddl(sel)
		id=id(sel)
		text=text(sel)
		kk=nsel-1
	endif
if keyword_set(screen) ne 0 then return
	trans_degree,ra*(!pi/180),dec*(!pi/180),ih,im,is,jd,jm,js
	free_lun,un
	idexc=['SMC','LMC','ABE','NGC','COM','VEL','ORI','CAL','M31','ALP', $
		'XRT','Hya','Cra','Vel','LUP']
	bin=lindgen(nsel)
	if keyword_set(delbright) ne 0 then begin
		dsel=[-99]
		for k=0,kk do begin
			c=where(strmid(id(k),0,3) eq idexc,nc)
			if nc ne 0 then dsel=[dsel,k]
		endfor
		dsel=dsel(1:*)
		remove,dsel,bin
		text=text(bin)
		rao=rao(bin)
		deco=deco(bin)
	endif

	if keyword_set(screen) ne 0 then return
	openw,unout,outfile,/get
	for i=0,n_elements(bin)-1 do begin
	     k=bin(i)
	     print,text(k),ddl(k),ddla(k),ih(k),im(k),is(k) $  
	,jd(k),jm(k),js(k)
 	     printf,unout,text(k)
	     printf,unout,ddl(k),ddla(k),ih(k),im(k),is(k),jd(k),jm(k),js(k),format='(2f9.3,2(2i4,f6.2))'
;	     printf,unout,seqno(k),rao(k),deco(k),ih(k),im(k),is(k),jd(k),jm(k),j;s(k),format='(i7,2f9.3,2(2i4,f6.2))'
next:
	endfor
	free_lun,unout
if n_elements(equi) ne 0 then begin
	if equi eq 1950 then begin rao=ra & deco=dec 
	endif else precess,rao,deco,2000,equi
endif
if !debug eq 2 then stop
return
end