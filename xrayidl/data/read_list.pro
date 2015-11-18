pro read_list,rao,deco,text,dir=dir,fname=fname,outfile=outfile,delbrigh=delbright,screen=screen,explimit=explimit,gl=gl,gb=gb,equi=equi
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - read_list,rao,deco,text,dir=dir,fname=fname,outfile=outfile,delbrigh=delbright,screen=screen,explimit=explimit'
retall
endif
;
if n_elements(explimit) eq 0 then explimit=5000.
if n_elements(dir) eq 0 then dir='/data1/wqd/archives/data/'
if n_elements(outfile) eq 0 then outfile=dir+'file_obs.dat'
if n_elements(fname) eq 0 then fname='ppublic_data.pos'
fname=dir+fname

	openr,un,fname,/get_lun

	n_source=5000
	text=strarr(n_source)
	seqno=strarr(n_source)
	site=strarr(n_source)
	filter=strarr(n_source)
	reltime=strarr(n_source)
	time=lonarr(n_source)
	id=strarr(n_source)
	person=strarr(n_source)
	ra=fltarr(n_source)
	dec=fltarr(n_source)

	k=0
	str=''
	while not EOF(un) do begin
	   readf,un, str
	   text(k)=str
	   seqno(k)=gettok(str,'|')
	   site(k)=gettok(str,'|')
	   filter(k)=gettok(str,'|')
	   reltime(k)=gettok(str,'|')
	   radec=gettok(str,'|')
	   radec=radec+' '+gettok(str,'|')
	   time(k)=gettok(str,'|')
	   id(k)=gettok(str,'|')
	   person(k)=gettok(str,'|')
;site(k) ne 'MPE ' and
	   if filter(k) ne 'Y' and time(k) ge explimit then begin
	stringad,radec,rascale,decscale
	ra(k)=rascale & dec(k)=decscale
	   	k=k+1
	   endif
	endwhile
;
 	kk=k-1 
	text=text(0:kk)
	seqno=seqno(0:kk)
	ra=ra(0:kk)
	dec=dec(0:kk)
	id=id(0:kk)
	;trans_getgc,ra,dec,gl,gb
	rao=ra
	deco=dec
	precess,ra,dec,2000,1950
	euler,ra,dec,gl,gb,1
	euler,ra,dec,del,dela,3

	sel=where(abs(dela) gt 0. and abs(gb) le 90.,nsel) ;abs(gb) gt 20.,nsel)
;	sel=where(abs(dela) gt 50. and abs(gb) le 20.,nsel) ;abs(gb) gt 20.,nsel)

	if nsel ne 0 then begin
		seqno=seqno(sel)
		ra=ra(sel)
		dec=dec(sel)
		rao=rao(sel)
		deco=deco(sel)
		gb=gb(sel)
		gl=gl(sel)
		id=id(sel)
		text=text(sel)
		kk=nsel-1
	endif

	trans_degree,ra*(!pi/180),dec*(!pi/180),ih,im,is,jd,jm,js
	free_lun,un

; get rid of some files
	idexc=['SMC','LMC','ABE','NGC','COM','VEL','ORI','CAL']
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
	     print,text(k),gl(k),gb(k),ih(k),im(k),is(k) $  
	,jd(k),jm(k),js(k)
 	     printf,unout,text(k)
	     printf,unout,gl(k),gb(k),ih(k),im(k),is(k),jd(k),jm(k),js(k),format='(2f9.3,2(2i4,f6.2))'
;	     printf,unout,seqno(k),rao(k),deco(k),ih(k),im(k),is(k),jd(k),jm(k),j;s(k),format='(i7,2f9.3,2(2i4,f6.2))'
	endfor
	free_lun,unout
if n_elements(equi) ne 0 then begin
	if equi eq 1950 then begin rao=ra & deco=dec 
	endif else precess,rao,deco,2000,equi
endif
if !debug eq 2 then stop
return
end