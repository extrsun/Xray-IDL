pro read_log,rao,deco,text,timesdir=dir,fname=fname,outfile=outfile $
,delbrigh=delbright,screen=screen,explimit=explimit,gl=gl,gb=gb,equi=equi
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - read_log,rao,deco,text,dir=dir,fname=fname,outfile=outfile,delbrigh=delbright,screen=screen,explimit=explimit'
retall
endif
;
if n_elements(explimit) eq 0 then explimit=10.
if n_elements(dir) eq 0 then dir='/data1/wqd/archives/data/'
if n_elements(outfile) eq 0 then outfile=dir+'file_obs.dat'
if n_elements(fname) eq 0 then fname='master_obs_m.list'
fname=dir+fname

	openr,un,fname,/get_lun

	n_source=5000
	text=strarr(n_source)
	radec=strarr(n_source)
	ra=fltarr(n_source)
	dec=fltarr(n_source)

	k=0
	radecs=''
	file=''
	inst=0
	time=0.
	texts=''
	while not EOF(un) do begin
	   readf,un,file,inst,time,radecs,texts,format='(a8,22x,i1,4x,f6.2,11x,a33,a40)'
;,format='(a8,i1,f6.2,3x,a33,a40)'
	  if time gt explimit and inst ne 0 then begin
	   text(k)=file+texts
	   radec=radecs
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
	rao=ra
	deco=dec
	precess,ra,dec,2000,1950
	euler,ra,dec,gl,gb,1
	euler,ra,dec,del,dela,3

	sel=where(abs(dela) gt 0. and abs(gb) gt 20.,nsel) ;abs(gb) gt 20.,nsel)
;	sel=where(abs(dela) gt 50. and abs(gb) le 20.,nsel) ;abs(gb) gt 20.,nsel)

	if nsel ne 0 then begin
		ra=ra(sel)
		dec=dec(sel)
		rao=rao(sel)
		deco=deco(sel)
		gb=gb(sel)
		gl=gl(sel)
		text=text(sel)
		kk=nsel-1
	endif

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

if n_elements(equi) ne 0 then begin
	if equi eq 1950 then begin rao=ra & deco=dec 
	endif else precess,rao,deco,2000,equi
endif
if !debug eq 2 then stop
return
end