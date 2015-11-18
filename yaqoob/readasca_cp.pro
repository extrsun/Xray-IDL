pro readasca,dname=dname,fname=fname,tname=tname,keyb,slist,blist
;
;	DNAME - DIRECTORY NAME CONTAINING THE DATA FILES
;	FNAME - FILENAME CONTAINING THE DATA FILENAMES
;	TNAME - FILENAME OF OUTPUT TIME INTERVALS FILE
; *************
;print,n_params(0)
;print,n_elements(fname),n_elements(dname),n_elements(tname)
if n_params(0) eq 0 then begin
  print,'readasca, dname=dname, fname=fname, tname=tname, keyb,slist,blist'
 print,' ROUTINE TO READ ASCA FITS EVENT DATA FILES - CREATES ONE OR TWO' 
print,' PHOTON LISTS'
 print, ' '
 print,' works for sis FAINT, BRIGHT and BRIGHT2 modes and gis PH mode'
 print,' presently produces PHA values in photon list not PI'
 print,'** INPUTS ** '
print,' DNAME: directory containing the data files (specify last slash)'
print,' FNAME: file containing list of datafile names - use make_cat '
print,' or unix command of the form'
print,' ls *S0*2M.fits | grep =v HK > filename.dat '
print,' TNAME: file which will contain cumulative list of GTIs'
print,' (not yet implemented)'
print,' KEYB: intger - value gt 0 kills graphics and demands keyboard'
print,' input only (for diagnostic purposes only)'
print,' ** OUTPUTS **'
print,' SLIST: output photon list '
print,' BLIST: additional output photon list if desired (for bkgd)'
print, ' '
print,' ** USEAGE **'
print,' If no bkgd is desired simply type '
print,' READASCA,0,SLIST'
print,' and you will be prompted for the rest. If bkgd is reqd type '
print,' READASCA,0,SLIST,BLIST'
print,' Should eventually give the ratio of selected source to background '
print,' geometrical areas but does not yet'
retall
end
if n_elements(dname) eq 0 then begin
	dname = ' '
;	dname = '/asca/data/70001000/'
;	dname='/home2/yaqoob/images/'
	read,' Enter name of directory containing data ',dname
endif
if n_elements(fname) eq 0 then begin
	fname = ' '
;	fname = 'testdata2.dat'
;	fname='gis2test.dat'
	read,' Enter name of file containing data filenames ',fname
endif
if n_elements(tname) eq 0 then begin
	tname=' '
;	tname = 'time.dat'
	read,' Enter rootname of time intervals file ',tname
endif
ibgd =1
if n_params(0) lt 3 then ibgd = 0
;max number of expected files
fmax = 1000
;do we have faint mode sis data
ifaint = 0
;are we mixing faint and bright mode sis data
smix = 0
;do we have coverted faint mode data
ib2=0
datname = strarr(fmax)
mode=strarr(fmax)
brate=strarr(fmax)
neve=intarr(fmax)
instr=strarr(fmax)
obje=strarr(fmax)
neve=lonarr(fmax)
gtimx=1000
stdgti_beg=dblarr(gtimx) & stdgti_end=dblarr(gtimx)
allgti_beg=dblarr(gtimx) & allgti_end=dblarr(gtimx)
;spth=intarr(4,fmax)
; accumulate temporary image in here
image=intarr(884,882)
;open the names file for read
openr,1,fname
i = 0
toteve = 0l
stdname=tname+'_std.gti'
allname=tname+'_all.gti'
openw,3,stdname & nstdtot = 0l
openw,4,allname & nalltot = 0l
;LOOP FOR READING IN THE DATA FILES
while (not eof(1)) do begin
;	read header and print some stuff
	name = ' '
	readf,1,name
	 datname(i) = dname+name
	 if (strmid(dname,0,1)) eq ' ' then datname(i) = name
		hdr = headfits(datname(i))
;		print,' READ HEADER FOR FILE ',i+1
		obje(i) = sxpar(hdr,'OBJECT')
		instr(i) = sxpar(hdr,'INSTRUME')
		brate(i) = sxpar(hdr,'BIT_RATE')
		mode(i) = SXPAR(HDR,'DATAMODE')
		neve(i) = sxpar(hdr,'NEVENTS')
		dmode = strmid(mode(i),0,7)
		inst = strmid(instr(i),0,4)
		if i eq 0 then inst0 = inst
		instyp = strmid(instr(i),0,3)
		if (i gt 0) and (inst ne inst0) then begin
		 print,' ** Cant mix instruments !! **
		endif
		if ((dmode eq 'BRIGHT2') and (ib2 eq 0)) then begin
			mode(i) = 'BRIGHT '
			ib2 = 1
		 print,' ** At least one file is BRIGHT2 **'
		endif
		if ((dmode eq 'FAINT  ') and (ifaint eq 0)) then begin
			ifaint=1
			split = 40
		read,' Enter split threshold (try 40)',split
		read,' Enter maximum grade to keep (try 5)',gmax
		print,' Enter echo fraction: Kieth G thinks 0.014'
		read,' for S0 and 0.0087 for S1 ',echo
		endif
if ((dmode eq 'BRIGHT ') and (ifaint eq 1) and (smix eq 0)) then begin
	smix = 1
	print,'** You are mixing FAINT and BRIGHT mode ** '
	print,'** Make sure you are using the same split threshold **'
endif 
if neve(i) eq 0 then print,'no events skipping file',i+1
if neve(i) ne 0 then begin
;	spth(1,i) = hdr(73) & spth(2,i) =  hdr(74) 
;	spth(3,i) = hdr(75) & spth(4,i) = hdr(76) 
;read fits event table for this file
	tab1=readfits(datname(i),h1,ext=1)
;get x and y values from fits table
	x=tbget(h1,tab1,'X')
	y=tbget(h1,tab1,'Y')
;	print,'size x y',(size(x))(1), (size(y))(1)
; place events in image
	for j=0l,neve(i)-1 do begin
if (x(j) ge 0) and (x(j) le 883) and (y(j) ge 0) and (y(j) le 881) then begin
	image(x(j),y(j))=image(x(j),y(j))+1
endif
	endfor
	if (i eq 0) and (keyb eq 0) then begin
		window,0,xsize=884,ysize=882
		plot,x,y,psym=3,/xst,/yst
	endif
	if (i gt 0) and (keyb eq 0) then oplot,x,y,psym=3
	toteve = toteve + neve(i)
;get std and all gti's
	tab2=readfits(datname(i),h2,ext=2)
	stdgti_start=tbget(h2,tab2,'START') & stdgti_stop=tbget(h2,tab2,'STOP')
	tab3=readfits(datname(i),h3,ext=3)
	allgti_start=tbget(h3,tab3,'START') & allgti_stop=tbget(h3,tab3,'STOP')
	nstd =(size(stdgti_start))(1) & nall = (size(allgti_start))(1)
	print,'nstd nall',nstd,nall

	if i eq 0 then tzstd=stdgti_start(0:0)
	if i eq 0 then tzall=allgti_start(0:0)
for k=0,nstd-1 do begin
stdgti_beg(nstdtot+k)=stdgti_start(k) & stdgti_end(nstdtot+k)=stdgti_stop(k)
printf,3,stdgti_start(k),stdgti_stop(k),1
;stdgti_start(k)-tzstd,stdgti_stop(k)-tzstd,1
endfor
for k=0,nall-1 do begin
allgti_beg(nstdtot+k)=allgti_start(k) & allgti_end(nstdtot+k)=allgti_stop(k)
printf,4,allgti_start(k),allgti_stop(k),2
;allgti_start(k)-tzall,allgti_stop(k)-tzall,2
endfor
nstdtot = nstdtot+nstd & nalltot=nalltot+nall
endif
 i = i + 1
print,nstd,nall,nstdtot,nalltot
endwhile
	forprint,stdgti_beg(0:nstdtot-1),stdgti_end(0:nstdtot-1)
	print,' '
	forprint,allgti_beg(0:nalltot-1),allgti_end(0:nalltot-1)
close,1
nfiles=i
nf=nfiles-1
print,'         		NEVENTS  '
flx=indgen(nfiles)+1
forprint,instr(0:nf),mode(0:nf),brate(0:nf),neve(0:nf)
print,' TOTAL EVENTS ',toteve
;following are to be parameters describing physical selection regions
	gtregion,rtype,keyb,px,py,csx,csy,cbx,cby,ibgd
;	oplot, csx,csy & oplot, cbx,cby
; given a total of toteve events we can compute the number of
; events expected (ects) and actual (acts)
; which exceed more than n counts per pixel where
; n ranges from 0 to np
;THIS STUFF IS ONLY RELEVANT FOR THE SIS
if instyp eq 'SIS' then begin
npm=20 & acts = lonarr(npm)
for k=0,npm-1 do acts(k) = total(image eq k)
;estimate mean counts/pixel for comparing with Poisson distribution
;(normalize to the observed value of p(0)
events=float(toteve)
npix=float(884.*882.)
meanct = double(alog(npix/float(acts(0)))) & poisson,meanct,npm,ects
ects = npix*ects
;print,'events = ',events, meanct
if keyb eq 0 then window,1
np = indgen(npm)
if (keyb eq 0) then begin
plot,np,float(acts),/xst,/yst,yrange=[1,ects(0)],ytype=1
;plot,np,float(acts),/xst,/yst,yrange=[0.,ects(0)]
oplot,np,ects,linestyle=1
endif
print,' 	cts/pixel 	No. pixels	expected No. pixels '
forprint,np(0:npm-1),acts(0:npm-1),ects(0:npm-1)
print,' Choose a generous cutoff to remove persistent hot pixels '
print,' Accepted pixels will have conuts less than or equal to the cutoff '
read,' Enter cutoff for counts/pixel (-1 to override hp cut) ',hcut
if (hcut ge 0 ) then begin
	hpm = image le hcut
;xysis,hpm,xyl
	if total(hpm) le 0.0 then begin
	 print,' ** No events remaining !! ** ' & goto,fin1
	endif
endif 
;select chip
print,' chip layout for   	SIS0		SIS1'
print,' 			0  1		2  3'
print,'			3  2		1  0'
read,' select chip ',chip
endif
; now start another loop to read in the data, filtering as we go
nzero=0
stot = 0l
btot = 0l
for i=0,nf do begin
	dmode=strmid(mode(i),0,3)
;	print,' MODE(i) =',dmode
	if (neve(i) eq 0) then goto, skip
	tab=readfits(datname(i),h,ext=1)
	if (dmode eq 'BRI') then brplist,h,tab,tplist
	if (dmode eq 'FAI') then ftplist,h,tab,tplist
	if (dmode eq 'PH ') then gislist,h,tab,tplist
;	print,'tplist ',(size(tplist.x))(1)
;SIS ONLY
if instyp eq 'SIS' then begin
	chipsel = where(tplist.ccd eq chip)
	if (total(chipsel) lt 1.0) then goto, skip
	tplist = tplist(chipsel)
print,'Total events remaining after chip selection ',(size(tplist))(1)
;	print,max(tplist.ccd),min(tplist.ccd)
;	print,'calling hpmask'
;	hpmask,(tplist.x),(tplist.y),xyl,hout
;	if (total(hout) lt 1.0) then goto, skip
;	print,' total hout',total(hout)
;	tplist = tplist(where(hout))
;	print,' tplist ',(size(tplist.x))(1)
endif
;	print,' calling gtsbpl'
	gtsbpl,rtype,px,py,tplist,rslist,rblist,ibgd
	bsize=(size(rblist))(1)
	ssize=(size(rslist))(1)
	print,' selected srce ',ssize
	if ibgd gt 0 then print,' selected bkgd ',bsize 
	src=0l
	bgd=0l
	if (ssize gt 0) then begin

		if (dmode eq 'BRI') then tslist = rslist
		if (dmode eq 'PH ') then tslist = rslist
		if (dmode eq 'FAI') then begin
			convf2b,split,echo,gmax,rslist,tslist
		endif
		src = (size(tslist.pha))(1)
		if (stot eq 0l) then slist = tslist
		if (stot gt 0l) then slist=[slist,tslist]
		stot = stot + src
;		print,'src and size x',src,(size(tslist.x))(1)
;		print,' total source events',stot
	endif
	if ((bsize gt 0) and (ibgd gt 0)) then begin
		if (dmode eq 'BRI') then tblist = rblist
		if (dmode eq 'PH ') then tblist = rblist
		if (dmode eq 'FAI') then begin
			convf2b,split,echo,gmax,rblist,tblist
		endif
		bgd = (size(tblist))(1)
		if (btot eq 0.0) then blist = tblist
		if (btot gt 0.0) then blist = [blist,tblist]
		btot = btot + bgd
;	print,'bgd and size x',bgd,(size(tblist.x))(1)
;		print,'total bgd events ',btot
	endif

skip: print,' Done file ',i+1
endfor
if (keyb eq 0) then begin
window,0,xsize=600,ysize=600
if instyp eq 'SIS' then begin
plot,slist.x,slist.y,/xst,/yst,xrange=[0,884],yrange=[0,882],psym=3
endif
if instyp eq 'GIS' then begin
plot,slist.x,slist.y,/xst,/yst,xrange=[0,260],yrange=[0,260],psym=3
endif
if ibgd gt 0 then oplot,blist.x,blist.y,psym=3
oplot,csx,csy & if ibgd gt 0 then oplot, cbx,cby
endif
print,'grand total events in slist',(size(slist))(1)
if ibgd gt 0 then print,'grand total events in blist',(size(blist))(1)
if (instyp eq 'GIS') and (keyb eq 0) then begin
	window,1
	plot,slist.pha,slist.rt,/xst,/yst,psym=3
	window,2
	plot,blist.pha,blist.rt,/xst,/yst,psym=3
endif
;remove hot pixel from SIS data
if (instyp eq 'SIS') and (hcut ge 0) then begin
 print,'Removing hot pixels from slist'
	x=slist.x & y=slist.y
 smsk=intarr(stot) & for j=0l,stot-1 do smsk(j)=(image(x(j),y(j)) le hcut)
 slist = slist(where(smsk)) & stot=(size(slist))(1)
 print,'Total events in slist after removal',(size(slist))(1)
if ibgd gt 0 then begin
 print,'Removing hot pixels from blist'
	x=blist.x & y=blist.y
 bmsk=intarr(btot) & for j=0l,btot-1 do bmsk(j)=(image(x(j),y(j)) le hcut)
 blist = blist(where(bmsk)) & btot=(size(blist))(1)
 print,'Total events in blist after removal',(size(blist))(1)
endif
endif
print,' STD GTI intervals are: '
forprint,stdgti_beg(0:nstdtot-1),stdgti_end(0:nstdtot-1)
print,' ALL GTI intervals are: '
forprint,allgti_beg(0:nalltot-1),allgti_end(0:nalltot-1)
ans=' '
read,'Which GTI intervals to filter on?[std, all or <cr> for none] ',ans
if ans eq 'std' then begin
  ntimes=nstdtot & tbeg=stdgti_beg(0:ntimes-1) & tend=stdgti_end(0:ntimes-1)
  ans='y'
endif
if ans eq 'all' then begin
  ntimes=nalltot & tbeg=allgti_beg(0:ntimes-1) & tend=allgti_end(0:ntimes-1)
  ans='y'
endif
if ans eq 'y' then begin
  stmsk=intarr(stot) & stime=slist.time
  if ibgd gt 0 then btmsk=intarr(btot) & btime=blist.time
  for k=0,ntimes-1 do begin
    stmsk=stmsk+((stime gt tbeg(k)) and (stime lt tend(k)))
    if ibgd gt 0 then btmsk=btmsk+((btime gt tbeg(k)) and (btime lt tend(k)))
  endfor
    slist=slist(where(stmsk)) & stot=(size(slist))(1)
    print,' Events in slist after time filtering: ',stot
    if ibgd gt 0 then begin
        blist=blist(where(btmsk)) & btot=(size(blist))(1)
        print,' Events in blist after time filtering: ',btot
    endif
endif
close,1
close,3
close,4
fin1: return
end
