pro readasca,dname=dname,fname=fname,tname=tname,det,rmode,geo,slist,blist
; Author T. Yaqoob March 1993-** present time
;
;	DNAME - DIRECTORY NAME CONTAINING THE DATA FILES
;	FNAME - FILENAME CONTAINING THE DATA FILENAMES
;	TNAME - FILENAME OF OUTPUT TIME INTERVALS FILE
; *************
;print,n_params(0)
;print,n_elements(fname),n_elements(dname),n_elements(tname)
if n_params(0) eq 0 then begin
print,'readasca,dname=dname,fname=fname,tname=tname,det,rmode,geo,slist,blist'
 print,' ROUTINE TO READ ASCA FITS EVENT DATA FILES ' 
 print,'** INPUTS ** '
print,' DNAME: directory containing the data files (specify last slash)'
print,' FNAME: file containing list of datafile names - use mkflist '
print,' TNAME: rootname of files which will contain cumulative list of GTIs'
print,'      : produces tname_std.gti '             
print,' DET  : DET=0 reads RAW coords only. DET>0 reads all 3 sets ' 
print,' RMODE: = 0 :2 PASSES for readasca- users selects subregions '    
print,'        = 1 :1 PASS for readasca-use SAOimage region file to select'
print,'        = 2 :1 PASS for readasca- read in ALL data '
print,' ** OUTPUTS **'
print,' GEO  : structure specifying geometrical parameters of selected regions'
print,' SLIST: output photon list '
print,' BLIST: additional output photon list if desired (for bkgd)'
print,' Look at source code for help on GEO'
;structure GEO: GEO.TYPE (integer) - type of region. so far:1 = conc. circles
;		GEO.CEN1 (flt arr x 2) - x and y coords of cenre of region 1
;		GEO.CEN2 (flt arr x 2) - x and y coords of cenre of region 2
;		GEO.D1 (flt arr x 5) - dimensions specifying region 1
;		GEO.D2 (flt arr x 5) - dimensions specifying region 2
;		GEO.S1 (flt arr x 5) - squares of dimensions specifying region 1
;		GEO.S2 (flt arr x 5) - squares of dimensions specifying region 2
;		GEO.INST (integer ) - instrument (0-3)
;		GEO.DET  (integer ) - Type of Coord region selection
;		0=RAW, 1=DETECTOR, 2=sky
;		GEO.CHP (intger ) - SIS chip ID
;		GEO.BSCL (flt arr x 1) - area scaling for bgd region
; note: at present GEO.D1(4) and GEO.D2(4) contain the number of hot
; pixels removed from the source and bkgd regions respectively
retall
end
;HIDDEN PARAMETER DALLGTI=0 to supress reading of the 2nd GTI ext
dallgti=0
;HIDDEN PARAMETER KEYB=1 to turn off graphics
keyb=0
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
if n_params(0) lt 5 then ibgd = 0
;FATAL STOP if user requests reading all data and two photon lists
if rmode eq 2 and ibgd gt 0 then begin
 print,'*** CAN T HAVE TWO PHOTON LISTS IF READING ALL THE DATA *** '
 print,'*** SORRY ABORTED !! *** '
 retall
end
;max number of expected files
fmax = 1000
;do we have faint mode sis data
ifaint = 0
;are we mixing faint and bright mode sis data
smix = 0
;do we have coverted faint mode data
ib2=0
rtype=0
xlow=10000 & ylow =xlow & xhigh=0 & yhigh=0
datname = strarr(fmax)
mode=strarr(fmax)
brate=strarr(fmax)
neve=intarr(fmax)
maxeve=10
instr=strarr(fmax)
obje=strarr(fmax)
neve=lonarr(fmax)
pspcpass=lonarr(fmax)
pspcphot=20000l
gtimx=10000
stdgti_beg=dblarr(gtimx) & stdgti_end=dblarr(gtimx)
allgti_beg=dblarr(gtimx) & allgti_end=dblarr(gtimx)
;setup the GEO structure
if n_elements(geo) eq 0 then begin
dum0=fltarr(1) & dum1=fltarr(2) & dum2=fltarr(5)
geo={geomet,type:0,cen1:dum1,cen2:dum1,d1:dum2,d2:dum2,s1:dum2,s2:dum2,$
inst:0,det:0,chp:0,bscl:dum0}
endif
;spth=intarr(4,fmax)
; accumulate temporary image in here
nxd=864 & nyd=848
image=lonarr(nxd,nyd)
;open the names file for read
openr,1,fname
i = 0
toteve = 0l
totevesel=0l
stdname=tname+'_std.gti'
allname=tname+'_all.gti'
openw,3,stdname & nstdtot = 0l & nstd = 0l
openw,4,allname & nalltot = 0l & nall = 0l
ifirst=0 & eveext=1 
;LOOP FOR READING IN THE DATA FILES
while (not eof(1)) do begin
;	read header and print some stuff
	name = ' '
	readf,1,name
	 datname(i) = dname+name
	 if (strmid(dname,0,1)) eq ' ' then datname(i) = name
		hdr = headfits(datname(i))
		instr(i) = sxpar(hdr,'INSTRUME')
		obje(i) = sxpar(hdr,'OBJECT')
		neve(i) = sxpar(hdr,'NEVENTS')
	  if instr(i) eq '       0' then begin
;not ASCA data - must be PSPC by default
		hdr= headfits(datname(i),exten=3)
		instr(i) = sxpar(hdr,'INSTRUME')
		neve(i) = sxpar(hdr,'NAXIS2')
	  endif
;		print,' READ HEADER FOR FILE ',i+1
		brate(i) = sxpar(hdr,'BIT_RATE')
		mode(i) = SXPAR(HDR,'DATAMODE')
		dmode = strmid(mode(i),0,7)
		inst = strmid(instr(i),0,4)
		if inst eq 'PSPC' then eveext=3 
		if i eq 0 then inst0 = inst
		instyp = strmid(instr(i),0,3)
		sistyp=strmid(instr(i),3,1)
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
		print,' Enter echo fraction: '
		read,' Try 0.0 ',echo
		endif
if ((dmode eq 'BRIGHT ') and (ifaint eq 1) and (smix eq 0)) then begin
	smix = 1
	print,'** You are mixing FAINT and BRIGHT mode ** '
	print,'** Make sure you are using the same split threshold **'
endif 
if neve(i) le maxeve then print,'No. events less than MAXEVE skipping file',i+1
if neve(i) gt maxeve then begin
;	spth(1,i) = hdr(73) & spth(2,i) =  hdr(74) 
;	spth(3,i) = hdr(75) & spth(4,i) = hdr(76) 
;are we reading PSPC data? if so read the event list piecewise. 20000 photons
;at a time
;	if neve(i) gt pspcphot then begin
	  pspcpass(i)=(neve(i)/20000l) & print,pspcpass(i)+1,' TABLE PASSES'
;don't read data on this first round if rmode = 1 or 2 , just GTIs
 if rmode eq 0 then begin
	  for pk=0l,pspcpass(i) do begin
		start=pk*pspcphot 
	if pk eq pspcpass(i) then numrow = neve(i) - start else $
		numrow = pspcphot
		tab1=readfits(datname(i),h1,startrow=start,numrow=numrow,ext=eveext)
	if inst ne 'PSPC' then begin 	
		rawx=tbget(h1,tab1,'RAWX') 
		rawy=tbget(h1,tab1,'RAWY')
	endif
	if inst eq 'PSPC' then begin
		rawx=tbget(h1,tab1,'X')
		rawy=tbget(h1,tab1,'Y')
	endif
	if instyp ne 'SIS' then begin
	 x=rawx & y=rawy
	endif
	if instyp eq 'SIS' then begin
		ccd=tbget(h1,tab1,'CCDID')
		print,'min max CCDID ',minmax(ccd)
		raw2new,rawx,rawy,x,y,sistyp,ccd
	endif
;		if pk eq 0l then begin
;			x=xt & y=yt
;		endif
;		if pk gt 0l then begin
;		  x=[x,xt] & y=[y,yt]
;		endif
		print,' EVENT TABLE: read photons ',start+1,'-',start+numrow
;	   endfor
;	endif
;	if neve(i) le pspcphot then begin
;read fits event table for this file
;	tab1=readfits(datname(i),h1,ext=eveext)
;get x and y values from fits table
;	x=tbget(h1,tab1,'X')
;	y=tbget(h1,tab1,'Y')
;	endif
	if ifirst eq 0 then begin
		cumx=x & cumy=y
	endif
	if ifirst gt 0 then begin
		cumx=[cumx,x] & cumy=[cumy,y]
	endif
	xlow=min([xlow,x]) & ylow=min([ylow,y]) & xhigh=max([xhigh,x]) 
	yhigh=max([yhigh,y])
	print,'XY LIMITS',xlow,xhigh,ylow,yhigh
	if instyp eq 'GIS' then begin
	 posdet=sxpar(hdr,'POS_DET')
	 print,'POS_DET = ',posdet
	endif
	nplot=0l
;	nplot=min([pspcphot,neve(i)])
	if dmode eq 'FAINT  ' then begin
	 fphas=tbget(h1,tab1,'PHAS') & gtgrade,fphas,split,echo,grad
	 if min(grad) gt gmax then goto, contf
	print,'before grade selction: ',(size(x))(1)
	 x=x(where(grad le gmax)) & y=y(where(grad le gmax))
	print,' after grade selection ',(size(x))(1)
	endif
	nplot=(size(x))(1)
;print,'size x y',(size(x))(1), (size(y))(1)
; place events in image
if instyp eq 'SIS' then begin
	for j=0l,nplot-1 do begin
;	for j=0l,neve(i)-1 do begin
if (x(j) ge 0) and (x(j) le 863) and (y(j) ge 0) and (y(j) le 847) then begin
	image(x(j),y(j))=image(x(j),y(j))+1
endif
	endfor
endif
	if (ifirst eq 0) and (keyb eq 0) then begin
	ifirst=1
if instyp eq 'SIS' then	begin
	if !d.name eq 'X' then window,0,xsize=884,ysize=882
	plot,x,y,xrange=[0,884],yrange=[0,882],psym=3,/xst,/yst
endif
if inst eq 'PSPC' then begin
	if !d.name eq 'X' then window,0,xsize=1024,ysize=1024
	plot,x,y,xrange=[0,16384],yrange=[0,16384],psym=3,/xst,/yst
endif
if instyp eq 'GIS' then begin
	if !d.name eq 'X' then window,0,xsize=861,ysize=861
	plot,x,y,xrange=[0,230],yrange=[0,230],psym=3,/xst,/yst
endif
	endif
	if (ifirst gt 0) and (keyb eq 0) then oplot,x,y,psym=3
contf: totevesel=totevesel+nplot
endfor
;following ENDIF closes (rmode = 0 loop)
endif 
toteve = toteve + neve(i)
;contf: toteve=toteve+nplot
;get std and all gti's
     if inst eq 'PSPC' then begin
	 tab2=readfits(datname(i),h2,ext=1)
	 stdgti_start=tbget(h2,tab2,'START') & stdgti_stop=tbget(h2,tab2,'STOP')
	 allgti_start=stdgti_start & allgti_stop=stdgti_stop
     endif
     if inst ne 'PSPC' then begin
	tab2=readfits(datname(i),h2,ext=2)
	stdgti_start=tbget(h2,tab2,'START') & stdgti_stop=tbget(h2,tab2,'STOP')
	if dallgti gt 0 then begin
	 tab3=readfits(datname(i),h3,ext=3)
	 allgti_start=tbget(h3,tab3,'START') 
	 allgti_stop=tbget(h3,tab3,'STOP')
	endif
     endif
	nstd =(size(stdgti_start))(1) 
	if dallgti gt 0 then nall = (size(allgti_start))(1)
;	print,'nstd nall',nstd,nall
	if i eq 0 then tzstd=stdgti_start(0:0)
	if i eq 0 and dallgti gt 0 then tzall=allgti_start(0:0)
for k=0,nstd-1 do begin
stdgti_beg(nstdtot+k)=stdgti_start(k) & stdgti_end(nstdtot+k)=stdgti_stop(k)
;printf,3,stdgti_start(k),stdgti_stop(k),1
;stdgti_start(k)-tzstd,stdgti_stop(k)-tzstd,1
endfor
if dallgti gt 0 then begin
for k=0,nall-1 do begin
allgti_beg(nstdtot+k)=allgti_start(k) & allgti_end(nstdtot+k)=allgti_stop(k)
;printf,4,allgti_start(k),allgti_stop(k),2
;allgti_start(k)-tzall,allgti_stop(k)-tzall,2
endfor
endif
nstdtot = nstdtot+nstd 
if dallgti gt 0 then nalltot=nalltot+nall
endif
 i = i + 1
;print,nstd,nall,nstdtot,nalltot
endwhile
;	forprint,stdgti_beg(0:nstdtot-1),stdgti_end(0:nstdtot-1)
	print,' '
;	forprint,allgti_beg(0:nalltot-1),allgti_end(0:nalltot-1)
zstd=stdgti_beg(0) 
if dallgti gt 0 then zall=allgti_beg(0)
gtyp=1
for kk=0,nstdtot-1 do begin
printf,3,format='(4(F15.3,2X),I3)',stdgti_beg(kk),stdgti_end(kk),stdgti_beg(kk)-zstd,$
stdgti_end(kk)-zstd,gtyp
endfor
gtyp=2
if dallgti gt 0 then begin
for kk=0,nalltot-1 do begin
printf,4,format='(4(F15.3,2X),I3)',allgti_beg(kk),allgti_end(kk),allgti_beg(kk)-zall,$
allgti_end(kk)-zall,gtyp
endfor
endif
close,1
nfiles=i
nf=nfiles-1
print,'         		NEVENTS  '
flx=indgen(nfiles)+1
forprint,instr(0:nf),mode(0:nf),brate(0:nf),neve(0:nf)
print,' TOTAL EVENTS = ',totevesel,' OF POSSIBLE ',toteve
if geo.type eq 0 then begin
;following are to be parameters describing physical selection regions
if instyp eq 'SIS' then begin
 if sistyp eq '0' then geo.inst=0 & if sistyp eq '1' then geo.inst=1
endif
if instyp eq 'GIS' then begin
 if sistyp eq '2' then geo.inst=2 & if sistyp eq '3' then geo.inst=3
endif
if inst eq 'PSPC' then geo.inst = 5
;IF rmode =1 set  up geo from SAOimage file and JUMP TO READ IN DATA
;NOT YET IMPLEMENTED
;IF rmode = 2 then set up some stuff and JUMP TO READ IN DATA
if rmode eq 2 then begin
;hcut is the hot pixel cut-off (SIS) and chip=4 means get data from all chips
 if instyp eq 'SIS' then begin 
  chip = 4
  geo.chp=4
  hcut=-1
;  read,' Enter Hot Pixel cutoff (you have to guess) ',hcut
 endif
 goto, rdat
endif
if geo.type gt 0 then begin
print,' Using previously defined region from GEO: '
rtype=geo.type & print,' rtype: ',rtype
print,' cen1: ',geo.cen1
print,' cen2: ',geo.cen2
print,' d1  : ',geo.d1
print,' d2  : ',geo.d2
drawreg,geo,ibgd
endif
;***SELECT REGION***
print,' Use cursor to zoom in before region selection as follows: '
print,' Click to define center of region and then again to define '
print,' half width of new square region = distance from new center '
print,' Click at least once below X-axis to do nothing and continue '
cursor,xc,yc,4
if yc le 0.0 then goto, doreg
cnx=xc & cny=yc & oplot,[cnx,cnx],[cny,cny],psym=1
cursor,xc,yc,4
if yc le 0.0 then goto, doreg
cnx1=xc & cny1=yc
rcn= (cnx1-cnx)*(cnx1-cnx)+(cny1-cny)*(cny1-cny)
if rcn le 0.0 then goto, doreg else rcn=sqrt(rcn)
xrmin=cnx-rcn & xrmax=cnx+rcn & yrmin=cny-rcn & yrmax=cny+rcn
plot,cumx,cumy,/xst,/yst,xrange=[xrmin,xrmax],yrange=[yrmin,yrmax],psym=3
doreg:	gtregion,rtype,keyb,geo,ibgd
;********************
;setup geo structure - eventually this will be tidied up
;geo.type=rtype & geo.cen1(0)=px(0) & geo.cen1(1)=py(0)
;r1sq=(px(1)-px(0))*(px(1)-px(0))+(py(1)-py(0))*(py(1)-py(0))
;r2sq=(px(2)-px(0))*(px(2)-px(0))+(py(2)-py(0))*(py(2)-py(0))
;if r1sq le 0 then r1 =0.0 & if r1sq gt 0 then r1 = sqrt(r1sq)
;if r2sq le 0 then r2 =0.0 & if r2sq gt 0 then r2 = sqrt(r2sq)
;geo.d1(0)=r1 & geo.s1(0)=r1sq & geo.d2(0)=r2 & geo.s2(0)=r2sq
endif
;	oplot, csx,csy & oplot, cbx,cby
; given a total of toteve events we can compute the number of
; events expected (ects) and actual (acts)
; which exceed more than n counts per pixel where
; n ranges from 0 to np
;THIS STUFF IS ONLY RELEVANT FOR THE SIS
if instyp eq 'SIS' then begin
print,'Maximum number of cts/pixel = ',max(image)
npm=20 & acts = lonarr(npm)
for k=0,npm-1 do acts(k) = total(image eq k)
;estimate mean counts/pixel for comparing with Poisson distribution
;(normalize to the observed value of p(0)
events=float(toteve)
npix=float(884.*882.)
meanct = double(alog(npix/float(acts(0)))) & poisson,meanct,npm,ects
ects = npix*ects
;print,'events = ',events, meanct
if keyb eq 0 then if !d.name eq 'X' then window,1
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
chip=0
print,' chip layout for   	SIS0		SIS1'
print,' 			0  1		2  3'
print,'			3  2		1  0'
read,' select chip [4=all]',chip
geo.chp=chip
endif
; now start another loop to read in the data, filtering as we go
rdat: nzero=0
stot = 0l
btot = 0l
for i=0,nf do begin
	dmode=strmid(mode(i),0,3)
;	print,' MODE(i) =',dmode
	if (neve(i) le maxeve) then goto, skip2
	for pk=0l,pspcpass(i) do begin
	   if neve(i) le pspcphot then tab=readfits(datname(i),h,ext=eveext)
	   if neve(i) gt pspcphot then begin
		start=pk*pspcphot 
	if pk eq pspcpass(i) then numrow = neve(i) - start else $
		numrow = pspcphot
		tab=readfits(datname(i),h,startrow=start,numrow=numrow,ext=eveext)
		print,' EVENT TABLE: read photons ',start+1,' - ',start+numrow
	 endif
	if inst eq 'PSPC' then mkpspcpl,h,tab,tplist
	if (dmode eq 'BRI') then brplist,det,h,tab,tplist
	if (dmode eq 'FAI') then ftplist,det,h,tab,tplist
	if (dmode eq 'PH ') then gislist,det,h,tab,tplist
;	print,'tplist ',(size(tplist.x))(1)
;SIS ONLY
if instyp eq 'SIS' then begin
;convert x and y
 xold=tplist.x & yold=tplist.y
 raw2new,xold,yold,xnew,ynew,sistyp,tplist.ccd
 tplist.x=xnew & tplist.y=ynew
     if chip le 3 then begin
	chipsel = where(tplist.ccd eq chip)
	if (total(chipsel) lt 1.0) then goto, skip1
	tplist = tplist(chipsel) 
     endif
     sztp=(size(tplist))(1)
print,'Total events remaining after chip selection ',(size(tplist))(1)
; throw away unwanted grades
	if (dmode eq 'FAI') then begin
	print,'before grade selction: ',(size(tplist))(1)
		gtgrade,tplist.phas,split,echo,grad
	 if min(grad) gt gmax then goto, skip1
		tplist=tplist(where(grad le gmax))
		sztp=(size(tplist))(1)
		print,' after grade selection ',sztp
	endif
;remove hot pixels from SIS data
if hcut ge 0 then begin;
print,'Removing hot pixels from tplist'
	x=tplist.x & y=tplist.y
smsk=intarr(sztp) & for j=0l,sztp-1 do smsk(j)=(image(x(j),y(j)) le hcut)
	if total(smsk) le 0.0 then goto, skip1
tplist = tplist(where(smsk)) 
 print,'Total events in tplist after removal',(size(tplist))(1)
endif
;	print,max(tplist.ccd),min(tplist.ccd)
;	print,'calling hpmask'
;	hpmask,(tplist.x),(tplist.y),xyl,hout
;	if (total(hout) lt 1.0) then goto, skip
;	print,' total hout',total(hout)
;	tplist = tplist(where(hout))
;	print,' tplist ',(size(tplist.x))(1)
endif
;	print,' calling gtsbpl'
; if rmode = 2 we don't want to select on region
	if rmode le 1 then begin
	  gtsbpl,0,geo,tplist,rslist,rblist,ibgd,ssize,bsize
	  print,' selected srce ',ssize
	  if ibgd gt 0 then print,' selected bkgd ',bsize 
        endif
	if rmode eq 2 then begin
	  rslist=tplist
	  ssize=n_elements(rslist.time) & bsize = 0l
        endif
	src=0l
	bgd=0l
	null = 0
	if (ssize gt 0l) then begin
		if (dmode eq 'BRI') then tslist = rslist
		if (dmode eq 'PH ') then tslist = rslist
		if (inst eq 'PSPC') then tslist = rslist
		if (dmode eq 'FAI') then begin
			convf2b2,det,split,echo,gmax,rslist,tslist,null
		endif
		if null ge 0 then begin
		src = (size(tslist.pha))(1)
		if (stot eq 0l) then slist = tslist
		if (stot gt 0l) then slist=[slist,tslist]
		stot = stot + src
		endif
;		print,'src and size x',src,(size(tslist.x))(1)
;		print,' total source events',stot
	endif
	null = 0
	if ((bsize gt 0l) and (ibgd gt 0)) then begin
		if (dmode eq 'BRI') then tblist = rblist
		if (dmode eq 'PH ') then tblist = rblist
		if (inst eq 'PSPC') then tblist = rblist
		if (dmode eq 'FAI') then begin
			convf2b2,det,split,echo,gmax,rblist,tblist,null
		endif
		if null ge 0 then begin
		bgd = (size(tblist))(1)
		if (btot eq 0.0) then blist = tblist
		if (btot gt 0.0) then blist = [blist,tblist]
		btot = btot + bgd
		endif
;	print,'bgd and size x',bgd,(size(tblist.x))(1)
;		print,'total bgd events ',btot
	endif
skip1: print,' Done pass ',pk+1,' for File ',i+1
  endfor
skip2: print,' Done File ',i+1
endfor
if (keyb eq 0) then begin
if !d.name eq 'X' then window,0,xsize=600,ysize=600
if instyp eq 'SIS' then begin
plot,slist.x,slist.y,/xst,/yst,xrange=[0,884],yrange=[0,882],psym=3
endif
if instyp eq 'GIS' then begin
plot,slist.x,slist.y,/xst,/yst,xrange=[-47,278],yrange=[-47,278],psym=3
endif
if inst eq 'PSPC' then begin
plot,slist.x,slist.y,/xst,/yst,xrange=[0,16384],yrange=[0,16384],psym=3
endif
if ibgd gt 0 then oplot,blist.x,blist.y,psym=3
drawreg,geo,ibgd
endif
print,'grand total events in slist',(size(slist))(1)
if ibgd gt 0 then print,'grand total events in blist',(size(blist))(1)
if (instyp eq 'GIS') and (keyb eq 0) then begin
	if !d.name eq 'X' then window,1
	plot,slist.pha,slist.rt,/xst,/yst,psym=3
  if ibgd gt 0 then begin
	if !d.name eq 'X' then window,2
	plot,blist.pha,blist.rt,/xst,/yst,psym=3
  endif
endif
;remove hot pixel from SIS data
;if (instyp eq 'SIS') and (hcut ge 0) then begin
; print,'Removing hot pixels from slist'
;	x=slist.x & y=slist.y
; smsk=intarr(stot) & for j=0l,stot-1 do smsk(j)=(image(x(j),y(j)) le hcut)
; slist = slist(where(smsk)) & stot=(size(slist))(1)
; print,'Total events in slist after removal',(size(slist))(1)o;if ibgd gt 0 then begin
; print,'Removing hot pixels from blist'
;	x=blist.x & y=blist.y
; do bmsk(j)=(image(x(j),y(j)) le hcut)
; blist = blist(where(bmsk)) & btot=(size(blist))(1)
; print,'Total events in blist after removal',(size(blist))(1)
;endif
;endif
;print,' STD GTI intervals'
;forprint,stdgti_beg(0:nstdtot-1),stdgti_end(0:nstdtot-1)
;print,' ALL GTI intervals are: '
;forprint,allgti_beg(0:nalltot-1),allgti_end(0:nalltot-1)
ans=' '
read,'Which GTI intervals to filter on?[std,  or <cr> for none] ',ans
if ans eq 'std' then begin
  ntimes=nstdtot & tbeg=stdgti_beg(0:ntimes-1) & tend=stdgti_end(0:ntimes-1)
  ans='y'
endif
if dallgti gt 0 then begin
if ans eq 'all' then begin
  ntimes=nalltot & tbeg=allgti_beg(0:ntimes-1) & tend=allgti_end(0:ntimes-1)
  ans='y'
endif
endif
if ans eq 'y' then begin
  stmsk=intarr(stot) & stime=slist.time
  if ibgd gt 0 then  begin
   btmsk=intarr(btot) & btime=blist.time
  endif
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
;if sis data check whether any events fall outside chip boundaries
;if geo.inst lt 2 then begin
;	print,'Checking with chip boundaries ',geo.inst,geo.chp
;	chipbnd,geo.inst,geo.chp,xl,xu,yl,yu
; 	x=slist.x & y=slist.y
;	slist=slist(where((x ge xl)and(x le xu)and(y ge yl)and(y le yu)))
;	stot=(size(slist))(1) 
;	print,' Events in slist after chip tidying ',stot
;	if ibgd gt 0 then begin
;	 x=blist.x & y=blist.y
;	blist=blist(where((x ge xl)and(x le xu)and(y ge yl)and(y le yu)))
;	btot=(size(blist))(1) 
;	print,' Events in blist after chip tidying ',btot
;	endif
;endif
nsrce=0l & nbkgd=0l
if (instyp eq 'SIS')  then begin
 if hcut ge 0  then begin
;write out the hot pixel list
hpm = image gt hcut 
  wh=where((hpm),nwh)
  if nwh gt 0 then begin
    openw,5,tname+'.hot'
    hy=wh/nxd & hx=wh-nxd*hy
    fchp2ccd,sistyp,hx,hy,hccd
    wduff=where((hccd lt 0),nhduff)
    if nhduff gt 0 then print,'** WARNING ** ',nduff,' Duff CCDIDs'
    hx=hx(where(hccd ge 0)) & hy=hy(where(hccd ge 0))
    hccd=hccd(where(hccd ge 0))
    fchp2raw,hx,hy,hxr,hyr,sistyp,hccd
    nwhw=n_elements(hccd)
    print,'Total number of Hot Pixels removed in ',inst,' = ',nwhw
    for hn=0l,nwhw-1 do printf,5,hccd(hn),hxr(hn),hyr(hn) 
;nhotpix,geo,nsrce,nbkgd,hpm,ibgd 
;geo.d1(4)=nsrce & geo.d2(4)=nbkgd
;print,' Source pixels removed: ',nsrce
;if ibgd gt 0 then print,' Bkgd pixels removed: ',nbkgd
  endif
 endif
endif
close,1
close,2
close,3
close,4
close,5
fin1: return
end
