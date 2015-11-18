pro absisrd,dname=dname,fname=fname,tname=tname,slist,blist
;
; READ ASCA SIS BRIGHT-MODE DATA, get the data file names from the 
; file fname. The routine will also create photons list for  two
; regions of a selected chip and time intervals file, containing
; initially only the gti's but which is later modified by other
; routines to include other times selected by the user. At present
; the time interval codes are as follows: 1 = std gti, 2=all gti,
; 3 = data excursions above maximum level, 4=hand picked, 5 & greater
; HK time selections. The photon list will have hot pixels removed.
; INPUTS:
;	DNAME - DIRECTORY NAME CONTAINING THE DATA FILES
;	FNAME - FILENAME CONTAINING THE DATA FILENAMES
;	TNAME - FILENAME OF OUTPUT TIME INTERVALS FILE
; *************
print,n_params(0)
print,n_elements(fname),n_elements(dname),n_elements(tname)
;if n_params(0) eq 0 then begin
;  print,'absisrd dname=dname fname=fname tname=tname'
;  print,' READS SIS BRIGHT MODE DATA & CREATES 2 PHOTON LISTS AND' 
;  print,' A TIME INTERVAL FILE '
;retall
;end
if n_elements(dname) eq 0 then begin
	dname = ' '
	dname = '/asca/data/70001000/'
;  	read,' Enter name of directory containing data ',dname
endif
if n_elements(fname) eq 0 then begin
	fname = ' '
	fname = 'testdata2.dat'
;	read,' Enter name of file containing data filenames ',fname
endif
if n_elements(tname) eq 0 then begin
	tname = 'time.dat'
;	read,' Enter name of time intervals file ',tname
endif
;max number of expected files
fmax = 1000
;do we have faint mode sis data
ifaint = 0
;are we mixing faint and bright mode sis data
smix = 0
datname = strarr(fmax)
mode=strarr(fmax)
brate=strarr(fmax)
neve=intarr(fmax)
instr=strarr(fmax)
obje=strarr(fmax)
;spth=intarr(4,fmax)
; accumulate temporary image in here
image=intarr(884,882)
;open the names file for read
openr,1,fname
i = 0
toteve = 0l
openw,3,tname
;LOOP FOR READING IN THE DATA FILES
while (not eof(1)) do begin
;	read header and print some stuff
	name = ' '
	readf,1,name
		datname(i) = dname+name
		hdr = headfits(datname(i))
;		print,' READ HEADER FOR FILE ',i+1
		obje(i) = sxpar(hdr,'OBJECT')
		instr(i) = sxpar(hdr,'INSTRUME')
		brate(i) = sxpar(hdr,'BIT_RATE')
		mode(i) = SXPAR(HDR,'DATAMODE')
		neve(i) = sxpar(hdr,'NEVENTS')
		if ((mode(i).eq.'FAINT') and (ifaint eq 0)) then do begin
			ifaint=1
			split = 40
		read,' Enter split threshold',split
		endif
if ((mode(i) eq 'BRIGHT') and (ifaint eq 1) and (smix eq 0) then do begin
	smix = 1
	print,'** You are mixing FAINT and BRIGHT mode ** '
	print,'** Make sure you are using the same split threshold **'
endif 
if neve(i) eq 0 then print,'no events skipping file',i+1
if neve(i) ne 0 then begin
;	spth(1,i) = hdr(73) & spth(2,i) =  hdr(74) 
;	spth(3,i) = hdr(75) & spth(4,i) = hdr(76) 
	forprint,(i+1),' ',obje(i),instr(i),brate(i),mode(i),neve(i)
;read fits event table for this file
	tab1=readfits(datname(i),h1,ext=1)
;get x and y values from fits table
	x=tbget(h1,tab1,'X')
	y=tbget(h1,tab1,'Y')
	print,'size x y',(size(x))(1), (size(y))(1)

; place events in image
	for j=0l,neve(i)-1 do begin
if (x(j) ge 0) and (x(j) le 883) and (y(j) ge 0) and (y(j) le 881)$
then begin
	image(x(j),y(j))=image(x(j),y(j))+1
endif
	endfor
	if i eq 0 then begin
		window,0
		plot,x,y,psym=3,/xst,/yst
	endif
	if i gt 0 then oplot,x,y,psym=3
	toteve = toteve + neve(i)
	print,'toteve = ',toteve
;get std and all gti's
	tab2=readfits(dataname(i),h2,ext=2)
	stdgti_start=tbget(h2,tab2,'START') & stdgt_stop=tbget(h2,tab2,'STOP')
	tab3=readfits(dataname(i),h3,ext=3)
	allgti_start=tbget(h3,tab3,'START') & allgti_stop=tbget(h3,tab3,'STOP')
	nstd =(size(stdgt_start))(1) & nall = (size(allgti_start))(1)
	forprint,text=tname,stdgti_start(0:nstd-1),stdgti_stop(0:nstd-1),0,1
	forprint,text=tname,allgti_start(0:nall-1),allgti_stop(0:nall-1),0,2
 endif
 i = i + 1
endwhile
close,1
nfiles=i
;following are to be parameters describing physical selection regions
	gtregion,rtype,px,py,csx,csy,cbx,cby
;	oplot, csx,csy & oplot, cbx,cby
; given a total of toteve events we can compute the number of
; events expected (ects) and actual (acts)
; which exceed more than n counts per pixel where
; n ranges from 0 to np
npm=11 & acts = lonarr(npm)
for k=0,npm-1 do acts(k) = total(image eq k)
;estimate mean counts/pixel for comparing with Poisson distribution
;(normalize to the observed value of p(0)
events=float(toteve)
npix=float(884.*882.)
meanct = double(alog(npix/float(acts(0)))) & poisson,meanct,npm,ects
ects = npix*ects
print,'events = ',events, meanct
openw,2,'hotpixels.qdp'
window,1
;	for k=1, do begin
;		mfac = mfac*meanct/float(k)
;		prob = prob + mfac
;		ects(k) = events*(1.-prob)
;	endfor
np = indgen(npm)
plot,np,float(acts),/xst,/yst,yrange=[ects(npm-1),ects(0)],ytype=1
oplot,np,ects,linestyle=1
forprint,np(0:npm-1),acts(0:npm-1),ects(0:npm-1)
forprint,text='hotpixels.qdp',np(0:npm-1),acts(0:npm-1),ects(0:npm-1)
close,2
read,' Enter cutoff for counts/pixel ',hcut
hpm = image gt hcut & xysis,hpm,xyl
;select chip
print,' chip layout for   	SIS0		SIS1'
print,' 			2  3		0  1'
print,'			1  0		3  2'
read,' select chip ',chip
; now start another loop to read in the data, filtering as we go
nzero=0
stot = 0.0
btot = 0.0
for i=0,nfiles-1 do begin
	if (neve(i) eq 0) then goto, skip
	tab=readfits(datname(i),h,ext=1)
	brplist,h,tab,tplist
	print,'tplist ',(size(tplist.x))(1)
	chipsel = where(tplist.ccd eq chip)
	if (total(chipsel) lt 1.0) then goto, skip
	tplist = tplist(chipsel)
	print,'tplist ',(size(tplist.x))(1)
	print,max(tplist.ccd),min(tplist.ccd)
	hpmask,(tplist.x),(tplist.y),xyl,hout
	if (total(hout) lt 1.0) then goto, skip
;	print,' total hout',total(hout)
	tplist = tplist(where(hout))
	print,' tplist ',(size(tplist.x))(1)
	gtsbpl,rtype,px,py,tplist,ssel,bsel
;	print,'tot ssel bsel',total(ssel),total(bsel)
	print,'size ssel',(size(ssel))(0),(size(ssel))(1)
	src = (size(ssel))(1)
	bgd = (size(bsel))(1)
	if (total(ssel) gt 0.0) then begin
		if (stot eq 0.0) then slist = tplist(ssel)
		if (stot gt 0.0) then slist=[slist,tplist(ssel)]
		stot = stot + src
		print,'src and size x',src,(size(tplist(ssel).x))(1)
		print,' total source events',stot
	endif
	if (total(bsel) gt 0.0) then begin
		if (btot eq 0.0) then blist = tplist(bsel)
		if (btot gt 0.0) then blist = [blist,tplist(bsel)]
		btot = btot + bgd
		print,'bgd and size x',bgd,(size(blist.x))(1)
		print,'total bgd events ',btot
	endif

skip: print,' Done file ',i+1
endfor
window,0
plot,slist.x,slist.y,/xst,/yst,xrange=[0,884],yrange=[0,882],psym=3
oplot,blist.x,blist.y,psym=3
oplot,csx,csy & oplot, cbx,cby
return
end
