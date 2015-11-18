pro telemgti,flist,elist,satfile=satfile,dir=dir,gtiname=gtiname,goodfile=goodfile
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
  print,'telemgti,flist,elist,satfile=satfile,dir=dir,gtiname=gtiname,goodfile=goodfile'
  print,' Search for SIS telem saturation and produce'
  print,' GTI file and last pixel list. Also produce corner pixel'
  print,' data for FAINT mode SIS files. '
  print,'*** EVENTS FILES MUST NOT BE FILTERED IN ANY WAY ***'
  print,'**INPUTS**'
  print,'FLIST	: File containing list of event filenames '
  print,'SATFILE: Rootname of telemetry sat and faint info files'
  print,'DIR	: Directory containing the events files '
  print,'GTINAME: Name of output file containing GTIs '
  print,'GOODFILE: New event file list excluding completely '
  print,'	   saturated files '
  retall
endif
dumx=0 & dumy=0
totrej=0l
 xm=424 & ym=424
; expmap=fltarr(4,xm+1,ym+1)
if n_elements(satfile) eq 0 then begin
 satfile=' '
 read,'Enter rootname of output telemetry sat and FAINT info files',satfile
endif
openw,4,satfile+'.sat'
if n_elements(flist) eq 0 then begin 
 flist=' '
 read,'Enter name of dir containing events files (include slash)',dir
endif
if n_elements(gtiname) eq 0 then begin
 gtiname=' '
 read,'Enter name of output GTI file ',gtiname
endif
if n_elements(goodfile) eq 0 then begin
 goodfile=' '
 read,'Enter name of file to contain good filenames ',goodfile 
endif
openw,3,goodfile
ityp=9
fmax=1000
instr=strarr(fmax) & mode=strarr(fmax) & brate=strarr(fmax) &
datname=brate & obje=brate & fname=brate
i=0 & ifirst=0 
rate=intarr(fmax) & modi=rate 
openr,2,flist
;LOOP FOR READING IN THE DATA FILES
while (not eof(2)) do begin
	name=' '
	readf,2,name
	 fname(i) = name
	 if strmid(dir,0,1) eq ' ' then  datname(i)=name else $
	 datname(i)=dir+name	 
	 hdr = headfits(datname(i))
	 instr(i) = sxpar(hdr,'INSTRUME')
	 obje(i) = sxpar(hdr,'OBJECT')
	 brate(i) = sxpar(hdr,'BIT_RATE')
	 mode(i) = SXPAR(HDR,'DATAMODE')
	 dmode = strmid(mode(i),0,7)
	 inst = strmid(instr(i),0,4)
 	 if ifirst eq 0 then begin
		print,' Files for ',inst
		inst0=inst
		ifirst=1
	 endif	
	 instyp = strmid(instr(i),0,3)
	 if instyp ne 'SIS' then begin
	  print,'Found Non-SIS file **ABORTING** '
	  goto, fin
	 endif 
	 if inst ne inst0 then begin
	  print,'You are mixing SIS0 and SIS1 **ABORTING**'
	  goto, fin
	 endif
	 modi(i) = -1
	 if dmode eq 'FAINT  ' then modi(i) = 0 
	 if dmode eq 'BRIGHT ' then modi(i) = 1
	 if modi(i) lt 0 then begin
	  print,'DATAMODE must be FAINT or BRIGHT  **ABORTING**'
	  goto, fin 
	 endif 
	 if strmid(brate(i),0,1) eq 'L' then rate(i)=0
	 if strmid(brate(i),0,1) eq 'M' then rate(i)=1
	 if strmid(brate(i),0,1) eq 'H' then rate(i)=2
	 i=i+1
	print,'File ',i,' ',dmode,' ',brate(i-1)
endwhile
nfiles=i
totgti=0l
openw,1,gtiname
wfnt=where((modi eq 0),nfnt)
;if nfnt gt 0 then openw,9,satfile+'.finfo'
for jj=0,nfiles-1 do begin
 ngti=0l
 limit=2^(2*modi(jj))*(2^((2^rate(jj))+rate(jj)))
print,'Telemtry limit (cts/s/SIS)',limit
;for a given chip in a readout, the maximum number of cts is 4*limit
;create a plist for this file
tab=readfits(datname(jj),h,ext=1)
if modi(jj) eq 0 then ftplist,0,h,tab,plist
if modi(jj) eq 1 then brplist,0,h,tab,plist 
np=0l
np=(size(plist))(1) 
maxcts = 4*limit
nread=0
told=(plist.time)(0)
cts=0
cread=fltarr(4) & nwc=lonarr(4)
chmax=max(plist.ccd) & chmin=min(plist.ccd)
print,'MIN MAX CCD ID: ',chmin,chmax
; 	print,'total events = ',np
	time=plist.time & stime=time(sort(time))
	print,format='(2(F15.3,1X))',min(time),max(time)
	x=plist.x & y=plist.y & ccd=plist.ccd
	x=x(sort(time)) & y=y(sort(time)) & ccd=ccd(sort(time))
	t0=min(stime)-2.0
	if jj eq 0 then tmin=min(time)
	thist=histogram(stime,min=t0,binsize=4)
	nbins=(size(thist))(1)
	print,nbins,' total 4s intervals '
	nbnz=(size(where(thist gt 0)))(1)
	print,nbnz,' total readouts '
	for k=chmin,chmax do begin
	 wc=where((plist.ccd eq k),nww)
	 nwc(k)=nww
	 if nwc(k) gt 0 then begin 
	  qlist=plist(wc)
	  if nwc(k) gt 1 then qtime=qlist.time else $
 qtime=qlist.time+dblarr(1)
	  chist=histogram(qtime,min=t0,binsize=4)
	  cread(k)=(size(where(chist gt 0)))(1)
	  print,long(cread(k)),' readouts for chip ',k
	 endif
	endfor
;FIRST WRITE OUT STUFF FOR FAINT MODE DATA
	if modi(jj) eq 0 then begin
        if n_params(0) eq 2 then begin
	  if n_elements(elist) eq 0 then elist=plist else $
	elist=[elist,plist]
        endif
;	  print,'MIN MAX PHAS',minmax(plist.phas)
	  phas=plist.phas & faint2b,phas,40,0.0,pha,grade
	  nphas=(size(grade))(1)
;	  print,'MIN MAX PHAS',minmax(phas) 
	  phas=phas(*,sort(time)) & grade=grade(sort(time))
	  pha=pha(sort(time))
;centre pixel
	  pkpha=phas(0:0,*)+lonarr(nphas)
;coner pixels
  pcnr=phas(1:1,*)+phas(3:3,*)+phas(6:6,*)+phas(8:8,*)+lonarr(nphas)
;	print,'MIN MAX PCNR ',minmax(pcnr)
;'anti'-corner pixels
  pacnr=phas(2:2,*)+phas(4:4,*)+phas(5:5,*)+phas(7:7,*)+lonarr(nphas)
  	for j=0l,nbins-1 do begin
		ix2= total(thist(0:j))-1l
		ix1= ix2-thist(j)+1l
		nevts=float(thist(j))
          if thist(j) gt 0 then begin
	 	avepk=total(float(pkpha(ix1:ix2)))/nevts
	       avecnr=total(float(pcnr(ix1:ix2)))/nevts
	      aveacnr=total(float(pacnr(ix1:ix2)))/nevts
              avegrd =total(float(grade(ix1:ix2)))/nevts	
;	printf,9,format='(I3,3X,3(F9.2,3X),F3.1,3X,F15.3)',ccd(ix1),$
; avepk,avecnr,aveacnr,avegrd,stime(ix1)
	  endif
	endfor
	endif
;DONE FAINT MODE STUFF
	sat=intarr(nbins)
	wsat=where((thist ge maxcts),nwsat)
	if wsat(0) eq 0 then t1 = stime(0)
	print,nwsat,' saturated frames '
	if nwsat ge nbins then begin
	 print,'No UNSATURATED frames - No GTIs for this file'
	 totrej=totrej+(nbins*4*limit)
	 for j=0l,nbins-1 do begin
		ix1=total(thist(0:j))-1
  	  if thist(j) gt 0 then $
  printf,4,format='(3(I4,2X),F15.3)',ccd(ix1),x(ix1),y(ix1),stime(ix1)
	 endfor
	 goto, nxtfil 
	endif
	if nwsat eq 0 then begin
	 print,'No SATURATED frames - 1 GTI '
	 t1=stime(0)-1.0 & t2=stime(np-1)+1.0
	 printf,1,format='(4(F15.3,2X),I3)',t1,t2,t1-tmin,t2-tmin,ityp
	 ngti=1l
;        if doexp gt 0 then begin
;	 for k=chmin,chmax do begin
;	  if nwc(k) gt 0 then begin
;	   expmap(k:k,0:xm,0:ym)=expmap(k:k,0:xm,0:ym)+cread(k)
;	  endif
;	 endfor
;	endif
	for j=0l,nbins-1 do begin
		ix1=total(thist(0:j))-1
	if thist(j) gt 0 then printf,4,format='(3(I4,2X),F15.3)',ccd(ix1),dumx,dumy,stime(ix1)
	endfor
	 printf,3,fname(jj)
	 goto, nxtfil 
	endif
	if nwsat gt 0 then begin 
	 totrej=totrej+(4*limit*nwsat)
	 printf,3,fname(jj)
	 sat(wsat) = 1
 	 if sat(0) eq 0 then t1=stime(0)-1.0 else t1=-1.0 
	 for j = 0l,nbins-1 do begin
	  ix1=total(thist(0:j))-1
	  ix2=total(thist(0:j))
	 if sat(j) eq 0 and thist(j) gt 0 then $
    printf,4,format='(3(I4,2X),F15.3)',$ 
 ccd(ix1),dumx,dumy,stime(ix1)
;	 if doexp gt 0 then begin
;	  if sat(j) eq 0 then expmap(ccd(ix1),0:xm,0:ym) = $
;	  expmap(ccd(ix1),0:xm,0:ym)+1.0
;	  if sat(j) eq 1 then begin
;		xx=x(ix1) & yy=y(ix1)
;	  expmap(ccd(ix1),0:xm,0:(yy-1))=expmap(ccd(ix1),0:xm,0:(yy-1))+1.0
;	  expmap(ccd(ix1),0:xx,yy:yy)=expmap(ccd(ix1),0:xx,yy:yy)+1.0
;	  endif
;	 endif
	  if sat(j) eq 1 then begin
	   print,j+1,ccd(ix1),stime(ix1),x(ix1),y(ix1),thist(j),sat(j)
	 if thist(j) gt 0 then printf,4,format='(3(I4,2X),F15.3)',ccd(ix1),x(ix1),y(ix1),stime(ix1)
	  endif
	 if t1 gt -1.0 then begin
	  if j lt nbins-1 then begin
	   if sat(j) eq 0 and sat(j+1) eq 1 then begin
		t2=stime(ix2)-1.0
	 	ngti=ngti+1
	     printf,1,format='(4(F15.3,2X),I3)',t1,t2,t1-tmin,t2-tmin,ityp 
             t1=-1.0
	   endif
	  endif
	 endif
	 if t1 le -1.0 and j lt nbins-1 then begin
	   if sat(j) eq 1 and sat(j+1) eq 0 then t1=stime(ix2)-1.0
	 endif
;	if n_elements(t1) gt 0 then print,'t1 = ',t1
;	if n_elements(t2) gt 0 then print,'t2 = ',t2
	endfor
	if t1 gt -1.0 then begin
	 t2=stime(np-1)+1.0
	 printf,1,format='(4(F15.3,2X),I3)',t1,t2,t1-tmin,t2-tmin,ityp
	endif
    endif
nxtfil: print,'Done file ',jj+1,' No. GTIs ',ngti
	totgti=totgti+ngti
endfor
print,'Total No. of GTIs = ',totgti
fin: close,1
close,2
close,3
close,4
close,9
print,'Excluding telem sat frames will reject ',totrej,' events '
return
end
