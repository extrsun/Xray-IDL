pro mkhkgti,hkinfo,hk,fov,xte=xte,mkfnames=mkfnames
;Author T. Yaqoob - March 1993 ->**
if n_params(0) eq 0 then begin
 print,'mkhkgti, hkinfo, hk, xte=xte, mkfnames=mkfnames'
 print,' Make or update structures detailing HK selection crieria '
 print,' The program also creates a GTI file corresponding to the'
 print,' HK selection criteria.  Program creates a structure if one '
 print,' doesn t exist already otherwise it modifies the existing '
 print,' structure. The organsization of the HKINFO structure  is as follows:'
 print,'HKINFO.TARGET 	 -name of target '
 print,'HKINFO.NFILES 	-number of input files '
 print,'HKINFO.FNAME(10)	-input file names from MKFILTER '
 print,'HKINFO.GTIFIL	-name of output GTI file '
 print,' organization of the HK structure is :'
 print,'HK.NAME	-name of the HK parameter '
 print,'HK.NWIN	-number of selected include windows <=100'
 print,'HK.WIND(100,2)-inclusive window selections '
 print,'HK.FLAG	- non zero value means don t select on this '
 print,' most commonly used parameters are '
 print,' START END TIME BIT_RATE Z_ALPHA Z_DELTA '
 PRINT,' EULER_3 ELV_MIN ELV_MAX COR_MIN COR_MAX'
 PRINT,' SAA (1=YES) FOV(0=SKY) NSAS (sun angle) '
 PRINT,' S0_MODE S1_MODE '
 PRINT,'0:fnt 1:br 2:fst 3:frm 4:Dkfrm 5:Hst 6:intg '
 print,'Modified August 1996 for use with XTE .xfl files - '
 print,'SET XTE=1'
 retall
end
if n_elements(hkinfo) eq 0 then begin
 print,' HKINFO structure doesn t exist. Creating new one '
 tgt=strarr(1) & fnam=strarr(100) & gtif=strarr(1)
 hkinfo={hki,target:tgt,nfiles:0,fname:fnam,gtifil:gtif}
 read,'Enter target name ',tgt & hkinfo.target=tgt
 read,'Enter number of .mkf files ',nmkf & hkinfo.nfiles=nmkf
 hkinfo.gtifil='none'
 if n_elements(mkfnames) eq 0 then begin
 for k=0,nmkf-1 do begin
  mname=' '
  read,'Enter .mkf filename ',mname
  fnam(k)=mname 
 endfor
 endif
 if n_elements(mkfnames) ne 0 then fnam(0:nmkf-1)=mkfnames(0:nmkf-1)
  hkinfo.fname=fnam
endif
print,'Current output GTI file is ',hkinfo.gtifil
gname=' '
read,'Enter rootname of  new output GTI file ',gname
hkinfo.gtifil=gname
;create the hk sel structure
nam=strarr(1) & window=fltarr(100,2) 
hkt={hksel,name:nam,nwin:0,wind:window,flag:0}
;how big is the structure
hksiz=0
if n_elements(hk) gt 0 then hksiz=(size(hk))(1)
print,' Current number of HK parameters in structure',hksiz
again: hkname=' '
read,'Enter name of HK parameter to add (or none )',hkname
fnam=(hkinfo.fname)
if hkname eq 'none' then begin
  for k=0,hkinfo.nfiles-1 do begin
     tab=readfits(fnam(k),hin,ext=1)
     hkcorhead,hin,h
     timtmp=tbget(h,tab,'TIME')
     if xte eq 0 then fovtmp=tbget(h,tab,'FOV') else $
	tableget,fnam(k),fovtemp,'time',1,xte=xte
     if k eq 0 then time=timtmp else time=[time,timtmp]
     if k eq 0 then fov=fovtmp else fov=[fov,fovtmp]
  endfor
  goto,cont1
endif
;read .mkf files

for k=0,hkinfo.nfiles-1 do begin
 tab=readfits(fnam(k),hin,ext=1)
 hkcorhead,hin,h
 if hkname ne 'BIT_RATE' then begin
  if xte eq 0 then begin
	valtemp=tbget(h,tab,hkname)
  endif
  if xte eq 1 then begin
	tableget,fnam(k),valtemp,hkname,1,xte=xte
  endif
 endif
 if hkname eq 'BIT_RATE' then begin 
   bittemp=tbget(h,tab,hkname)
   nvl=n_elements(bittemp)
   valtemp=fltarr(nvl) 
   locl = where((bittemp eq 'L'),nlocl)
   locm = where((bittemp eq 'M'),nlocm)
   loch = where((bittemp eq 'H'),nloch)
   if nlocl gt 0 then valtemp(locl)=0.0
   if nlocm gt 0 then valtemp(locm)=1.0
   if nloch gt 0 then valtemp(loch)=2.0
 endif
 if xte eq 0 then begin
 tmptim=tbget(h,tab,'TIME')
 fovtmp=tbget(h,tab,'FOV')
 endif
 if xte eq 1 then begin
  tableget,fnam(k),tmptim,'time',1,xte=xte
  fovtmp=tmptim
 endif
 if k eq 0 then begin
	hkvals=valtemp & time=tmptim & fov=fovtmp
 endif
 if k gt 0 then begin
	hkvals=[hkvals,valtemp]
	time=[time,tmptim]
	fov=[fov,fovtmp]
 endif
endfor
print,' MIN and MAX values of ',hkname
print,minmax(hkvals)
window,0,xsize=800,ysize=600
plot,time,hkvals
read,'How many INCLUSIVE selection windows ?',nw
windw=fltarr(100,2)
for k=0l,nw-1 do begin
 print,'WINDOW ',k+1
 read,'Enter Lower Value Of Window ',wlo
 read,'Enter Upper Value Of Window ',whi
  windw(k,0)=wlo & windw(k,1)=whi
  flg=-1
; print,' Enter Flag value for this parameter: -1 don t care about FOV'
; read,' <-1 = off; 0 = sky; 1=Dark Earth; 2=Bright Earth ',flg
endfor
hkt.nwin=nw
hkt.wind=windw
hkt.name=hkname
hkt.flag=flg
if n_elements(hk) gt 0 then hk=[hk,hkt] else hk=hkt
hksiz=(size(hk))(1)
print,' Number of HK parameters in structure ',hksiz
ans=' '
read,'Do another HK paramter? (y/n) ',ans
if ans eq 'y' then goto, again
;now make the GTI
cont1: ntime=(size(time))(1) & tmask=lonarr(ntime)
;for each HK param and each window
nw=hk.nwin
winsel=hk.wind
hflag=hk.flag
hname=hk.name
for j=0l,hksiz-1 do begin
  if hflag(j) ge -1 then begin
    for k=0l,hkinfo.nfiles-1 do begin
	tab=readfits(fnam(k),hin,ext=1)
	hkcorhead,hin,h
	if hname(j) ne 'BIT_RATE' then begin
 	  if xte eq 0 then begin
		tparam=tbget(h,tab,hname(j))
 	  endif
	  if xte eq 1 then begin
	    tableget,fnam(k),tparam,hname(j),1,xte=xte
	  endif
	endif
	if hname(j) eq 'BIT_RATE' then begin
	 bittemp=tbget(h,tab,hname(j))
	 locl=where((bittemp eq 'L'),nlocl)
	 locm=where((bittemp eq 'M'),nlocm)
	 loch=where((bittemp eq 'H'),nloch)
	 nvl=n_elements(bittemp) & tparam=fltarr(nvl)
	 if nlocl gt 0 then tparam(locl)=0.0
	 if nlocm gt 0 then tparam(locm)=1.0
	 if nloch gt 0 then tparam(loch)=2.0
        endif
	if k eq 0 then param=tparam else param=[param,tparam]
     endfor
     for i=0l,nw(j)-1 do begin
       if hflag(j) ge 0 then begin
       print,(size(where(fov eq hflag(j))))
       wp=where(((param ge winsel(i,0,j)) and (param le winsel(i,1,j))$
       and (fov eq hflag(j))),wpc)
       endif
       if hflag(j) eq -1 then begin
       wp=where((param ge winsel(i,0,j) and param le winsel(i,1,j)),wpc)
       endif
;       print,' fov ',minmax(fov),(size(fov))(1)
       print,'windows ',winsel(i,0,j),winsel(i,1,j)
       print,'hflag and wpc ',hflag(j),wpc
       if wpc gt 0 then tmask(wp) = tmask(wp)+1l
     endfor
   endif
endfor
whk=where((hflag ge -1),nactive)
if nactive lt 0 then begin
 print,' No HK parameters are active for selection: returning'
endif
;now find out where the non-zero points in tmask begin and end
;these are the GTIs
nactive=long(nactive) & print,'Active HK params = ',nactive
ngti=0l
plot,time,tmask
tbeg=dblarr(1) & tend=tbeg & ityp=5
openw,1,hkinfo.gtifil+'.gti'
if tmask(0) eq nactive then tbeg = time(0)
for j=0l,ntime-2 do begin
  if tmask(j) lt nactive and tmask(j+1) eq nactive then tbeg=time(j+1)
  if tmask(j) eq nactive and tmask(j+1) lt nactive then begin
   tend=time(j)
   print,tbeg,tend
   printf,1,format='(4(F15.3,2X),I2)',tbeg,tend,tbeg-time(0),$
tend-time(0),ityp
    ngti=ngti+1l 
  endif
endfor
print,' Number of HK GTI intervals: ',ngti
close,1
return
end
 
