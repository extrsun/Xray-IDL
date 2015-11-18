pro getarea,geo,expname=expname,inside=inside,wbin,sx0,sy0,bx0,by0,swmap,bwmap
;Author T. Yaqoob - March 1993 ->**
if n_params(0) eq 0 then begin
 print,'getarea,geo,expname=expname,inside=inside,wbin,sx0,sy0,bx0,by0,swmap,bwmap'
 print,'Assign the correct AREA/EXPOSURE scale factors to GEO'
 print,'Assumes the GEO units are DETX and DETY'
 print,'DIVIDE SIS factor by 1280*1280 for OGIP FITS PHA file '
 print,'DIVIDE GIS factor by 256*256 for OGIP FITS PHA file '
 print,'EXPNAME	: name of SIS exposure map file [can use dummy chip map]'
 print,'For the SIS the area is computed for the specified chip in geo'
 print,'taking into account the gaps and works for any selection'
 print,'size. If geo.chp=4 area is the total intercepted by 4 chips. '
 print,'** MAKE SURE YOUR GEO REGION SELECTION IS IN DET COORDS '
 print,'** FOR ANYTHING OTHER THAN THE SIS MAKE SURE YOUR SELECTION '
 print,'** REGION LIES COMPLETELY WITHIN THE DETECTOR FOV '
 print,'+++ Except if you know the SIS region is ALL inside 1 chip '
 print,'+++ set inside>0 for speed'
 print,'----- stuff for WMAP ----'
 print,'WBIN- Input rebin factor for WMAP [recomd 1 for GIS, 8 for SIS]'
 print,'** OUTPUTS ** '
 print,'SX0,SY0 - bottom left hand corner of source WMAP'
 print,'BX0,BY0 - bottom left hand corner of bgd WMAP'
 print,'SWMAP, BWMAP - source and bgd WMAPS'
 retall
end
 print,'** MAKE SURE YOUR GEO REGION SELECTION IS IN DET COORDS '
 print,'** FOR ANYTHING OTHER THAN THE SIS MAKE SURE YOUR SELECTION '
 print,'** REGION LIES COMPLETELY WITHIN THE DETECTOR FOV '
pi=3.14159265359
p=fltarr(10)
if geo.inst le 1 and inside eq 0 then inst='SIS' else inst='GIS'
if inst eq 'SIS' then begin 
 if n_elements(expname) eq 0 then begin
;must input exposure map file for SIS
 expname=' '
 read,'Enter name of SIS exposure map or chip map file ',expname
 endif
 sisim=readfits(expname,h)
 chip=geo.chp
endif
if geo.type ge 1 and geo.type le 3 then begin
;we are dealing with circular regions
;compute area of 1st circle
  if inst eq 'GIS' then begin 
	a1=pi*geo.s1(0) 
	a2=pi*geo.s2(0)
	a3=pi*geo.s2(1)
;set wmaps
   if geo.type le 3 then begin
	sxmin=geo.cen1(0)-geo.d1(0)
	symin=geo.cen1(1)-geo.d1(0)
	sxmax=geo.cen1(0)+geo.d1(0)
	symax=geo.cen1(1)+geo.d1(0)
    if geo.d2(0) gt 0. then begin
	bxmin=geo.cen2(0)-geo.d2(0)
	bymin=geo.cen2(1)-geo.d2(0)
	bxmax=geo.cen2(0)+geo.d2(0)
	bymax=geo.cen2(1)+geo.d2(0)
    endif
   endif
  endif
  if inst eq 'SIS' then begin
   p(0)=geo.cen1(0) & p(1)=geo.cen1(1) & p(2)=geo.d1(0)
   sisarea,1,p,sisim,area,sxmn,symn,sxmx,symx & a1=area(chip)
   p(0)=geo.cen2(0) & p(1)=geo.cen2(1) & p(2)=geo.d2(0)
   sisarea,1,p,sisim,area,xmn1,ymn1,xmx1,ymx1 & a2=area(chip)
   p(0)=geo.cen2(0) & p(1)=geo.cen2(1) & p(2)=geo.d2(1)
   sisarea,1,p,sisim,area,xmn2,ymn2,xmx2,ymx2 & a3=area(chip)
   sxmin=sxmn(chip)
   symin=symn(chip)
   sxmax=sxmx(chip)
   symax=symx(chip)
  endif
  geo.s1(4)=a1
  rs=geo.d1(0)
  if geo.type eq 1 then begin
	geo.s2(4)=a2-a1
	rb=geo.d2(0)
      if inst eq 'SIS' then begin
	bxmin=xmn1(chip) & bymin=ymn1(chip) 
	bxmax=xmx1(chip) & bymax=ymx1(chip)
      endif
  endif 
  if geo.type eq 2 then begin 
	geo.s2(4)=a3-a2
	rb=geo.d2(1)
      if inst eq 'SIS' then begin
	bxmin=xmn2(chip) & bymin=ymn2(chip)
	bxmax=xmx2(chip) & bymax=ymx2(chip)
      endif
  endif
  if geo.type eq 3 then begin 
	geo.s2(4)=a2
	rb=geo.d2(0)
       if inst eq 'SIS' then begin
	bxmin=xmn1(chip) & bymin=ymn1(chip)
	bxmax=xmx1(chip) & bymax=ymx1(chip)
       endif
  endif
endif
    blnkwmap,sxmin,symin,sxmax,symax,wbin,sx0,sy0,sxc,syc,sxdim,sydim,swmap
;replace the -1's with 0's inside selected src region
    for i=0l,sxdim-1l do begin
	for j=0l,sydim-1l do begin
	  rad=sqrt((sxc(i)-geo.cen1(0))^2.+(syc(j)-geo.cen1(1))^2.)
	  if rad le rs then swmap(i,j)=0.
	endfor
    endfor
    if geo.d2(0) gt 0. then begin 
    blnkwmap,bxmin,bymin,bxmax,bymax,wbin,bx0,by0,bxc,byc,bxdim,bydim,bwmap
;replace the -1's with 0's inside selected bgd region
    for i=0l,bxdim-1l do begin
	for j=0l,bydim-1l do begin
	  rad=sqrt((bxc(i)-geo.cen2(0))^2.+(byc(j)-geo.cen2(1))^2.)
 if geo.type eq 1 and rad le rb and rad ge rs then bwmap(i,j)=0.
if geo.type eq 2 and rad le rb and rad ge geo.d2(0) then bwmap(i,j)=0.
 if geo.type eq 3 and rad le rb then bwmap(i,j)=0.
	endfor
    endfor
    endif
if geo.type gt 3 then begin
 print,'Unrecognized GEO TYPE
 return
endif
print,'Exposure weigthed area of SOURCE region ',geo.s1(4)
print,'Exposure weigthed area of BKGD   region ',geo.s2(4)
if geo.s1(4) gt 0.0 then geo.bscl=geo.s2(4)/geo.s1(4)
print,'GEO.BSCL: BGD/SRC area = ',geo.bscl
if geo.bscl gt 0.0 then print,'1./geo.bscl = ',1./geo.bscl
return
end
 
