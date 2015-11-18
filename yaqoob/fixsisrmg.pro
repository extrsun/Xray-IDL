pro fixsisrmg,specname,evname=evname,sisfile=sisfile,sccd=sccd,batch=batch,table=table,oldevt=oldevt,oldlvdl=oldlvdl,kname1=kname1,kname2=kname2
if n_params(0) eq 0 then begin
 print,'fixsisrmg,specname,evname=evname,sisfile=sisfile,sccd=sccd,batch=batch,table=table,oldevt=oldevt,oldlvdl=oldlvdl'
 print,'Temporary fix for SISRMG bug in FTOOLS v3.6'
 print,'SPECNAME	- Input spectral file'
 print,'EVNAME 		- Optional events file - used only to '
 print,'		- to get mean RAWX RAWY. If not specified'
 print,'		uses RAWX=270. RAWY=260. (SIS0) and '
 print,'					 (SIS1).'
 print,'SISFILE		- ph2pi file [ sisph2pi_110397.fits ]'
 print,'		specify full path '
 print,'default is /FTP/caldb/data/asca/sis/bcf/sisph2pi_110397.fits'
 print,'SCCD		- In the event of 2 or 4 CCD mode specify '
 print,'		  which chip to use'
 print,'BATCH		- use sccd=1 (S0) and sccd=3 (S1) if batchmode'
 print,'TABLE		- table>0 produce a table for 60 months'
 print,' ** OUTPUTS ** '
 print,'OLDEVT		- old event threshold '
 print,'OLDLVDL		- old level discriminator' 
 retall
end
hs=headfits(specname)    
inst=sxpar(hs,'INSTRUME')
if strmid(inst,0,4) eq 'SIS0' then sis=0 else sis=1
if sis eq 0 then chipon='S0CCDPOW' else chipon='S1CCDPOW'
ccd=sxpar(hs,chipon)
ccdid=intarr(4)
cname=strarr(2,4) & tname=cname
lname=strarr(2,4)
for k=0,3 do begin
 ccdid(k)=strn(strmid(ccd,k,1))
 cname(0,k)='s0c'+strtrim(string(k),2)
 cname(1,k)='s1c'+strtrim(string(k),2)
 tname(0,k)='s0_evtr'+strtrim(string(k),2)
 tname(1,k)='s1_evtr'+strtrim(string(k),2)
 lname(0,k)='s0_lvdl'+strtrim(string(k),2)
 lname(1,k)='s1_lvdl'+strtrim(string(k),2)
endfor
if total(ccdid) gt 1 and n_elements(sccd) eq 0 then begin
 print,'** SISRMG FUDGE ONLY ALLOWED FOR 1 CCD AT A TIME  ** '
 if n_elements(batch) gt 0 then begin
  print,' ******** WARNING !!! *********'
  print,' YOUR EVENTS FILE HAS COUNTS FROM MORE THAN ONE CHIP'
  print,' AND YOU DID NOT SPECIFY WHICH CHIP TO DO THE CORRECTION'
  print,' FOR SO THE RESULT MAYBE WRONG BECAUSE WE ARE ASSUMING '
  print,' S0C1 OR S1C3'
  if sis eq 0 then sccd=1
  if sis eq 1 then sccd=3
 endif else begin 
  read,'Specify which chip to use (0-3) ',sccd
 endelse
 wc=sccd
endif else begin
 wc=where(ccdid eq 1) & wc=wc(0)
 sccd=wc
endelse
kname1=strtrim(tname(sis,wc),2)
kname2=strtrim(lname(sis,wc),2)
evtr=sxpar(hs,kname1)
lvdl=sxpar(hs,kname2)
oldevt=evtr & oldlvdl=lvdl
kname1=tname(sis,wc)
tstart=sxpar(hs,'tstart')
tstop=sxpar(hs,'tstop')
t0=(tstart+tstop)/2.
print,'Uncorrected event threshold = ',evtr
print,'Uncorrected Lower level discriminator = ',lvdl
if n_elements(evname) eq 0 then begin
 if sis eq 0 then begin
 	rawx=270. & rawy=260.
 endif else begin
	rawx=310. & rawy=250.
 endelse
endif else  begin
 tab=readfits(evname,h,ext=1)
 rx=tbget(h,tab,'rawx')
 ry=tbget(h,tab,'rawy')
 ci=tbget(h,tab,'ccdid')
 ww=where((ci eq sccd),nn)
 if nn gt 0 then begin
  rxx=rx(ww) & ryy=ry(ww)
 endif
 rawx=total(rxx)/(size(rxx))(1)
 rawy=total(ryy)/(size(ryy))(1)
; qreadasca,evname,plist,h,gti,tab
; rawx=total(plist.x)/(size(plist.x))(1)
; rawy=total(plist.y)/(size(plist.y))(1)
endelse
if n_elements(sisfile) eq 0 then sisfile='/FTP/caldb/data/asca/sis/bcf/sisph2pi_110397.fits' 
tab=readfits(sisfile,h,ext=1)
time=tbget(h,tab,'time')
nt=(size(time))(1)
coeff=tbget(h,tab,cname(sis,wc))
ytab1=reform(coeff(0,*))
ytab2=reform(coeff(1,*))
ytab3=reform(coeff(2,*))
linterp,time,ytab1,t0,yout1
linterp,time,ytab2,t0,yout2
linterp,time,ytab3,t0,yout3
print,rawx,rawy
print,yout1,yout2,yout3
yout=yout1+yout2*rawx+yout3*rawy
print,yout
newevtr=fix(yout*evtr/3.65)
newlvdl=fix(yout*lvdl/3.65)
fstr='fparkey value = '+strtrim(string(newevtr),2)+' fitsfile = '+specname+'+0 keyword = '+strtrim(tname(sis,wc),2)
print,fstr
spawn,fstr,/sh
fstr='fparkey value = '+strtrim(string(newlvdl),2)+' fitsfile = '+specname+'+0 keyword = '+strtrim(lname(sis,wc),2)
print,fstr
spawn,fstr,/sh
if n_elements(table) gt 0 then begin
nmonths=80l
openw,3,'sis'+strtrim(string(sis),2)+'_fix.tab'
;print out a table
 tm=findgen(nmonths)*30.*86400.
 for j=0l,nmonths-1l do begin
  t0=tm(j)
  linterp,time,ytab1,t0,yout1
  linterp,time,ytab2,t0,yout2
  linterp,time,ytab3,t0,yout3
  yout=yout1+yout2*rawx+yout3*rawy
  factor=yout/3.65
 GET_ASCA_DATE,t0,mo,day,year,hr,min,sec,cdate,ctime
  printf,3,strtrim(cdate,2),' ',factor 
 endfor
 close,3
endif
return
end
