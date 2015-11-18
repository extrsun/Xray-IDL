pro pha2pi,plist,xl,yl,posdet,gis,dir=dir,gdir=gdir,tcor=tcor
if n_params(0) eq 0 then begin
 print,'PHA2PI,plist,xlin,ylin,posdet,gis,dir=dir,gdir=gdir,/tcor'
 print,'Coverts GIS PHA to PI in photon list structure plist '
 print,' Prompts you for the directory containing the calibration files '
 print,' Uses gain history file dir specified by the user [gdir ]'
 print,' POSDET = 0 for FLF and 1 for POW2: GIS = 2 or 3'
 print,' Uses files gis2_telescope_def_cu_flf.fits etc '
 print,'TCOR >0 applies temporal gain correction'
 retall
end
;T. Yaqoob 1993->
;put in temporal gain correction March 1997
cal_pi=500.0
if n_elements(dir) eq 0 then begin
 dir=' '
 read,' Enter name of directory with CAL files[include all slashes] ',dir
endif
if n_elements(gdir) eq 0 then begin
 gdir=' '
 read,' Enter name of directory with gain file[include all slashes]',gdir
endif
;if n_elements(gfile) eq 0 then begin
; gfile=' '
; read,' Enter name of name of gain history file itself',gfile
;endif
;read gain history file
;gainfile=dir+'gainhist_v1.fits'
;look for the gain history files
gfiles=findfile(gdir+'*.ghf',count=count)
 print,'Found ',count,' Gain history files '
 for k=0l,count-1 do print,gfiles(k)
 nhist=count
for gk=0l,nhist-1l do begin
 gainfile=gfiles(gk)
print,' ** USING GIS GAIN HISTORY FILE ',gfiles(gk)
rdgisgain,gainfile,tmpgain
 if gk eq 0l then gain=tmpgain else gain=[gain,tmpgain]
endfor
; check that the gain history file covers the time period of the
;observation
tobsmin=min(plist.time) & tobsmax=max(plist.time)
print,' Observation spans the following times: '
print,tobsmin,tobsmax, ' (secs)'
tbegmin=min(gain.tbeg) & tendmax=max(gain.tend)
print,' Gain History File spans the following times: '
print,tbegmin,tendmax,' (secs)'
tbeg=gain.tbeg & tend=gain.tend & ntim=(size(tbeg))(1)
print,' Number of time bins in GIS history file = ',ntim
if gis eq 2 then fecol=gain.s2calph
if gis eq 3 then fecol=gain.s3calph
tfirst=0l & tlast=ntim-1
for j=0l,ntim-2 do begin
if (tobsmin ge tbeg(j)) and (tobsmin lt tbeg(j+1)) then tfirst=j
if (tobsmax gt tend(j)) and (tobsmax le tend(j+1)) then tlast=j+1
endfor
print,'TFIRST: ',tfirst,tbeg(tfirst),tend(tfirst)
print,'TLAST: ',tlast,tbeg(tlast),tend(tlast)
if tobsmax lt tbegmin then begin
 print,' Observation ends before gain history data begins : Cannot do anything '
 topt = 1
endif
if tobsmin gt tendmax then begin
 print,' Observation starts after end of gain history file : Cannot do anything'
 topt = 2
endif
if (tobsmin ge tbegmin) and (tobsmax gt tendmax) then begin
 print,' **CAUTION** Gain history data ends before end of observation: '
 print,' Assuming the last values are valid for rest of observation '
 topt = 3 & tlast=ntim-1
endif
if (tobsmin lt tbegmin) and (tobsmax le tendmax) then begin
 print,' **CAUTION** Observation starts before Gain history file starts '
 print,' Assuming the first entries in the Gain history file are valid '
 print,' for first part of the observation '
 topt = 4 
endif
if (tobsmin lt tbegmin) and (tobsmax gt tendmax) then begin
 print,'**CAUTION** Observation start and end times occur outside Gain history '
 print,' file time limits: assuming 1st and last values in Gain history file '
 print,' are valid for those periods not covered '
 topt = 5
endif
if (tobsmin ge tbegmin) and (tobsmax le tendmax) then begin 
 print,' Valid Gain history data exists for entire observation '
 topt = 6
endif
if topt le 2 then goto, fin
;read gain map file
nph=(size(plist))(1) & print,' Photon list size = ',nph
time=plist.time & x=plist.x & y=plist.y
gtgismap,posdet,gis,dir,gmap,cal,detx,dety,deltax,deltay,coeff
;linearize x and y coords
gislinxy,coeff,cal,detx,dety,deltax,deltay,x,y,xl,yl,xlp,ylp
;agree with the new version 1 type of gain history file
;correction for version 1 gain history
v1corr=472.9188/511.4022/0.98488
;cal.ratio=cal.ratio*472.9188/500
;print,' CALRATIO = ',cal.ratio
print,' CAL_X0 & CAL_Y0 ',cal.x0,cal.y0
print,' CAL_NORM ',cal.norm
print,' CAL_ADU ',cal.adu
;what is the effect of linearizing cal.x0 and cal.y0
gislinxy,coeff,cal,detx,dety,deltax,deltay,cal.x0,cal.y0,u,v,x0l,y0l
print,'linearized x0 and y0: ',x0l,y0l
;picol=fltarr(nph)  norm=cal_pi*gmap(fix(cal.x0-1),fix(cal.y0-1))/cal.ratio
print,'calnorm ',size(calnorm)
newx0=x0l(0) & newy0=y0l(0) & 
picol=fltarr(nph) & calnorm=cal_pi*gmap(newx0,newy0)/cal.ratio
;setup interpolated calpeak function with 1000 bins
nbins=1000l & tstart=tbeg(tfirst)  & delt=(tend(tlast)-tstart)/1000.
tlow=tstart+delt*findgen(nbins)
thigh=tlow+delt & tcen=(thigh+tlow)/2.
cpeak=fltarr(nbins)
for i=tfirst,tlast do begin
  t1=tbeg(i) & t2=tend(i)
  i1=fix((t1-tstart)/delt) & i1=i1 < (nbins-1)
  i2=fix((t2-tstart)/delt) & i2=i2 < (nbins-1)
; print,t1,t2,i1,i2
  for k=i1,i2 do cpeak(k)=fecol(i)
  if (i lt tlast)  then begin
	if tend(i) lt tbeg(i+1) then begin
	t3=tbeg(i+1) & i3=fix((t3-tstart)/delt) & i3 = i3 < (nbins-1)
;	print,t2,t3,i2,i3
	for k=i2,i3 do begin
cpeak(k)=((tcen(k)-tend(i))*(fecol(i+1)-fecol(i))/(tbeg(i+1)-tend(i)))+fecol(i)
	endfor
  endif
 endif
endfor
;cpeak=smooth(cpeak,100)
ymin=0.8*min(cpeak) & ymax=1.2*max(cpeak)
;window,0
plot,yrange=[ymin,ymax],/xst,/yst,tbeg(tfirst:tlast),fecol(tfirst:tlast),psym=1 
oplot,tend(tfirst:tlast),fecol(tfirst:tlast),psym=1
oplot,tcen,cpeak
ru=-0.5+randomu(seed,nph) 
print,'size ru',nph,(size(ru))(1)
pnt=fix((time-tstart)/delt) & pnt=pnt < (nbins-1) & pnt = pnt >0
calpeak=cpeak(pnt)
ggain=gmap(xlp,ylp)
;force pha channels with -ve gain in the map to have pi>1024
;and therefore get rejected.
zgn=where((ggain le 0.),nzgn) 
if nzgn gt 0 then ggain(zgn) = -1.
;print,' minmax(calnorm) =',minmax(calnorm)
print,' minmax(calpeak) = ',minmax(calpeak)
;fudge if version 1 gain history
if (gain.vers)(0) eq 1 then cal.norm=cal.norm/v1corr
picol=(ru+float(plist.pha))*cal.norm*cal.adu/calpeak/ggain
print,' minmax(gmap) = ',minmax(gmap(xlp,ylp)),minmax(ggain)
if nzgn gt 0 then picol(zgn)=10000.
print,'minmax(picol) = ',minmax(picol)
;now apply the temporal gain corrections
 tcoef=fltarr(2,4)
 tcoef(0,0:3)=[-3.945e-15,1.090E-11,2.851E-08,1.004E+00]
 tcoef(1,0:3)=[-3.871E-14,2.974E-10,6.128E-07,9.970E-01]
 if tcor gt 0 then begin
  ig=gis-2
  tmid=total(minmax(plist.time))/2.
rtsq=(plist.detx-128.5)*(plist.detx-128.5)+(plist.dety-128.5)*(plist.dety-128.5)
  tcorfac=(tcoef(ig,0)*rtsq+tcoef(ig,1))*tmid+(tcoef(ig,2)*rtsq+tcoef(ig,3)) 
  picol=picol/tcorfac 
 endif
plist.pi=fix(picol)
plist.pi = plist.pi*(plist.pi ge 1) & plist.pi = plist.pi*(plist.pi le 1024)
fin: return
end
