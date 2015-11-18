pro qlasca,srcho,srchi,srche,inst,rootname=rootname,cmpcen=cmpcen,cens=cens,$
 srcreg=srcreg,bgdreg=bgdreg,dir=dir,arf=arf,rmf=rmf,$
 sispi=sispi,tbin=tbin,mkflist=mkflist,tmfile=tmfile,sisfile=sisfile,grdstr=grdstr,tcor=tcor
if n_params(0) eq 0 then begin
 print,'Quick hybrid IDL+XSELECT ASCA analysis script to analyse'
 print,'screened data'
print,'qlasca,srcho,srchi,srche,inst,rootname=rootname,cmpcen=cmpcen,cens=cens,srcreg=srcreg,bgdreg=bgdreg,dir=dir,arf=arf,rmf=rmf,sispi=sispi,tbin=tbin,mkflist=mkflist,tmfile=tmfile,sisfile=sisfile,grdstr=grdstr,tcor=tcor'
 print,'SRCHO	- 1st substring to search event files (e.g. seq no.)'
 print,'SRCHI	- Instrument key-string for searching event files '
 print,'SRCHE 	- extension for searching event files '
 print,'INST	- 0,1,2,3 - instrument '
 print,'ROOTNAME - output files will have names rootname_srcho_srchi '
 print,'CMPCEN	- compute centroid for extraction; if omitted use the
 print,'	- following specs '
 print,'CENS	- value of smoothing parameter for computing centroid'
 print,'SRCREG  - array(5) containing manual overide parameters for '
 print,'	- annular region extraction for source+bgd data '
 print,'	- Xc,Yc,r1,r2,r2 [if bgdreg exists only r1 is used]'
 print,'BGDREG	- If this exists, bgd is preferntially extracted from '
 print,'	- this separate circular region: array(3): Xc,Yc,r1'
 print,'	- Note that in both cases the centroid is in DETX DETY'
 print,'	- and RADII are in ARCMIN '
 print,'DIR	- directory (incl last slash) with input event files '
 print,'ARF	- if set make ARF file '
 print,'RMF	- if set run sisrmg '
 print,'SISPI	- if set run SISPI '
 print,'TBIN	- lightcurve bin size in seconds
 print,'MKFLIST - Name of ascii file containing HK parameter selection'
 print,'	- criteria. First line is a numerical value equal'
 print,'	  to the initial crude hot pixel cut-off '
 print,'          If mkflist is not specified then the default is '
 print,'	  equivalent to: '
 print,'	  200 '
 print,'	  saa.eq.0 '
 print,'	  elv.gt.5 '
 print,'	  cor.gt.7.0'
 print,'TMFILE  - Optional time intervals (ascii) file to filter on '
 print,'	  Useful for second round '
 print,'SISFILE	- full path to SIS pha->pi file '
 print,'GRDSTR	- grade string for sisrmg and grade selection - default '
 print,'	  is grade.ne.1.and.grade.le.4 '
 print,'TCOR	- Apply GIS temporal gain correction if >0 (default 1)'
 retall
end
gisbcf='/FTP/caldb/data/asca/gis/bcf/'
;first convert region radii to DET coords
plate_scale=0.98220003
srcregd=srcreg 
if inst le 1 then detc=0.027 else detc=0.25 
 srcregd(2:4)=srcreg(2:4)/detc/plate_scale
; print,'SRCREGD(3) =',srcregd(3)
 if n_elements(bgdreg) gt 0 then begin
  bgdregd=bgdreg
  bgdregd(2:4)=bgdreg(2:4)/detc/plate_scale
 endif 
if n_elements(rootname) eq 0 then begin
 rootname=srcho+'_'+'srchi'+'_'
endif
root=rootname+'_'+srcho+'_'+srchi
;define grade selection
 gradeflt=''
 ng=strlen(grdstr)
 for kl=0l,ng-1l do begin
  gradeflt=gradeflt+'grade.eq.'+strmid(grdstr,kl,1)
  if kl lt ng-1 then gradeflt=gradeflt+'.or.'
 endfor
 gradeflt=strtrim(gradeflt,2)
if n_elements(dir) eq 0 then begin
 dir=''
endif
if n_elements(sispi) eq 0 then sispi=0
if n_elements(rmf) eq 0 then rmf=0
;mkf stuff
 mkfexpr=''
if n_elements(mkflist) eq 0 then begin
 mkfexpr='saa.eq.0.0.and.elv.gt.5.0.and.cor.gt.7.0'
 hpcut=200
endif else begin
 openr,9,mkflist(inst)
 mkfexpr=''
 isel=0l
 while not eof(9) do begin
  dummy=' '
  if isel eq 0 then readf,9,hpcut else readf,9,dummy
  if isel eq 1 then mkfexpr=strtrim(dummy,2) 
  if isel gt 1 then mkfexpr=mkfexpr+'.and.'+strtrim(dummy,2)
  if isel gt 0 then print,'Selection ',isel,' is ',mkfexpr
  isel=isel+1l
 endwhile
 close,9
endelse
;find the event files
flist=findfile(dir+"*"+srcho+"*"+srchi+"*"+"."+srche, count=count)
print,'Found '+strn(count)+" "+srchi+" "+srche+" "+" files "
nfiles=count
det_name=['sis0','sis1','gis2','gis3']
;now write a little xselect script to read in the files and make
;an image and save a single events file
xname1=root+'_xsel1.xco'
openw,1,xname1
printf,1,'xsel'
printf,1,'set instr ' + det_name(inst)
printf,1,'set xybinsize 1. '
 for j=0l,nfiles-1l do begin
;need to separate the actual filename from the path
  tmpfile=str_sep(flist(j),'/')
  nprts=(size(tmpfile))(1)
printf,1,'read eve '+strtrim(tmpfile(nprts-1l),2)+' '+strtrim(dir,2)
 endfor
printf,1,'extr eve '
if inst le 1 then begin
 printf,1,'sisclean saoimage=no sis_plot=no clean=1 bkg_thr=' + $
strn(hpcut) + ' clean_phalow=0 clean_phahi=4095' 
 printf,1,'extr eve'
 hpcut_name = root + '_hc' + strn(hpcut) + '.evt'
 printf,1,'save eve ' + hpcut_name + ' use_events=yes clobber=yes'
 printf,1,'clear all'
 printf,1,'yes'
 printf,1,'set instr ' + det_name(inst)
 printf,1,'read eve ' + hpcut_name + ' .'
 printf,1,'select eve '+gradeflt
 if strlen(mkfexpr) gt 0 then printf,1,'select mkf '+mkfexpr+' mkf_dir=../aux/'
 printf,1,'sisclean saoimage=no sis_plot=no clean=2 cellsize=5'+$
' log_prob=-5.24  bkg_thr=2 clean_phalow=0 clean_phahi=4095'
endif
if n_elements(tmfile) gt 0 then begin
 printf,1,'filter time file '+strtrim(tmfile,2)
endif
;now run gisclean and extract the gis ring and cal source
if inst ge 2 then begin
 rmvname=det_name(inst)+'_nocalib.rmv'
 printf,1,'filter region ',strtrim(rmvname,2)
 printf,1,'gisclean rti_table_user=/local/home/rohrshni/yaqoob/giscal/rti_gis_1024_040693.fits'
 openw,7,rmvname
 printf,7,'# Removal of Cal source and ring for ',det_name(inst)
 if inst eq 2 then begin
  printf,7,'CIRCLE(124,132,81.00)'
  printf,7,'-CIRCLE(166.00,221.0,24.00)'
 endif else begin
  printf,7,'CIRCLE(133.0,119.0,73.00)'
  printf,7,'-CIRCLE(205.0,99.0,8.0)'
 endelse
 close,7
 if strlen(mkfexpr) gt 0 then printf,1,'select mkf '+mkfexpr+' mkf_dir=../aux/'
endif  
printf,1,'extr eve'
printf,1,'save eve '+root+'_all.evt use_events=yes clobber=yes'
printf,1,'set xybinsize 1.'
printf,1,'extr image '
printf,1,'save image '+root+'_all.img clobber=yes'
printf,1,'exit '
printf,1,'no '
close,1
;actual xselect part
xselstr='xselect @'+strtrim(xname1,2)
spawn,xselstr,/sh
;now convert the PI values in the GIS events files
;run pha2pi on gis events files
if tcor gt 0 and inst ge 2 then begin
 posdet=0
 gis=inst
 gdir='../aux/'
 gvfile=root+'_all.evt'
 for gk=0l,nfiles-1l do begin
  print,'Running PHA2PI on ',gvfile
  qreadasca,gvfile,glist,hg,gtig,tabg
  PHA2PI,glist,xlin,ylin,posdet,gis,dir=gisbcf,gdir=gdir,tcor=tcor
  updategiseve,glist.pi,evename=gvfile
 endfor
endif
;now read the image into IDL and find the centroid and write the 
;appropriate xselect region file
if n_elements(cmpcen) gt 0 then begin
  imfile=root+'_all.img'
 image=readfits(imfile,h)
 getcntrd,image,cens,xcen,ycen
; centroid,smooth(image,cens),xcen,ycen,xy_peak=xy_peak,/peak_locate
 print,'X Y of centroid = ',xcen,ycen
 getcntrd,image,cens+1.,xcen,ycen
; centroid,smooth(image,cens+1.),xcen,ycen,xy_peak=xy_peak,/peak_locate
 print,'X Y of centroid = ',xcen,ycen
; centroid,smooth(image,cens+2.),xcen,ycen,/peak_locate
; print,'X Y of centroid = ',xcen,ycen
 print,'Final smoothing factor ',cens+1.
;catch weak sources
  getcntrd,image,0.,x1,y1,xy_peak=xy_peak
  x2=xcen & y2=ycen
 if abs((x2-xy_peak(0))/xy_peak(0)) gt 0.20 then begin
  print,'** USING SOURCE PEAK **' 
  xcen=xy_peak(0) & ycen=xy_peak(1)
 endif
 if abs((y2-xy_peak(0))/xy_peak(0)) gt 0.20 then begin
  print,'** USING SOURCE PEAK ** '
  xcen=xy_peak(0) & ycen=xy_peak(1)
 endif
endif else begin
 xcen=srcreg(0) & ycen=srcreg(1)
endelse 
;for SIS must exclude non-source chip regions
; must make a photon list first - also used for lightcurve
evname=root+'_all.evt'
 qreadasca,evname,plist,h,gti,tab
if inst le 1 then begin
 srcrad=srcreg(2) & bgdrad=srcreg(3) & sis=inst & bin=1.
 srcregfile=root+'_src_'+strmid(strtrim(string(srcrad),2),0,4)+'min.reg' 
 bgdregfile=root+'_bgd_'+strmid(strtrim(string(bgdrad),2),0,4)+'min.reg' 
 print,'JUST BEFORE REG ROUTINE srcrad and bgdrad ',srcrad,bgdrad
 qmake_sis_reg,plist,xcen,ycen,root,bin,srcrad,bgdrad,sis=sis,srcregfile=srcregfile,bgdregfile=bgdregfile
endif else begin
;for GIS regions must lie entirely within GIS FOV.
;now also have the choice of chhosing a separate bgd region
 srcregfile=root+'_src.reg'
 bgdregfile=root+'_bgd.reg'
  openw,1,srcregfile
  openw,2,bgdregfile
  printf,1,'CIRCLE(',xcen,',',ycen,',',srcregd(2),')'
  close,1
 if n_elements(bgdreg) eq 0 then begin
  printf,2,'CIRCLE(',xcen,',',ycen,',',srcregd(4),')'
  printf,2,'-CIRCLE(',xcen,',',ycen,',',srcregd(3),')'
 endif else begin
  printf,2,'CIRCLE(',bgdreg(0),',',bgdreg(1),',',bgdregd(2),')'
 endelse 
 close,2
endelse
;run sispi
if sispi gt 0 and inst le 1 then begin
 if n_elements(sisfile) eq 0 then sisfile=' FTOOLS '
 print,'SISPI: using cal file ',sisfile
 sispi_str = "sispi " + " datafile = "+evname+" calfile = "+sisfile
 spawn,sispi_str,/sh
endif
;now make spectra, arf and rmf files
xybin=1. & rebin = 20 
extr_asca_spec,evname,inst,srcregfile,bgdregfile, xybin=xybin, /arf,$
rebin=rebin, loglun=loglun, rmf=rmf ,sisfile=sisfile,grdstr=grdstr
;make lightcurve
print,'Finished spectra now making lightcurve '
lcname=root+'_lc.qdp'
qreadasca,root+'_src.evt',slist,hs,sgti,stab
qreadasca,root+'_bgd.evt',blist,hb,bgti,btab
;get background scaling
hsp=headfits(root+'_src.pha',ext=1)
hbp=headfits(root+'_bgd.pha',ext=1)
bfac=sxpar(hbp,'BACKSCAL')
sfac=sxpar(hsp,'BACKSCAL')
bgdfac=sfac/bfac
;multiply the background count rate by the above factor
print,'Background scaling factor = ',bgdfac
openw,1,lcname
printf,1,'READ SERR 1 2'
printf,1,'SKIP SINGLE'
printf,1,'MARK 17 on  1..3'
printf,1,'LA X Time (s)'
printf,1,'LA Y ct/s'
 MKCURVE,slist.time,sgti,tbin,nbins,stcen,stbounds,steff,sctspsec,sctserr  
 refbnds=[min(slist.time),max(slist.time)]
 MKCURVE,blist.time,bgti,tbin,nbins,btcen,btbounds,bteff,bctspsec,bctserr,refbnds=refbnds 
 nbcts=bgdfac*bctspsec & nbctserr=bgdfac*bctserr
 critexp=0.6
 sexpf=2.*steff/tbin
 bexpf=2.*bteff/tbin
 for i=0l,nbins-1l do begin
  if sexpf(i) gt critexp then printf,1,stcen(i)-stcen(0),steff(i),sctspsec(i),sctserr(i)
 endfor
 printf,1,'NO NO NO NO'
 for i=0l,nbins-1l do begin
  if bexpf(i) gt critexp then $
    printf,1,btcen(i)-btcen(0),bteff(i),nbcts(i),nbctserr(i)
 endfor
 printf,1,'NO NO NO NO'
 subcts=sctspsec-nbcts
 subctserr=sctserr*sctserr+nbctserr*nbctserr
 for i=0l,nbins-1l do begin
  if subctserr(i) gt 0. then subctserr(i)=sqrt(subctserr(i)) else $
subctserr(i)=0.0
  if sexpf(i) gt critexp then printf,1,stcen(i)-stcen(0),steff(i),$
subcts(i),subctserr(i) 
 endfor
close,1
return
end 
