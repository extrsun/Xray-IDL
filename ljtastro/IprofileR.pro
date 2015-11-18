; $Id: Iprofile.pro,v 1.0 2009/01/06
;
; Copyright (c), Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.
;===============================================================

pro IprofileR, filepath=filepath,filename=filename,type=type,outputpath=outputpath,outfile=outfile,ctonth=ctonth,stonth=stonth,binnumber=binnumber, pa=pa,pixelsize=pixelsize,block=block,fb=fb,efb=efb,ellipticity=ellipticity,radiusmin=radiusmin,radiusmax=radiusmax,wcut=wcut,anglemin=anglemin,anglemax=anglemax

;===============================================================
;+
; NAME:
;       Iprofile
; PURPOSE:
;       calculate the radial profile.
; CALLING SEQUENCE:
;  	IprofileR,filepath=filepath,filename=filename,type=type,outputpath=outputpath,outfile=outfile,ctonth=ctonth,stonth=stonth
;	,binnumber=binnumber,pa=pa,pixelsize=pixelsize,block=block,fb=fb,efb=efb,ellipticity=ellipticity,radiusmin=radiusmin,radiusmax=radiusmax 
;	,wcut=wcut,cut=cut
; INPUT PARAMETERS:
;	filename: data file names, this is an array contains counts file, background file and exposure file, if type='Chandra'. It contains only the
;	intensity file if type='others', note that here it is still an array.
;
;	pa: position angle of the positive x-axis (major axis), clockwise.
;	pixelsize: pixel size of the image in units of arcmin.
; OPTIONAL INPUT PARAMETERS:
;	anglemax: max angle of the calculating range. The default value is !dpi (180 degree).
;	anglemin: min angle of the calculating range. The default value is -180 degree. Calculate from the positive 
;		  direction of X-axis. Always in degree!!!
;	filepath: data file path, if not given, using the current path.
;	type: 'Chandra' or 'others', the default value is 'Chandra'.
;	outputpath: output file path, it is only useful when outfile is given.
;	outfile: output file name, if not set, only display the profile without any file output.
;	ctonth: counts to noise ratio, the default value is 0.
;	stonth: signal to noise ratio, the default value is 0.
;	binnumber: initial bin number before calculating the bin of the profile, the default value is 1.
;	block: bin size of the image, used to get the true pixel size. The default value is 1.
;	fb: sky background level in unit of count/s/arcmin^2 for Chandra data. The default value is 0..
;	efb: error of background level. The default value is 0..
;	ellipticity: ellipticity of the elliptical annulus (major_x/major_y). The default value is 1.0.
;	radiusmin: min radius of the calculating range. The default value is 0..
;	radiusmax: max radius of the calculating range. The default value is 1..
;	wcut: disk height to be masked, |z|>wcut. The default value is 0..
; EXAMPLE:
;	instr='aciss'
;	cenvset,instr
;	filepath='/home/ljt/ljt/data/AboutAGN/sample1/NGC0891/Chandra/profiles/'
;	filename=['c_s_diffuse.fits','b_s_diffuse.fits','t_s_diffuse.fits']
;	type='Chandra'
;	stonth=8
;	outputpath='/home/ljt/ljt/data/AboutAGN/sample1/NGC0891/Chandra/profiles/'
;	outfile='0.5-1.5keV_e0.5rmin0rmax3wc0.05_SNR8'
;	pa=90.-22.
;	block=2
;	pixelsize=!size_pixel*block/60. ;in units of arcmin
;	ellipticity=0.5
;	anglemin=-180.
;	anglemax=180.
;	radiusmin=0.5
;	radiusmax=4.
;	wcut=0.5
;	IprofileR,filepath=filepath,filename=filename,type=type,pa=pa,pixelsize=pixelsize,ellipticity=ellipticity,radiusmin=radiusmin, 	radiusmax=radiusmax,wcut=wcut,stonth=stonth,block=block;,outputpath=outputpath,outfile=outfile
; NOTE:
;	Note that when radiusmin should not be smaller than wcut.


;===============================================================

if keyword_set(filepath) eq 0 then begin
	filepath=''
endif
if keyword_set(type) eq 0 then begin
	type='Chandra'
endif
if keyword_set(outputpath) eq 0 then begin
	outputpath=''
endif
if keyword_set(ctonth) eq 0 then begin
	ctonth=0
endif
if keyword_set(stonth) eq 0 then begin
	stonth=0
endif
if keyword_set(binnumber) eq 0 then begin
	binnumber=1
endif
if keyword_set(block) eq 0 then begin
	block=1
endif
if keyword_set(fb) eq 0 then begin
	fb=0.
endif
if keyword_set(efb) eq 0 then begin
	efb=0.
endif
if keyword_set(ellipticity) eq 0 then begin
	ellipticity=1.0
endif
if keyword_set(radiusmin) eq 0 then begin
	radiusmin=0.
endif
if keyword_set(radiusmax) eq 0 then begin
	radiusmax=0.
endif
if keyword_set(wcut) eq 0 then begin
	wcut=0.
endif
if keyword_set(anglemin)  eq 0 then anglemin=-!dpi else anglemin=anglemin*!dpi/180.
if keyword_set(anglemax)  eq 0 then anglemax=!dpi else $
   if anglemax gt 2*!dpi then anglemax=anglemax*!dpi/180.
if anglemax gt !dpi then begin
  print,"Please reset the anglemin & anglemax parameter!"
  return
endif

;===============================================================
tran=1./pixelsize
trans=tran^2.0
;===============================================================for Chandra profile
if strcmp(type,'Chandra') then begin
ctest=readfits(filepath+filename[0],mh)
sz = size(ctest)
ca=readfits(filepath+filename[0],mh)
ba=readfits(filepath+filename[1])
ea=readfits(filepath+filename[2])
endif
;===============================================================for multiband profile
if strcmp(type,'others') then begin
ca=readfits(filepath+filename[0],mh)
sz = size(ca)
ba=ca-ca
ea=ca-ca+1
endif
;===============================================================
numberx=intarr(3)
numbery=intarr(3)
numberx[0]=n_elements(ca[*,0])
numberx[1]=n_elements(ba[*,0])
numberx[2]=n_elements(ea[*,0])
numbery[0]=n_elements(ca[0,*])
numbery[1]=n_elements(ba[0,*])
numbery[2]=n_elements(ea[0,*])

sz[1]=min(numberx)
sz[2]=min(numbery)

cac=fltarr(sz[1],sz[2])
eac=fltarr(sz[1],sz[2])
bac=fltarr(sz[1],sz[2])

cac[0:sz[1]-1,0:sz[2]-1]=ca[0:sz[1]-1,0:sz[2]-1]
bac[0:sz[1]-1,0:sz[2]-1]=ba[0:sz[1]-1,0:sz[2]-1]
eac[0:sz[1]-1,0:sz[2]-1]=ea[0:sz[1]-1,0:sz[2]-1]

; Modified by sw on May 2nd, 2011
;bsiga=bac^2
bsiga=abs(bac)

xp=(sz[1]-1)/2.
yp=(sz[2]-1)/2.

doff=0.

loc2=lindgen(sz[1]*sz[2])
y=loc2/sz[1]
x=loc2 mod sz[1]    
rot_xy,x,y,pa,block=block,xpref=xp*block,ypref=yp*block,/xyreal

nbins=long((radiusmax-radiusmin)/(float(binnumber)*pixelsize))
vbins=(radiusmax-radiusmin)*tran/(nbins)*findgen(nbins+1)+radiusmin*tran

;===============================================================

e_image=eac(*,*)*0.
vdist= findgen(nbins)*0.0
vcount= findgen(nbins)*0.0
vexpt= findgen(nbins)*0.0
vback= findgen(nbins)*0.0
vsig2= findgen(nbins)*0.0
vnorm= findgen(nbins)*0.0
vsback= findgen(nbins)*0.0
n_pix=findgen(nbins)*0.0

c_image=cac
b_image=bac
e_image=eac
bsig_image=bsiga
fcon=fb
efcon=efb

;;;;;;;;;;;;;;;
;;;;;The following calculates intensity profiles according to a given S/N ratio.
;;;;;;;;;;;;;;;

aidimg=e_image-e_image+1

ind=where(atan((y-yp)*ellipticity,x-xp) lt anglemin or atan((y-yp)*ellipticity,x-xp) gt anglemax,com=com_ind)
print,yp,xp,mean(x),mean(y)
if ind[0] ge 0 then begin
  c_image[ind]=0.
  b_image[ind]=0.
  bsig_image[ind]=0.
  e_image[ind]=0
endif

for k=0, nbins-1 do begin
    pix=where(e_image gt 0 and ((x-xp)^2/vbins(k)^2+(y-yp)^2/(vbins(k)*ellipticity)^2) ge 1 and ((x-xp)^2/vbins(k+1)^2+(y-yp)^2/(vbins(k+1)*ellipticity)^2) lt 1 and abs(y-yp) gt wcut*tran/2.,npix)
    n_pix(k)=npix
    if npix[0] ge 0 then begin
      vdist(k)=(vbins[k]+vbins[k+1])*0.5
      vcount(k)=float(total(c_image(pix)))
      vexpt(k)=float(total(e_image(pix)))
      vnorm(k)=float(total(bsig_image(pix)))
      vsback(k)=vexpt(k)*fcon/trans
      vback(k)=float(total(b_image(pix)))+vsback(k)
      vsig2(k)=vcount(k)+vnorm(k)+efcon/trans*vexpt(k)
      aidimg(pix)=aidimg(pix)+k+1
    endif
endfor

aidpix=where(e_image gt 0 and ((x-xp)^2/(radiusmin*tran)^2+(y-yp)^2/(radiusmin*tran*ellipticity)^2) ge 1 and ((x-xp)^2/(radiusmax*tran)^2+(y-yp)^2/(radiusmax*tran*ellipticity)^2) lt 1 and abs(y-yp) gt wcut*tran/2.,aidnpix)
aidimg(aidpix)=c_image(aidpix)+aidimg(aidpix)
window,0,retain=2,xs=1024,ys=1024
tv,bscale(aidimg,min(aidimg),max(aidimg))

surb_1d,vdist,vcount,vback,vexpt,flux,eflux,dist,dlow,dhigh,ctonth=ctonth,stonth=stonth,nounit=nounit,block=block,rsig2=vsig2,pixelsize=pixelsize*60./block

dist=dist-doff
d0=dist
dlow0=dlow-doff
dhigh0=dhigh-doff
;===============================================================for Chandra profile
if strcmp(type,'Chandra') then begin
f0=flux*trans*1e3
ef0=eflux*trans*1e3
endif
;===============================================================for multiband profile
if strcmp(type,'others') then begin
f0=flux                                                                   ;here the units is Image Units/arcmin^2
ef0=eflux   
endif
;===============================================================

print,'data number=',n_elements(dist),'bins=',nbins

if strcmp(type,'Chandra') then begin
window,1,xs=1024,ys=768,retain=2
yrange=[1,max(f0)+fb*1e3]
xrange=[radiusmin,radiusmax]
backconst=fcon*1e3
plot,[0,0],yrange,xtit='Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)',yrange=yrange,xrange=xrange,charsize=2,/ylog
oploterr,dist,f0+fb*1e3,ef0,4
errplot_x,dist,f0+fb*1e3-ef0,f0+fb*1e3+ef0
oplot,xrange,[1.,1.]*fb*1e3
endif

if strcmp(type,'others') then begin
window,1,retain=2
yrange=[min(f0),max(f0)]
xrange=[radiusmin,radiusmax]
plot,dist,f0,yrange=yrange,xrange=xrange, charsize=2
endif

if keyword_set(outfile) ne 0 then begin
openw,lun,outputpath+outfile,/get_lun
printf,lun,dist
printf,lun,dlow0
printf,lun,dhigh0
printf,lun,f0                   ;+fb*1e3
printf,lun,ef0
close,lun
free_lun,lun
endif

end
