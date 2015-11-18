; $Id: Iprofile.pro,v 1.0 2008/12/15
;
; Copyright (c), Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.
;===============================================================

pro Iprofile, filepath=filepath,filename=filename,type=type,outputpath=outputpath,outfile=outfile,ctonth=ctonth,stonth=stonth, binnumber=binnumber,pa=pa,pixelsize=pixelsize,block=block,fb=fb,efb=efb,hsizep=hsizep,hsizen=hsizen,rsizep=rsizep,rsizen=rsizen,wcut=wcut,cut=cut

;===============================================================
;+
; NAME:
;       Iprofile
; PURPOSE:
;       calculate the vertical or parallel profile.
; CALLING SEQUENCE:
;  	Iprofile,filepath=filepath,filename=filename,type=type,outputpath=outputpath,outfile=outfile,ctonth=ctonth,stonth=stonth
;	,binnumber=binnumber,pa=pa,pixelsize=pixelsize,block=block,fb=fb,efb=efb,hsizep=hsizep,hsizen=hsizen,rsizep=rsizep,rsizen=rsizen,wcut=wcut,cut=cut
; INPUT PARAMETERS:
;	filename: data file names, this is an array contains counts file, background file and exposure file, if type='Chandra'. It contains only the
;	intensity file if type='others', note that here it is still an array.
;
;	pa: position angle of the positive x-axis (major axis).
;	pixelsize: pixel size of the image in units of arcmin.
; OPTIONAL INPUT PARAMETERS:
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
;	hsizep: calculating range in unit of arcmin along positive x direction. The default value is 1..
;	hsizen: calculating range in unit of arcmin along negative x direction. The default value is 1..
;	rsizep: calculating range in unit of arcmin along positive y direction. The default value is 1..
;	rsizen: calculating range in unit of arcmin along negative y direction. The default value is 1..
;	wcut: disk height to be masked, |z|>wcut. The default value is 0..
;	cut:  if cut=1, filter the disk region. The default value is 0.
; EXAMPLE:
;	instr='aciss'
;	cenvset,instr
;	filepath='/home/ljt/ljt/data/AboutAGN/NGC5866/image/multicolorimg/HST/original/ACS/'
;	filename='F555W_subsrc.fits'
;	type='others'
;	outputpath='/home/ljt/ljt/data/AboutAGN/NGC5866/image/multiprofile/HST/F555W/'
;	outfile='F555W_h0.02-0.03'
;	pa=54.5-90.
;	block=1
;	pixelsize=0.0498617*block/60. ;in units of arcmin, for ACS
;	hsizep=0.5
;	hsizen=0.5
;	rsizep=0.03
;	rsizen=-0.02
;	Iprofile,filepath=filepath,filename=filename,type=type,pa=pa,pixelsize=pixelsize,hsizep=hsizep,hsizen=hsizen, rsizep=rsizep,rsizen=rsizen,block=block,outputpath=outputpath,outfile=outfile
; NOTE:
;	Note that when rsizep or rsizen equals 0, there will be problem, this problem is currently unresolved, but users may set them with a very small value, for example, smaller than the pixel size.


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
if keyword_set(hsizep) eq 0 then begin
	hsizep=1.
endif
if keyword_set(hsizen) eq 0 then begin
	hsizen=1.
endif
if keyword_set(rsizep) eq 0 then begin
	rsizep=1.
endif
if keyword_set(rsizen) eq 0 then begin
	rsizen=1.
endif
if keyword_set(wcut) eq 0 then begin
	wcut=0.
endif
if keyword_set(cut) eq 0 then begin
	cut=0
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

bsiga=bac^2

xp=(sz[1]-1)/2.
yp=(sz[2]-1)/2.

xmin=xp-hsizen*tran
xmax=xp+hsizep*tran
ymin=yp-rsizen*tran
ymax=yp+rsizep*tran
doff=xp/tran

loc2=lindgen(sz[1]*sz[2])
y=loc2/sz[1]
x=loc2 mod sz[1]    
rot_xy,x,y,pa,block=block,xpref=xp*block,ypref=yp*block,/xyreal

nbins=long((hsizep+hsizen)/(float(binnumber)*pixelsize))
vbins=(xmax-xmin)/(nbins)*findgen(nbins+1)+xmin

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

for k=0, nbins-1 do begin
    if cut eq 0 then pix=where(e_image gt 0 and y ge ymin and y lt ymax and x ge vbins(k) and x lt vbins(k+1),npix)
    if cut eq 1 then pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) gt wcut*tran and x ge vbins(k) and x lt vbins(k+1),npix)
    n_pix(k)=npix
    vdist(k)=(vbins[k]+vbins[k+1])*0.5
    vcount(k)=float(total(c_image(pix)))
    vexpt(k)=float(total(e_image(pix)))
    vnorm(k)=float(total(bsig_image(pix)))
    vsback(k)=vexpt(k)*fcon/trans
    vback(k)=float(total(b_image(pix)))+vsback(k)
    vsig2(k)=vcount(k)+vnorm(k)+efcon/trans*vexpt(k)

;aidimg(pix)=aidimg(pix)+k+1

endfor

if cut eq 0 then aidpix=where(e_image gt 0 and y ge ymin and y lt ymax and x ge vbins(0) and x lt vbins(nbins-1),aidnpix)
if cut eq 1 then aidpix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) gt wcut*tran and x ge vbins(0) and x lt vbins(nbins-1),aidnpix)
aidimg(aidpix)=c_image(aidpix);+aidimg(aidpix)
imgnumx=700
imgnumy=700
window,0,retain=2,xs=imgnumx,ys=imgnumy
aidimg=congrid(aidimg,imgnumx,imgnumy,/interp)
tv,bscale(aidimg,min(aidimg),max(aidimg))

surb_1d,vdist,vcount,vback,vexpt,flux,eflux,dist,dlow,dhigh,ctonth=ctonth,stonth=stonth,nounit=nounit,block=block,rsig2=vsig2,pixelsize=pixelsize*60./block

dist=dist-doff
d0=dist
dlow0=dlow-doff
dhigh0=dhigh-doff
;===============================================================for Chandra profile
if strcmp(type,'Chandra') then begin
f0=flux*trans*1e4
ef0=eflux*trans*1e4
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
yrange=[1,max(f0)+fb*1e4]
xrange=[-hsizen,hsizep]
backconst=fcon*1e4
plot,[0,0],yrange,xtit='Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)',yrange=yrange,xrange=xrange,charsize=2,/ylog
oploterr,dist,f0+fb*1e4,ef0,4
errplot_x,dist,f0+fb*1e4-ef0,f0+fb*1e4+ef0
oplot,xrange,[1.,1.]*fb*1e4
endif

if strcmp(type,'others') then begin
window,1,retain=2
yrange=[min(f0),max(f0)]
xrange=[-hsizen,hsizep]
plot,dist,f0,yrange=yrange,xrange=xrange, charsize=2
endif

if keyword_set(outfile) ne 0 then begin
openw,lun,outputpath+outfile,/get_lun
printf,lun,dist
printf,lun,dlow0
printf,lun,dhigh0
printf,lun,f0                   ;+fb*1e4
printf,lun,ef0
close,lun
free_lun,lun
endif

end
