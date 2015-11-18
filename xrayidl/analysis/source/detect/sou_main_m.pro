;pro sou_main,instr,evtfname,cb,hh,tb,tbs,cbm,hr,hre,hr2,hr2e,evtfdir=evtfdir,fhead=fhead,mapdir=mapdir,noplot=noplot
;+
; Main script for X-ray source detection
;
;*Requirements:
; If you need to revise sou_det_params.pro, which sets up the basic
; parameters for the source detection, you nedd copy the file 
; from /net/xray/pub/xrayidl/analysis/source/detect/
; to a slightly different name (e.g., sou_det_params_m.pro) in the directory, 
; where the source detection is going be excecuted and outputs will be 
; placed. Finally, you probably need to .run the file (e.g.,
; .run sou_det_params_m) to make it current.
;
; Three exposure maps, named something like evtfname_i1.fits, where '1' refers 
; to band 1. The maps need to be produced with dimension, block size,
; and energy bands (bmin, bmax)  as defined in sou_det_params.pro
; 
;*Inputs:
; evtfname - event file name without the ext '.fits', 
;		e.g., 'acisf00945N002_evt2'
; evtfdir - directory in which the event file is placed  (def='../xdata/')
; fhead - the head name of the instrument maps (def = evtfname_i)
; mapdir - the directory in which the instrument maps are located
;		(def ='../xdata/')
; noplot - if set, outputs will not be plotted on the screen.
;
;*Outputs:
;	evtfname_wl - wavelet detection source list
;	evtfname_map* - map detection source list
;	evtfname_map*_ml* - Source list after the maximum likelihood analysis
;	evtfname_map*_mlm - merged source list 
;	evtfname_ratio - source list containing count rates in individual bands
;	evtfname_out - Source list in a (La)TeX format, 
;		including hardness ratios
;	cb, hh - optional output count image and its fits header
;	tb, tbs - exposure map without and with the source region removed
;	cbm - the final background map
;
;*Example:
; cd,'/net/xray/data1/n4631/sou'
; sou_main,'aciss',evtfdir='../xdata/','NGC4631_evt2_cleanpix',cb,hh,tb,tbs,cbm,mapdir='../xdata/',/noplot
;
;*Algorithm:
; see the source detection and analysis website.
;
; written by wqd, 7/8/2001
;-
;=======================================================================
;This block of commands are essential for setting up the parameters
if n_elements(evtfdir) eq 0 then evtfdir='../xdata/'
if n_elements(mapdir) eq 0 then mapdir='../xdata/'

;set the instrument environment:
; (even if you restore from a idlsave.dat file, you still need to run this)
cenvset,'',instr=instr ;lower case

;setup the source detection parameters:
sou_det_params,instr,dim,block,ccd,emin,emax,bmin,bmax,dsfrac,asfrac,psffile,bpsffile,ftail,aimoff,subfac

;get the data file parameters
fname=evtfdir+evtfname+'.fits'
file_params,fname,h,cra,cdec,expt,nra,ndec,xoff,yoff,roll,aimoff=aimoff

; define source detection subimage parameters
del=!size_pixel*block/3600. ;pixel size in units of deg
xmin=!pref+xoff-dim*block/2 ;low left pixel of the subimage
ymin=!pref+yoff-dim*block/2

;get the fits header of the entire data image
get_fitshead,fltarr(dim,dim),hh,h,del=del,crval=[nra,ndec]

;====================================================
;get the count list
list_xray,fname,l,emin=emin,emax=emax,pior='energy',ccd=ccd

;generate a subimage in the source detection field
list_image,l,xmin,ymin,cb,dim,block=block,emin=emin,emax=emax

;visualize the image
if not keyword_set(noplot) then begin
	window,xs=dim,ys=dim
	loadct_self,18
	tv,bscale(cb,0,2)
endif

;get exposure maps for both source detection and hardness ratio calculations:
map_exp,expt,tb,ta,fhead=evtfname+'_i',mapdir=mapdir

;------------------------------------------------
;wavelet source detection on 4 scales:
soufile=evtfname+'_wl'
scan_wl_main,nra,ndec,imdiv(cb,tb),imdiv(cb,tb^2),wbin=1./2^(findgen(3)),block=block,mask=tb,wims=wl,outfile=soufile
;wl contains the S/N ratio of the wavelet images 
;if you get thousands of sources, then something is wrong with the image:
;e.g., a shift between the exposure map and count map may be the cause.

;show the image and sources
if keyword_set(noplot) eq 0 then begin
	cor=[0,1,0,1]
	cont_grey,cb,hh,lev=1.e10,cor=cor,greymin=0,greymax=4,/full
	source_plot,soufile,cor,hh,slow=0,psym=6,sym=2
endif
print,'Wavelet detection finished! Source file: ',soufile

;--------------
;get source info and source-removed mask
source_info,sn,sra,sdec,slow=2.5,souf=soufile,/deg
tbs=source_sub_v(tb,nra,ndec,sra,sdec,block=block,fac=subfac,/deg,perc=asfrac,psffile=psffile,cra=cra,cdec=cdec)

;check to see if the sources removal is reasonably cleanly:
if keyword_set(noplot) eq 0 then begin
	c=cb
	c(where(tbs le 0.))=0.
	tv,bscale(c,0,2)
;for bright sources, there might be a halo remaining. That might be OK, since
;a median filter will be applied in the background map creation.
endif
;---------------------------------
;get a background map:
get_bmap,cb,tb,tbs,cbm

;check any residual source effects:
if keyword_set(noplot) eq 0 then begin
	cont_grey,cbm,hh,lev=1.e10,cor=cor,/full 
	source_plot,soufile,cor,hh,slow=1,psym=6,sym=2
endif
writefits,evtfname+'_wl0.fits',cbm,hh 
;===================================================
; map source detection (which may take quite some time to finish): 
slow=2.5
;radius=8.4
radius=1.e10 ;the entire field

soufile=evtfname+'_map'+ftail
print,'running scan_map, which may take quite some time.'
scan_map,nra,ndec,cb,cbm,tb,xoff,yoff,block=block,sigma=s,cntr=c,thre=slow,sfrac=dsfrac,outr=radius,outf=soufile,psffile=psffile
print,'Map detection finished!'

if not keyword_set(noplot) then begin
	cont_grey,cb,hh,lev=1.e10,cor=cor,greymin=0,greymax=4,/full
	source_plot,soufile,cor,hh,slow=slow,psym=6,sym=2
endif
;==========================================
; Maximum Likelihood analysis:
ml_anal,l,cra,cdec,tb,cbm,tblock=block,chn=1,inf=soufile,slow=slow,rxp=xoff+!pref,ryp=yoff+!pref,sfrac=asfrac,psffile=psffile ;,radius=2

;Merge sources:
soufile=soufile+'_ml'
source_merge,cra,cdec,sfrac=dsfrac,soufile,slow=slow

soufile=soufile+'m'
if keyword_set(noplot) eq 0 then begin
	cont_grey,cb,hh,lev=1.e10,cor=cor,greymin=0,greymax=5,/full
	source_plot,soufile,cor,hh,slow=slow,psym=6.,sym=2
endif
print,'Maximum Likelihood analysis finished! Source file: ',soufile
;-------------------------------------------
;count rates in bands (Here default bands are used. Otherwise bands need to be defined)
map_back,l,xmin,ymin,dim,bmin,bmax,tb,tbs,ba,block=block 

map_ratio,l,cra,cdec,bmin,bmax,ta,ba,tb=block,nra=nra,ndec=ndec,th=slow,inf=soufile,sfrac=asfrac,psffile=bpsffile,radius=radius
;Small source parameter differences from ml_anal are expected, 
;because the background maps and source radii are different in different bands.

;================================
;Hardness ratio analysis:
; with option for an output of the final source list into a nice LaTex format
soufile=soufile+'_ratio'
hr_out,soufile,hr,hr2,hre,hr2e,sn,outf=soufile+'_out'

;plot color-color diagram:
!x.style=1
!y.style=1
s=where(hre lt 0.2 and hr2e lt 0.2)
set_plot,'ps'
plot_xy,hr(s),hr(s)-hre(s),hr(s)+hre(s),hr2(s),hr2(s)-hr2e(s),hr2(s)+hr2e(s),xrang=[-1.2,1.2],yr=[-1.2,1.2],xt='HR1',yt='HR2'

; model hardness ratios
hr_model,'vp',/noer,thick=3
hr_model,'vr',/noer
xyouts,hr(s)-0.15,hr2(s)+0.05,sn(s)
;ps output
device,/close
print,'idl.ps is created for the color-color diagram!'
$gv idl.ps
set_plot,'x'

;get the final source info and source-removed mask:
source_info,sn,sra,sdec,slow=2.5,souf=soufile,/deg,/deci
tbs=source_sub_v(tb,nra,ndec,sra,sdec,block=block,fac=subfac,/deg,perc=asfrac,psffile=psffile,cra=cra,cdec=cdec)

;get the final background map 
get_bmap,cb,tb,tbs,cbm
writefits,evtfname+'_0.fits',cbm,hh ;for later timing analysis

stop,'stopped inside sou_main: now you can do some checks of the results.'
end
