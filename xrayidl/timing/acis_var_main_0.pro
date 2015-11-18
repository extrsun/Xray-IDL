;+
; ACIS_VAR_MAIN_PRE
;
; Main program preparing for variability tests of Chandra ACIS sources
;
; Requirements:
;	(see memo_var)
;	sou_det_params has been run
;	file_root - the root name of the source file (e.g., 
;		file_root='acisf02025N001_evt2_new_clean')
;	maindir - the directory location above sou/, where the soufile is
;		located (e.g., maindir='..')
;	source detection has been carried out: sou_main.pro and sou_final.pro
;	4-band exposure maps have been constructed
;
; Outputs:
;	various IDL source and event structures
;
; written by wqd, Sept 14, 2002.
; 
;-
if n_elements(soufile) eq 0 then $
 soufile=maindir+'/sou/'+file_root+'_map70BSH_hr' $
  else  print,'Using existing source file ',soufile
evtfile=maindir+'/xdata/'+file_root+'.fits'
expfile=maindir+'/xdata/'+file_root+'_i1.fits' ;exposure image used as a filter
if n_elements(cbmfile) eq 0 then $
  cbmfile=maindir+'/sou/'+file_root+'_cbma.fits' $
  else  print,'Using existing source file ',cbmfile

;get baisc observation information:
file_params,evtfile,h,cra,cdec,expt,nra,ndec,xoff,yoff,roll,aimoff=aimoff
if n_elements(subfac) eq 0 then subfac=3. ;factor for source removal radius

;definition of energy bands
bandn=['B','S','H'] ;band names
blow=[0,0,2]	; indexes of the band lower limits
bhi=[3,1,3]	; indexes of the band upper limits
nbandn=n_elements(bandn)
if n_elements(bmin) ne 4 then stop,'The program assumes 4 bands!!!'

;block=4
;dim=512
;--------
;get the events list:
row={x:0L,y:0L,chipx:0L,chipy:0L,energy:0,ccd_id:0,time:0.0d}
;row={x:0L,y:0L,energy:0,ccd_id:0,time:0.0d}
xmin=!pref+xoff-dim*block/2 ;low left pixel of the subimage
ymin=!pref+yoff-dim*block/2
list_xray,evtfile,list,emin=min(bmin),emax=max(bmax),ccd=ccd,row=row

;for testing:
;generate an image in the source detection field
;list_image,list,xmin,ymin,cb,dim,block=block,emin=emin,emax=emax,sel=sel
;tv,bscale(cb,0,3)
;-----------------
; get GTIs
list_xray,evtfile,lti,row={start:0.0d,stop:0.0d},ext=2
tlow=lti.start
thi=lti.stop

;get the background map:
cbma=readfits(cbmfile)

;get source list:
sou_fits_info,soufile,sl,/all ;,slow=slow
rr=sl.sradius/!size_pixel
trans_dist,cra,cdec,sl.ra,sl.dec,xp,yp,pixsize=!size_pixel,/deg
loc=long(yp/block+dim*0.5)*dim+long(xp/block+dim*0.5)
nxp=xp+!pref
nyp=yp+!pref

;get exposure map:
tb=readfits(expfile,h)
;tb=image_comp(tb,0.5)
ts=source_sub_v(tb,nra,ndec,sl.ra,sl.dec,fac=subfac,/deg,sr=rr/float(block),block=block)

end
