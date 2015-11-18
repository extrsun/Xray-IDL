;+
; Main program for detecting sources 
;
;*Requirements:
;
;*INPUTS (defining before running sou_main):
; evtfname - event file name root (without the ext '.fits'), 
;		e.g., 'acisf00945N002_evt2'
; In addition, cenvset needs to be run first for setting up instrument
;	parameters
;
;*OPTIONAL INPUTS:
; evtfdir - directory in which the event file is placed  (def='../xdata/')
; mapdir - the directory in which the instrument maps are located
;		(def ='../xdata/')
; noplot - if equal to 1, outputs will not be plotted on the screen.
; plotout - def = 1. If =0, there will be no ps file output (hardness ratio
; 	maps and sources overplay on the image
; oldbmap - if equal to 1, saved old backg maps will used
; nodetect - if =1, only images are produced, performing no source detection.
; mfile - if not = 0, flist should be given.
; flist - name of the file containing a list of evtfname to search for sources
; greymin, greymax - greyscale lower and upper limits for output plots
;	(def =10, 1000 for greyl=2)
; sband - vector contains bands for separate source detections
;	'B': bv=[1,2,3,4]; 'S': bv=[1,2]; 'H': bv=[3,4]; 
;	'H2': bv=[4]; 'H1': bv=[3]; 'S2': bv=[2]; 'S1': bv=[1]
;	where bv gives. of oldbmap=0 and 'B' is to be included, 'B' should 
;	be included in the first place. Def=['B','S','H']
; dfac - the factor multiplied to the source error radius (def =2) for source
;	mergers
; sbmin, sbmax - if not=0, bmin and bmax defined in sou_det_params are replaced
; wavesd - if =0, a fits soufile needs to be supplied and wavedetection is not
;		run (def=1)
; nband - number of exposure map bands (def = 4). If nband=4, hr1 will be 
;	be calculated.
;
;*Outputs:
;	evtfname_wl - wavelet detection source list
;	evtfname_map* - map detection source list
;	evtfname_map*_ml - Source list after the maximum likelihood analysis
;	evtfname_map*_mlm - merged source list 
;	evtfname_map*_mlm_ratio - source list containing count rates 
;		in individual bands
;	evtfname_map*_mlm_ratio_out - Source list in a (La)TeX format, 
;		including hardness ratios
;	cb, hh - optional output count image and its fits header
;	tb, tbs - exposure map without and with the source region removed
;	cbm - the final background map
;
;*Example:
; cd,'/net/xray/data1/gc/sou'
; instr='acisi' & evtfdir='../xdata/' & mapdir='../xdata/' 
; evtfname='acisf00945N002_evt2'
; noplot=0 		;show the plots
; .run sou_main
;
;*Algorithm:
; see the source detection and analysis website.
;
; Initially written by wqd, 7/8/2001
; Modified to include multiple observations and multiple bands. wqd, May/2002
; For multiple files, sou_final part is included and will run automatically.
; Add the streak background contribution to the smoothed background
; wqd, 1/4/2003
;-
;=======================================================================
;This block of commands are essential for setting up the parameters
if n_elements(evtfdir) eq 0 then evtfdir='../xdata/'
if n_elements(mapdir) eq 0 then mapdir='../xdata/'
if n_elements(noplot) eq 0 then noplot=0
if n_elements(maphead) eq 0 then maphead='_i'
	;the head text of exp maps
;-------------------------------------
if n_elements(greymin) eq 0 then greymin=10
if n_elements(greymax) eq 0 then greymax=1000
if n_elements(dfac) eq 0 then dfac=2.
if n_elements(g2) eq 0 then g2=psf_gaussian(fwhm=2,/norm,npix=121)
;-----------------------------------------------
;setup the source detection parameters:
sou_det_params,instr,dim,block,ccd,bmin,bmax,dsfrac,asfrac,psffile,bpsffile,ftail,aimoff,subfac,vpfname=vpfname,vrfname=vrfname
subfac=1 ;wqd 4/10/02
if n_elements(sbmin) ne 0 then if total(sbmin) ne 0 then bmin=sbmin
if n_elements(sbmax) ne 0 then if total(sbmax) ne 0 then bmax=sbmax
if n_elements(ccdsel) ne 0 then if total(ccdsel) ne 0 then ccd=ccdsel
sradius_min=1. ;minimum source radius for merging 
;---------------------------------------------
; source detection band:
nsband=n_elements(sband)
if nsband eq 0 then begin
	sband=['B','S','H'] ;bands used in the source detection
	nsband=1
endif
sband=strupcase(strtrim(sband,2))
;=======================================================================
; This block is for multiple files only:
if n_elements(mfile) eq 0 then mfile=0
if mfile ne 0 then begin
	openr,un,flist,/get
	evtfnamev='' & fname=''
	while not eof(un) do begin
		readf,un,fname
		evtfnamev=[evtfnamev,fname]
	endwhile
	free_lun,un
	evtfnamev=evtfnamev(1:*)
	nf=n_elements(evtfnamev)
endif else nf=1

for kf=0,nf-1 do begin
if mfile ne 0 then evtfname=evtfnamev(kf)
if !debug eq 2 then stop
;===================================================
;get the data file parameters
fname=evtfdir+evtfname+'.fits'
file_params,fname,h,cra,cdec,expt,nra,ndec,xoff,yoff,roll,aimoff=aimoff
; define source detection subimage parameters
del=!size_pixel*block/3600. ;pixel size in units of deg
xmin=!pref+xoff-dim*block/2 ;low left pixel of the subimage
ymin=!pref+yoff-dim*block/2

;get the fits header of the entire data image
get_fitshead,fltarr(dim,dim),hh,h,del=del,crval=[nra,ndec]
norm=1.e3/(del*60.)^2
;===================================================
map_exp,expt,tb,ta,fhead=evtfname+maphead,mapdir=mapdir,bv=4,tbw=tbw
row={x:0L,y:0L,energy:0,ccd_id:0}
list_xray,fname,l,emin=min(bmin),emax=max(bmax),ccd=ccd,row=row

;generate a subimage in the source detection field
list_image,l,xmin,ymin,cb,dim,block=block,emin=emin,emax=emax,sel=sel
cm=convolve(imdiv(cb,tb),g2)*norm ;sou_final uses it


if nsband gt 1 then begin
 soufile=evtfname+'_map'+ftail
 for nsb=0,nsband-1 do soufile=soufile+strtrim(sband(nsb),2)
 sou_sdb_merge,evtfname,dfac=dfac,soufile,slist=slist,nbv=sband,sradius=sradius_min
	;no systematic error in the merging of sources from the same obs
endif
;======================================================
;The following is copied from sou_final.pro and is run only for multiple files
if mfile eq 0 then stop,'sou_main is finished!'
;sou_final
; calculate hardness ratios of selected sources and generate output table and
; plots
;================================
if n_elements(slist) eq 0 then sou_fits_info,soufile,slist,/all
;Hardness ratio analysis:
; with option for an output of the final source list into a nice LaTex format
; only sources with slow > hrslow are calculated
;probth=-8
probth=-7 ;for single observation
hrslow=4.
;hr_out,slist,hr,hr2,hre,hr2e,outf=soufile+'_out',hrslow=4,probth=probth
hr_cal,slist,slist,hr,hre,hr2,hr2e,hr1,hr1e,outf=soufile+'_hr',hrslow=hrslow,probth=probth,nband=nband,soufile=soufile
;hr_out,slist,slist,hr,hre,hr1,hr1e,hr2,hr2e,outf=soufile+'_hr',texf=soufile+'_hr.tex',hrslow=hrslow,probth=probth,nband=nband
hr_out,slist,outf=soufile+'_hr',texf=soufile+'_hr.tex',nband=nband,hrth=hrth
sn=slist.sn

;========================================
;Hardness ratios:
s2=where(hre lt 0.2 and hr2e lt 0.2,ns2)
print,ns2,' sources are selected for the HR2 plot (s2): '
print,s2+1
if nband eq 4 then s1=where(hre lt 0.2 and hr1e lt 0.2,ns1)
print,ns1,' are selected for the HR1 plot (s1): '
print,s1+1
if noplot eq 0 then begin
	;plot color-color diagram:
	!x.style=1
	!y.style=1
	plot_xy,hr(s2),hr(s2)-hre(s2),hr(s2)+hre(s2),hr2(s2),hr2(s2)-hr2e(s2) $
	 ,hr2(s2)+hr2e(s2),xrang=[-1.2,1.2],yr=[-1.2,1.2],xt='HR',yt='HR2'
	; model hardness ratios
	if n_elements(mpv) eq 0 then mpv=[1,2,3]
	if n_elements(mrv) eq 0 then mrv=[0.1,0.3,1,3,10]
	if n_elements(mnv) eq 0 then mnv=[1.,10,30,100,300]
	hr_model,vpfname,/noer,thick=3,mpv,mnv,hrch=2
	hr_model,vrfname,/noer,mrv,mnv,hrch=2
	xyouts,hr(s2)-0.15,hr2(s2)+0.05,sn(s2)

	if nband eq 4 then begin
		plot_xy,hr(s1),hr(s1)-hre(s1),hr(s1)+hre(s1),hr1(s1) $
		 ,hr1(s1)-hr1e(s1),hr1(s1)+hr1e(s1),xrang=[-1.2,0.6] $
			,yr=[-1.2,1.2],xt='HR',yt='HR1'
		hr_model,vpfname,/noer,thick=3,mpv,mnv,hrch=1
		hr_model,vrfname,/noer,mrv,mnv,hrch=1
		xyouts,hr(s1)-0.15,hr1(s1)+0.05,sn(s1)
	endif
endif
;-----------
if n_elements(plotout) eq 0 then plotout=1
if plotout ne 0 then  begin
	set_plot,'ps'
	device,bits=8,/land,color=1,xsize=18.,ysiz=18,yoff=22,xoff=4
	cs=0.7
	device,filename=soufile+'_hr.ps'
	plot_xy,hr(s2),hr(s2)-hre(s2),hr(s2)+hre(s2),hr2(s2),hr2(s2)-hr2e(s2) $
	,hr2(s2)+hr2e(s2),xrang=[-1.2,1.2],yr=[-1.2,1.2],xt='HR',yt='HR2'
	; model hardness ratios
	hr_model,vpfname,/noer,thick=3,mpv,mnv,hrch=2
	hr_model,vrfname,/noer,mrv,mnv,hrch=2
	xyouts,hr(s2)-0.15,hr2(s2)+0.05,sn(s2)
	if nband eq 4 then begin
	plot_xy,hr(s1),hr(s1)-hre(s1),hr(s1)+hre(s1),hr1(s1),hr1(s1)-hr1e(s1) $
	,hr1(s1)+hr1e(s1),xrang=[-1.2,1.],yr=[-1.2,1.2],xt='HR',yt='HR1'
	hr_model,vpfname,/noer,thick=3,mpv,mnv,hrch=1
	hr_model,vrfname,/noer,mrv,mnv,hrch=1
	xyouts,hr(s1)-0.15,hr1(s1)+0.05,sn(s1)
	endif
	device,/close
	print,soufile+'_hr.ps is created for the color-color diagram!'
	;========================================
	device,filename=soufile+'_cm.ps'
	; Overplay of source positions on the broadband image:
	cont_grey,cm,hh,/noc,cor=cor,greymin=greymin,greymax=greymax $
		,greyl=2,/full,/def,barf=0,f_c=-1,/ps
	source_plot,soufile,cor,hh,slow=0,probth=probth,psym=6.,sym=2 $
		,/fits,s_c=!d.n_colors-1
	source_plot,soufile,cor,hh,slow=0,probth=probth,psym=6.,sym=cs $
		,/fits,/sou_no,sn=slist.sn,s_c=!d.n_colors-1
	device,/close
	;========================================
	set_plot,'x'
	;get the source-removed mask:
	;tbs=source_sub_v(tb,nra,ndec,slist.ra,slist.dec,block=block $
	;	,fac=subfac,/deg,perc=asfrac,psffile=psffile,cra=cra,cdec=cdec)
endif 
print,'sou_final.ps is created!'
;========================================
;===================================================
oldbmap=0
endfor ;for multiple files only
;===================================================

end
