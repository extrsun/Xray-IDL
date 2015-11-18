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
; dfac - the factor multiplied to the source error radius (def =1) for source
;	mergers
; sbmin, sbmax - if not=0, bmin and bmax defined in sou_det_params are replaced
; wavesd - if =0, a fits soufile needs to be supplied and wavedetection is not
;		run (def=1)
; nband - number of exposure map bands (def = 4). If nband=4, hr1 will be 
;	be calculated.
; fac_ssr - fraction to be enlarged for source radius in source
;           search. if not =1, no correction for PSF scattering is
;           applied. Useful in finding extended sources.
;
;*Outputs:
;	evtfname_wl - wavelet detection source list
;	evtfname_map* - map detection source list
;	evtfname_map*_ml - Source list after the maximum likelihood analysis
;	evtfname_map*_mlm - merged source list 
;	cbm - the final background map
;
;*Example:
; follow memo_anal
;
;*Algorithm:
; see the source detection and analysis website.
;
; Initially written by wqd, 7/8/2001
; Modified to include multiple observations and multiple bands. wqd, May/2002
; For multiple files, sou_final part is included and will run automatically.
;
; Add the streak background contribution to the smoothed background
; (sbyes, sb; see m82/sou/memo)
; wqd, 5/7/2003
;-
;=======================================================================
;This block of commands are essential for setting up the parameters
cenvset,instr

nimpsf=n_elements(aimpsf) ;too see if the psf images are provided
if nimpsf ne 0 then detagname='m' ;use mx and my for calculating eer of evts

fra=2 ;reducing fraction of image dimension for smoothing
if n_elements(probth) eq 0 then probth=-6
if n_elements(cblock) eq 0 then cblock=1 
      ;for the source search in the expanded central region
if n_elements(cdim) eq 0 then cdim=512
if cblock ne 0 then begin
    inr=cdim*float(cblock)*!size_pixel/60.*0.4 ;0.4 for edge 
    outr=inr*1.1 ;give a bit overlap with outr of the previous run
endif else inr=0.
    
if n_elements(evtfdir) eq 0 then evtfdir='../xdata/'
if n_elements(mapdir) eq 0 then mapdir='../xdata/'
if n_elements(noplot) eq 0 then noplot=0
if n_elements(maphead) eq 0 then maphead='_i'
	;the head text of exp maps
;-------------------------------------
if n_elements(greymin) eq 0 then greymin=3
if n_elements(greymax) eq 0 then greymax=100
if n_elements(dfac) eq 0 then dfac=2. 
  ;final merge radius factor of the position errors 
if n_elements(g2) eq 0 then g2=psf_gaussian(fwhm=2,/norm,npix=121)
;-----------------------------------------------
;setup the source detection parameters:
sou_det_params,instr,dim,block,ccd,bmin,bmax,dsfrac,asfrac,psffile,bpsffile,ftail,aimoff,subfac,vpfname=vpfname,vrfname=vrfname,tabfile=tabfile,dhrch=dhrch
;asfrac=0.7 ;commented out on June 16, 2007
if n_elements(esubfac) ne 0 then subfac=esubfac
if n_elements(edim) ne 0 then dim=edim
if n_elements(mbmin) gt 1 then bmin=mbmin ;set mbmin= scalar if not used
if n_elements(mbmax) gt 1 then bmax=mbmax 
if n_elements(mblock) ne 0 then if mblock ne 0 then block=mblock
;change the default block

if n_elements(sbmin) ne 0 then if total(sbmin) ne 0 then bmin=sbmin
if n_elements(sbmax) ne 0 then if total(sbmax) ne 0 then bmax=sbmax
if n_elements(ccdsel) ne 0 then if total(ccdsel) ne 0 then ccd=ccdsel
if n_elements(sradius_min) eq 0 then sradius_min=1. 
 ;minimum source radius for merging 
;---------------------------------------------
; source detection band:
nsband=n_elements(sband)
if nsband eq 0 then begin
	sband=['B','S','H'] ;bands used in the source detection
	nsband=3
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
if n_elements(evtfname) eq 0 then begin
    if !instr eq 'epic' then evtfname='pn' else evtfname='evt2file_new_clean'
endif
soufroot=evtfname
;===================================================
;get the data file parameters
fname=evtfdir+evtfname+'.fits'
file_params,fname,hdr,cra,cdec,expt,nra,ndec,xoff,yoff,roll,aimoff=aimoff
; define source detection subimage parameters
del=!size_pixel*block/3600. ;pixel size in units of deg
xmin=!pref+xoff-dim*long(block)/2 ;low left pixel of the subimage
ymin=!pref+yoff-dim*long(block)/2

;get the fits header of the entire data image
get_fitshead,fltarr(dim,dim),hh,hdr,del=del,crval=[nra,ndec]
norm=1.e3/(del*60.)^2
;        slow=2.5
       if n_elements(slow) eq 0 then slow=2.
;===================================================
; This block is for multiple band detections:
if n_elements(nband) eq 0 then nband=4
for nsb=0,nsband-1 do begin
 case sband(nsb) of
	'B': bv=indgen(nband)+1
	'S': bv=indgen(nband-2)+1
	'H': bv=indgen(2)+(nband-1)
	'S1': bv=[1]
	'S2': bv=[2]
	'H1': bv=nband-1
	'H2': bv=nband
	else: stop,'not recorgnized source detection band name!!!'
 endcase
 nb=n_elements(bv)
;======================================
 emin=min(bmin(bv-1)) & emax=max(bmax(bv-1))
 !blow=emin & !bhigh=emax
 print,'emin,emax = ',emin,emax
;get exposure maps for both source detection and hardness ratio calculations:
if n_elements(subarray) eq 0 then $
 map_exp,expt,tb,ta,fhead=evtfname+maphead,mapdir=mapdir,bv=bv,tbw=tbw,mask=mask $
else begin 
 if subarray ne 0 then $
   map_exp,expt,tb,ta,fhead=evtfname+maphead,mapdir=mapdir,bv=bv,tbw=tbw,hdr=hh,mask=mask
endelse 
;=========================================
 if nsb eq 0 then begin ;this block is needed only for the first band
;---------
;get the count list
    if !instr eq 'epic' then tagin=['X','Y','PI','CCDNR']
    if n_elements(detagname) eq 0 then  begin
        row={x:0.,y:0.,energy:0,ccd_id:0} 
        list_xray,fname,list,emin=min(bmin),emax=max(bmax),ccd=ccd,row=row,tagin=tagin
    endif else list_xray,fname,list,emin=min(bmin),emax=max(bmax),ccd=ccd,tagin=tagin
;generate a subimage in the source detection field
    if n_elements(sbyes) eq 0 then sbyes=0
    if sbyes eq 0 then begin
      wl_emap,tb,block,list,xmin,ymin,dim,fb,tbe,fbe,emin=emin,emax=emax
    endif else begin
      wl_emap,tb,block,list,xmin,ymin,dim,fb,tbe,fbe,emin=emin,emax=emax $
              ,sb=total(sb(*,*,bv-1),3)
    endelse 
    cmb=convolve(fb,g2)*norm
    
    if n_elements(nodetect) ne 0 then begin
	cont_grey,cmb,hh,/noc,cor=cor,greymin=greymin,greymax=greymax $
		,greyl=2,/full,/def,barf=0,f_c=-1
	exit 
    endif
;------------------------------------------------
    soufile=soufroot+'_wl'
    if n_elements(wavesd) eq 0 then wavesd=1
    if wavesd then begin
        if n_elements(ewbin) ne 0 then wbin=ewbin else $
          wbin=1./2^(findgen(3)) ;wavelet source detection on 3 scales:
        if sbyes ne 0 then $
          scan_wl_main,nra,ndec,fb,imdiv(fb,tb),wbin=wbin $
          ,block=block,mask=tb,wims=wl,outfile=soufile,threshold=slow $
          ,bfluxe=fbe,inr=inr $
        else $
          scan_wl_main,nra,ndec,fb,imdiv(fb,tb),wbin=wbin $
          ,block=block,mask=tb,wims=wl,outfile=soufile,threshold=slow,inr=inr

 ;wl contains the S/N ratio of the wavelet images 
 ;if you get thousands of sources, then something is wrong with the image:
                                ;e.g., a shift between the exposure
                                ;map and count map may be the cause.
;============================================================================
;block for refined search with small block in the central region 
        if cblock ne 0 then begin
            print,'WL searching in the central region'
            if sbyes eq 0 then begin
                wl_emap,tb,block,list,xmin,ymin,cdim,fb,tbe,fbe $
                  ,block=cblock,emin=emin,emax=emax ,xmine=xmine,ymine=ymine
            endif else begin
                wl_emap,tb,block,list,xmin,ymin,cdim,fb,tbe,fbe $
                  ,block=cblock,emin=emin,emax=emax $
                  ,sb=total(sb(*,*,bv-1),3),xmine=xmine,ymine=ymine
            endelse 
            cmbe=convolve(fb,g2)*(norm*(block/float(cblock))^2)
            get_fitshead,cmbe,cmbch,hh,del=del*cblock/block
            if n_elements(nodetect) ne 0 then begin
                cont_grey,cmbe,cmbch,/noc,cor=cor,greymin=greymin $
                  ,greymax=greymax*10,greyl=2,/full,/def,barf=0,f_c=-1
                exit 
            endif
;------------------------------------------------
            if sbyes ne 0 then $
              scan_wl_main,nra,ndec,fb,imdiv(fb,tbe),wbin=1./2^(findgen(2)) $
              ,block=cblock,mask=tbe,wims=wlc,outfile=soufile,threshold=slow $
              ,bfluxe=fbe,/append,radius=outr $
            else $
              scan_wl_main,nra,ndec,fb,imdiv(fb,tbe),wbin=1./2^(findgen(2)) $
              ,block=cblock,mask=tbe,wims=wl,outfile=soufile,threshold=slow $
              ,/append,radius=outr
        endif
;============================================================================
 ;show the image and sources
        if noplot eq 0 then begin
            cont_grey,cmb,hh,/noc,cor=cor,greymin=greymin,greymax=greymax $
		,greyl=2,/full,/def,barf=0,f_c=-1
            source_plot,soufile,cor,hh,slow=0,psym=6,sym=2
        endif
        print,'Wavelet detection finished! Source file: ',soufile
        ;source_info,sn,sra,sdec,slow=0,souf=soufile,/deg ;,ston,cntr
    endif 
    if n_elements(oldbmap) eq 0 then begin
        oldbmap=0
        print,'using the soufile = ',soufile,' for source subtraction!!!'
    endif 
	
    tao=ta
;endelse
    if oldbmap eq 0 then begin
;        source_info,sn,sra,sdec,slow=3,souf=soufile,/deg 
        source_info,sn,sra,sdec,slow=slow,souf=soufile,/deg 
;--------------
if nimpsf ne 0 then begin
    trans_dist,cra,cdec,sra,sdec,xp,yp,/deg,pixsize=!size_pixel
    sz=size(tb)
    loc=long(yp/block+sz(2)*0.5)*sz(1)+long(xp/block+sz(1)*0.5)
    map_impsf,impsf,aimpsf,bv=bv,tbw=tbw
    sradius=impsf(loc)
    tbs=source_sub_v(tb,cra,cdec,sra,sdec,block=block,fac=subfac $
        ,/deg,cra=cra,cdec=cdec,sradius=sradius) ;just rescale sradius ,perc=asfrac
endif else tbs=source_sub_v(tb,cra,cdec,sra,sdec,block=block,fac=subfac $
                         ,/deg,perc=asfrac,cra=cra,cdec=cdec)             
;no cntr scaling wqd 4/10/02
;check to see if the sources removal is reasonably cleanly:
;if noplot eq 0 then begin
;	c=cm
;	c(where(tbs le 0.))=0.
;	cont_grey,c,hh,/noc,cor=cor,greymin=greymin,greymax=greymax $
;		,greyl=2,/full,/def,barf=0,f_c=-1
;for bright sources, there might be a halo remaining. That might be OK so that
;the halo will be accounted for the background. Few fake sources will be 
;detected in the halo region.
;endif 
;---------------------------------
;get a background maps for all bands:
;commented by wqd, Jan 6, 2003
;if sband(nsb) ne 'B' then oldbmap=1 $ ; using old backg maps
;	else begin
	;get_bmap,cb,tb,tbs,cbm ;default th1=10
	print,"I am HERE!",block
	dbmaps_as,list,xmin,ymin,block,dim,bmin,bmax,tb,tbs,ta,cbm,cbma $
          ,sb=sb,ncmin=1.e-3,fra=fra,expminf=expminf
        hh3=hh
	; writefits,evtfname+'_cbma.fits',cbma,hh
        writefits,evtfname+'_cbma.fits',cbma,hh3 ;writefits changes the header
;	oldbmap=1 ;use the background maps for other bands
;for other bands, it would not be here! wqd, Jan 6, 2003
     endif else begin
	print,'Using existing background maps!!!'
	cbma=readfits(evtfname+'_cbma.fits')
    endelse
;----------------------
 endif ;for nsb = 0 
;===================================================
if nimpsf ne 0 then begin
    map_impsf,impsf_d,aimpsf_d,bv=bv,tbw=tbw
    map_impsf,impsf_cd,aimpsf_cd,bv=bv,tbw=tbw
endif
;cbm=readfits(evtfname+'_0.fits')
;use the bands specified in bv
 if nb gt 1 then cbm=total(cbma(*,*,bv-1),3) else cbm=cbma(*,*,bv-1)
probth_map=probth+2
 soufile=soufroot+'_map'+ftail+strtrim(sband(nsb),2)
;-----------------------
if n_elements(conly) eq 0 then conly=0
if conly eq 0 then begin
;generate the image for the source detection
list_image,list,xmin,ymin,cb,dim,block=block,emin=emin,emax=emax,sel=sel
; map source detection (which may take quite some time to finish): 
 print,'running scan_map, which may take quite some time.'
 scan_map,nra,ndec,cb,cbm,tb,xoff,yoff,block=block,sigma=s,cntr=c $
 ,thre=probth_map,sfrac=dsfrac,inr=inr,outf=soufile,psffile=psffile,fac_ssr=fac_ssr,blow=emin,bhigh=emax,impsf=impsf_d
;   ,thre=probth,sfrac=dsfrac,inr=inr,outf=soufile,psffile=psffile
;count rates are overestimated when the chosen aperture sizes 
; (small dsfrac) are smaller than the bin size (forced to be larger than 1).
 print,'Map detection finished!'

 if sbyes ne 0 then fb=imdiv(cb-total(sb(*,*,bv-1),3),tb) else fb=imdiv(cb,tb)
;cm=convolve(imdiv(cb,tb),g2)*norm ;sou_final uses it
 cm=convolve(fb,g2)*norm 
 if nsb eq 0 then cmb=cm ;sou_final uses it
 if noplot eq 0 then begin
	cont_grey,cm,hh,/noc,cor=cor,greymin=greymin,greymax=greymax $
		,greyl=2,/full,/def,barf=0,f_c=-1
	source_plot,soufile,cor,hh,slow=0,probth=probth_map,psym=6,sym=2,/fits
    endif
endif 
;==========================================
;central region
if cblock ne 0 then begin
    if n_elements(cdsfrac) eq 0 then cdsfrac=0.5 ; dsfrac
    if n_elements(cfac_ssr) eq 0 then cfac_ssr=0.5 ;fac_ssr
    wl_emap,tb,block,list,xmin,ymin,cdim,fb,tbe,fbe,cbme,sel=selc $
     ,block=cblock,emin=emin,emax=emax,sb=cbm,cbe=cbe,xmine=xmine,ymine=ymine $
            ,imcoff=imcoff,impsf=impsf_cd,eimpsf=eimpsf_cd
    if n_elements(imcoff) eq 0 then imcoff=[0,0]
    cimcoff=imcoff*(block/cblock)
  if conly eq 0 then begin
    scan_map,nra,ndec,cbe,cbme,tbe,xoff,yoff,block=cblock,sigma=s,cntr=c $
   ,thre=probth_map,sfrac=cdsfrac,fac_ssr=cfac_ssr,blow=emin,bhigh=emax $
             ,outf=soufile,psffile=psffile,imcoff=cimcoff,/append,impsf=eimpsf_cd ;,outr=outr
    ;fac_ssr here is used only for the source search in star_search_map
  endif else begin
    scan_map,nra,ndec,cbe,cbme,tbe,xoff,yoff,block=cblock,sigma=s,cntr=c $
   ,thre=probth_map,sfrac=cdsfrac,fac_ssr=cfac_ssr,blow=emin,bhigh=emax $
             ,outf=soufile,psffile=psffile,imcoff=cimcoff,impsf=eimpsf_cd
    sel=selc
endelse
    cmbc=convolve(fb,g2)*(norm*(block/float(cblock))^2);cbm subtracted flux map
    if nsb eq 0 then begin
        cp=(cdim+1.)*0.5-cimcoff
        get_fitshead,cmbc,cmbch,hh,del=del*cblock/block,cpx=cp(0),cpy=cp(1)
        bcmbc=cmbc
;        cgreymin=min(bcmbc) > 1.
;        cgreymax=max(bcmbc)
    endif 
    if noplot eq 0 then begin
	cont_grey,bcmbc,cmbch,/noc,cor=cor,greymin=greymin,greymax=greymax*10 $
		,greyl=2,/full,/def,barf=0,f_c=-1
	source_plot,soufile,cor,cmbch,slow=0,probth=probth_map,psym=6,sym=2,/fits
    endif
endif
;==========================================
; Maximum Likelihood analysis:
 ml_anal,list(sel),cra,cdec,tb,cbm,tblock=block,chn=1,infile=soufile $
   ,slow=probth_map,rxp=xoff+!pref,ryp=yoff+!pref,sfrac=dsfrac,psffile=psffile $
         ,slist=slist,dig=dig,fac_ssr=fac_ssr,blow=emin,bhigh=emax,silent=silent $
         ,impsf=impsf_d,detagname=detagname
 print,'Maximum Likelihood analysis finished! Source file: ',soufile
;stop
;-------------------------------------------
;count rates in bands (Here default bands are used. Otherwise bands need to be defined)
 soufile=soufile+'_ml'
 map_ratio,slist,list,cra,cdec,bmin,bmax,tao,cbma,block,nra=nra,ndec=ndec $
   ,sfrac=asfrac,radius=radius,outfile=soufile,infile=soufile,probth=probth_map $
   ,fac_ssr=fac_ssr,bv=bv,aimpsf=aimpsf,detagname=detagname
;
;Small source parameter differences from ml_anal are expected, 
;because the background maps and source radii are different in different bands.
;the psf radius increases with energy. For a source with high absorption, for
; example, the sum of the backg counts from individual bands could be 
; substantially higher than from the broad band.
;----------------------------------------------------
;Merge sources:
;source_merge,cra,cdec,sfrac=dsfrac,soufile,slow=slow
 probth_merge=probth+1
 sou_merge_fits,soufile,soufile+'m',outslist=slist,probth=probth_merge,dfac=dfac $
	,sradius=0,ns=ns
	;no systematic error in the merging of sources from the same band

 soufile=soufile+'m'
 if noplot eq 0 and ns ne 0 then begin
;	cont_grey,cmb,hh,/noc,cor=cor,greymin=greymin,greymax=greymax
;	$
	cont_grey,cmb,hh,/noc,cor=cor,greymin=greymin,greymax=greymax $
		,greyl=2,/full,/def,barf=0,f_c=-1
	source_plot,soufile,cor,hh,slow=0,probth=probth_merge,psym=6.,sym=2,/fits
 endif
 print,'Sources are merged to the file: ',soufile
;stop
 if !debug eq 3 then stop
;sou_fits_info,soufile,slist,/all        
;if min((slist.cntrb1+slist.cntrb2)) le 0 then stop
;------------------------------------
if n_elements(no_smina) eq 0 then no_smina=1
if no_smina eq 0 then begin
;create detection lower limit map:
sz=size(tb)
dist_circle,dis,sz(1),(sz(1)+1.)/2.,(sz(2)+1.)/2.
bin_sel=where(tb gt 0) ;size may be changed in map_extract
if nimpsf ne 0 then begin
    map_impsf,impsf,aimpsf,bv=bv,tbw=tbw
    core_size=impsf(bin_sel)
endif else begin
    dd=dis(bin_sel)*(block*!size_pixel/60.) ; in units of arcmin
    psf_params,dd,core_size,perclimit=asfrac,blow=emin,bhigh=emax,instr=instr,psffile=psffile
endelse
core_size=core_size/block
map_extract,core_size,tb,cbm,bin_sel=bin_sel,core_2=bs,core_1=exptv $
  ,core_bin=core_bin ;,crpix=crpix,/scale,asfrac=asfrac,nbin=nbin,block=block
exptv=exptv/core_bin
bs=bs*(core_size^2*!pi/core_bin)
poisson_inv,double(bs),1.-10^double(probth),slo ;,/interp
		;source detection lower limit in counts
slo=slo > (fix(2.5+sqrt(bs*10))+1)
;slo=slo > (2.5+sqrt(bs*10)) 
 ;slightly biased to small value to compensate the foolowing differences
smina=cbm*0.
smina(bin_sel)=((slo-bs)/exptv)/asfrac 
  ;this broad band rate coult be slightly different the sum of the
  ;rates in subbands as for source rates
writefits,soufile+'_smina',smina,hh
endif 
;===================================================
endfor ; for multiple bands
if nsband gt 1 then begin
 fmid='_map'+ftail
 soufile=soufroot+fmid
 for nsb=0,nsband-1 do soufile=soufile+strtrim(sband(nsb),2)
if n_elements(fdfac) eq 0 then fdfac=1.5
 sou_sdb_merge,evtfname,soufile,dfac=fdfac,slist=slist,nbv=sband,fmid=fmid,probth=probth ;,sradius=sradius_min
endif
 if noplot eq 0 and ns ne 0 then begin
	cont_grey,cmb,hh,/noc,cor=cor,greymin=greymin,greymax=greymax*10 $
                  ,greyl=2,/full,/def,barf=0,f_c=-1
;	source_plot,soufile,cor,hh,slow=0,probth=probth_merge,psym=6.,sym=2,/fits
	source_plot,soufile,cor,hh,slow=0,probth=probth,psym=6.,sym=2,/fits
 endif
;======================================================
; MODIFIED by sw, Apr. 22 2011
;Original: 
if mfile eq 0 then stop,'sou_main is finished!'
;stop,'sou_main is finished!'

;The following is copied from sou_final.pro and is run only for multiple files
;sou_final
; calculate hardness ratios of selected sources and generate output table and
; plots
;================================
if n_elements(slist) eq 0 then sou_fits_info,soufile,slist,/all
;Hardness ratio analysis:
; with option for an output of the final source list into a nice LaTex format
; only sources with slow > hrslow are calculated
;probth=-8
;probth=probth ;for single observation
hrslow=4. ;signal-to-noise of the broad band count rate for calculating HRs
if n_elements(hrth) eq 0 then hrth=0.2 ; threshold of HR error for hardcopy
;output in the table and plots.
;hr_out,slist,hr,hr2,hre,hr2e,outf=soufile+'_out',hrslow=4,probth=probth
hr_cal,slist,slist,hr,hre,hr2,hr2e,hr1,hr1e,hrslow=hrslow,probth=probth,nband=nband,dhrch=dhrch ;,soufile=soufile
;hr_cal,slist,slist,hr,hre,hr2,hr2e,hr1,hr1e,outf=soufile+'_hr',hrslow=hrslow,probth=probth,nband=nband,soufile=soufile
;hr_out,slist,slist,hr,hre,hr1,hr1e,hr2,hr2e,outf=soufile+'_hr',texf=soufile+'_hr.tex',hrslow=hrslow,probth=probth,nband=nband
hr_out,slist,outf=soufile+'_hr',texf=soufile+'_hr.tex',nband=nband,hrth=hrth
sn=slist.sn

;========================================
;Hardness ratios:
s2=where(hre lt hrth and hr2e lt hrth,ns2)
print,ns2,' sources are selected for the HR2 plot (s2): '
print,s2+1
if nband eq 4 then s1=where(hre lt hrth and hr1e lt hrth,ns1)
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
	cont_grey,cmb,hh,/noc,cor=cor,greymin=greymin,greymax=greymax $
		,greyl=2,/full,/def,barf=0,f_c=-1,/ps
	source_plot,soufile,cor,hh,slow=0,probth=probth,psym=6.,sym=2 $
		,/fit,s_c=!d.n_colors-1
	source_plot,soufile,cor,hh,slow=0,probth=probth,psym=6.,sym=cs $
		,/fits,/sou_no,sn=slist.sn,s_c=!d.n_colors-1
	device,/close
	;========================================
	set_plot,'x'
;	get the source-removed mask:
        tbs=source_sub_v(tb,nra,ndec,slist.ra,slist.dec,block=block $
;		,fac=subfac,/deg,perc=asfrac,psffile=psffile $
        ,fac=subfac,/deg,sradius=slist.sradius/(block*!size_pixel) $
          ,cra=cra,cdec=cdec)
	sou_fits_reg,hdr,soufile+'_hr.reg',slist,fac=subfacc,perclimit=asfrac
;sou radius scaled by cntr
endif 
print,'sou_final.ps is created!'
;========================================
;===================================================
oldbmap=0
endfor ;for multiple files only
;===================================================

end
