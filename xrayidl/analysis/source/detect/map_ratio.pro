;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro map_ratio,slist,elist,cra,cdec,bmin,bmax,image_t,image_b,tblock, $
nra=nra,ndec=ndec,radius=radius,outfile=outfile,sfrac=sfrac,rc=rc $
,infile=infile,probth=probth,slow=slow,fac_ssr=fac_ssr,offaxis=offaxis $
,rfac=rfac,sel=sel,count=count,back=back,exptv=exptv,rs=rs,noadd=noadd $
,cntrat=cntrat,cntreat=cntreat,bv=bv,aimpsf=aimpsf,detagname=detagname,no_update=no_update,no_print=no_print,minrs=minrs
;+
; Calaculate count rates in individual bands with 
; background and exposure maps and a photon list
;
; slist - source structure (if infile is provided, slist is the output)
; elist - photon list containing counts of all on-source and background regions
; cra, cdec - the center of the observation
; bmin, bmax - vectors containing lower and upper limits of the energy bands
;		in units of eV for Chandra data
; image_t, image_b - stacked exposure and background COUNT images in
; 		individual bands.
;		The two images must have the SAME size.
; tblock - the block size of the images
; nra, ndec - Ra nd Dec of the image center (which may be different from 
;		the axis center)
; radius - within which sources are to be analyzed (arcmin, def = infinite)
; outfile - output file name (def = infile+'ratio')
; sfrac - fraction of counts in the on-source aperture 
;		(def = 0.9) used in get_snr.pro
; rc - the size of the aperture for collecting the counts used in the fit.
; 	if set, overruling the 90% radius and dfac.
;
; infile - input file name. If given, the source list will be read from the
;		file, overriding the input one.
; probth -  upper limit of the probability threshold for the source selection
; slow - the Signal-to-noise threshold for selecting sources (def = 0).
; rfac - factor of the source PSF radius to be used for source count
;       and background count calculation; useful for extented sources,
;       together with fac_ssr.
; count, back, exptv,cntrat, cntreat - count, background count,
;                                      exposure, total count rate and
;                                      its error
; rs - source radius used
; noadd - if set, the resultant count rates are not added to the list
; no_update - if set, no update to cntr, count, bcount, expt, snr
; no_print - if set, no printing 
;
; writen by wqd, april 24, 1996
; modified to include the adjustment of the cntr,prob,count,bcount,
; etc. in the structure before the output, wqd, April 30,, 2005
;
;-
if n_params() eq 0 then begin
print,'Calling procedure - map_ratio,slist,elist,cra,cdec,bmin,bmax'
print,',image_t,image_b,tblock,nra=nra,ndec=ndec,radius=radius'
print,',outfile=outfile,sfrac=sfrac,rc=rc,infile=infile,probth=probth,slow=slow ,fac_ssr=fac_ssr,offaxis=offaxis,rfac=rfac,sel=sel,count=count,back=back,exptv=exptv,rs=rs,noadd=noadd,cntrat=cntrat,cntreat=cntreat,bv=bv,aimpsf=aimpsf,detagname=detagname,no_update=no_update,no_print=no_print'
return
endif
;
if n_elements(tblock) eq 0 then tblock=!block
if n_elements(radius) eq 0 then radius=1.e22 ;arcmin
if n_elements(sfrac) eq 0 then sfrac=0.9
if n_elements(minrs) eq 0 then minrs=0.
if n_elements(nra) eq 0 then begin
	nra=cra & ndec=cdec
endif
;
;------------------------------------------------------------
if n_elements(infile) ne 0 then begin
	if n_elements(slow) eq 0 then slow=0.
	; read the sources from the input source file
	sou_fits_info,infile,slist,slow=slow,flow=flow,probth=probth,/all
	; otherwise use the input source list
endif 

trans_dist,nra,ndec,slist.ra,slist.dec,xp,yp,/deg,pixsize=!size_pixel

if N_elements(offaxis) eq 0 then offaxis=slist.offaxis
;if n_elements(rfac) eq 0 then srfac=1. else srfac=rfac^2 
if n_elements(rfac) eq 0 then rfac=1. 
image_t2=image_t(*,*,0)
sz=size(image_t2)
; get source pixel locations in the image:
loc=long(yp/tblock+sz(2)*0.5)*sz(1)+long(xp/tblock+sz(1)*0.5)

;sel=where(offaxis le radius and abs(xp) lt (sz(1)*0.5*tblock) $
;	and abs(yp) lt (sz(2)*0.5*tblock) and image_t2(loc) gt 0., ns)
sel=where(offaxis le radius and abs(xp) lt (sz(1)*0.5*tblock) $
	and abs(yp) lt (sz(2)*0.5*tblock), ns)
if ns eq 0 then stop,'no source within the radius!' else $
msel,sel,slist,xp,yp,offaxis,loc	

;pixel positions of sources:
trans_dist,cra,cdec,slist.ra,slist.dec,xp,yp,/deg,pixsize=!size_pixel
xo=!pref+xp
yo=!pref+yp

xp=elist.x & yp=elist.y

nimpsf=n_elements(aimpsf)
if nimpsf ne 0 then begin
    imsel,loc,aimpsf,rc ;sel values in aimpsf and put them into rc
    rc=rc*rfac
    map_impsf,impsf,aimpsf ;,bv=bv,tbw=tbw ;this changes with bv
    bsr=impsf(loc)*rfac ;broad band source radius
    if n_elements(detagname) ne 0 then begin
        tagin=tag_names(elist)
        match,tagin,strupcase(detagname+'x'),xtagn
        match,tagin,strupcase(detagname+'y'),ytagn
        oxp=elist.(xtagn(0)) & oyp=elist.(ytagn(0)) ;xragn has to be scalar
    endif else begin
        oxp=xp & oyp=yp
    endelse 
    dis=sqrt((oxp-!pref)^2+(oyp-!pref)^2)*(!size_pixel/60.)
endif else begin
    psf_params,offaxis,rs,perclimit=sfrac,blow=bmin(0),bhigh=max(bmax)
    bsr=rs*rfac 
endelse
if !debug eq 3 then stop
bsr=bsr > minrs
nrc=n_elements(rc)

nb=n_elements(bmin)
cntra=fltarr(ns,nb)
cntrea=cntra
csv=fltarr(ns)
count=lonarr(ns,nb)
back=fltarr(ns,nb)
exptv=fltarr(ns,nb)

for n=0,nb-1 do begin ;loop over all bands
        csel=where(elist.energy ge bmin(n) and elist.energy lt bmax(n))
	if nrc ne 0 then begin
            if nimpsf ne 0 then begin
                rs=csv*0.+rc(*,n) ;mean PSF radius in backg and exp images
                psf_params,dis(csel),crs,perclimit=sfrac $
                       ,blow=bmin(n),bhigh=bmax(n) ;PSF radius for each count of individual obs
                scrs=(crs > 1.)^2
            endif else rs=csv*0.+rc
        endif else $
          psf_params,offaxis,rs,perclimit=sfrac,blow=bmin(n),bhigh=bmax(n)
	srs=(rs*rfac > minrs)^2 > 1.
	nbin_s=srs*!pi
        core_size=sqrt(srs)/float(tblock)
        map_extract,core_size,image_t(*,*,n),image_b(*,*,n),bin_sel=loc $
          ,core_2=bc,core_1=expt,core_bin=core_bin
        expt=expt/core_bin
        bc=bc*(nbin_s/tblock^2/core_bin)
	for k=0L,(ns-1) do begin
               if  nimpsf ne 0 then csv(k)=total(float(((xp(csel)-xo(k))^2 $
                  +(yp(csel)-yo(k))^2) le scrs)) else $
                csv(k)=total(float(((xp(csel)-xo(k))^2 $
                  +(yp(csel)-yo(k))^2) le srs(k)))
        endfor 
	get_snr,csv,nbin_s,bc,nbin_s,csn,snr,sfrac=sfrac $
                ,ecsn=ecsn,/bmap,fac_ssr=fac_ssr
		;nbin_s is not used
	cntra(*,n)=csn/expt
	cntrea(*,n)=ecsn/expt
        count(*,n)=csv
        back(*,n)=bc
        exptv(*,n)=expt
;        print,csv,csn,ecsn,bc,expt
;if !debug eq 3 then stop
    endfor
if nb eq 1 then cntrat=cntra else cntrat=total(cntra,2)
if nb eq 1 then cntreat=cntrea else cntreat=sqrt(total(cntrea^2,2))
;because the exposure can be substantially different in different bands,
;cntreat can significntly differ sqrt(count)/expt
;sr=sqrt(srs)
if keyword_set(noadd) ne 0 then return
if !debug eq 3 then stop
;output the results:
;slist.sradius=sr
struct_col_add,slist,reform([cntrat,cntreat],ns,2),['CNTRB','CNTRBE'],0.0*[1,1],slistn
;struct_col_add,slist,reform([cntrat,cntreat,sr],ns,3),['CNTRB','CNTRBE','SRADIUS'],0.0*[1,1,1],slistn
;struct_col_add,slistn,cntra,['CNTRB1','CNTRB2','CNTRB3'],0.0*[1,1,1],slist
;struct_col_add,slist,cntrea,['CNTRB1E','CNTRB2E','CNTRB3E'],0.0*[1,1,1],slistn
struct_col_add,slistn,cntra,replicate('CNTRB',nb)+strtrim(indgen(nb)+1,2),replicate(0.0,nb),slist
struct_col_add,slist,cntrea,replicate('CNTRBE',nb)+strtrim(indgen(nb)+1,2),replicate(0.0,nb),slistn
rs=bsr
help,back(*,[bv-1]),/struct
print,bv
if nb gt 1 then begin
    back=total(back(*,[bv-1]),2)
    count=total(count(*,[bv-1]),2)
endif else begin
    back=back(*,bv-1)
    count=count(*,bv-1)
endelse
poisig_v,back,count,prob
slist=slistn
slist.sradius=bsr
slist.prob=prob ;used for obj_cntr
if !debug eq 3 then stop
;---------------------------------
if keyword_set(no_update) eq 0 then begin
expt=(count-back)/total(cntra(*,bv-1),2)/sfrac ;effective exp that reproduce cntra
slist.cntr=(count-back)/expt/sfrac
slist.count=count
slist.bcount=back
slist.expt=expt
slist.snr=(count-back)/sqrt(count > back)
s=where(prob gt probth or count lt (2.5+sqrt(back*10.)),nsr) ;Eq. 7 of Wang04
if nsr ne 0 then remove,s,slist
ns=ns-nsr
endif 
;-----------------------------------
if keyword_set(no_print) eq 0 then begin
    sou_struct_out,slist,text
    print,tag_names(slist)
    for k=0,(ns-1) do print,text(k)
endif 
if n_elements(outfile) eq 0 then return
sou_struct_fits,slist,outfile
if !debug eq 3 then stop
return
end
