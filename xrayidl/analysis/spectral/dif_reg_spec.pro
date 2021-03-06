pro dif_reg_spec,regfile,lhdr,mlist,tstart,mfname,bmlist=bmlist,wemin=wemin,wemax=wemax,ctonth=ctonth,nogroup=nogroup,chlow=chlow,chhigh=chhigh,bspecfname=bspecfname,grouponly=grouponly
;+
; generate spectra (including grouped one as well as rsp files) for a 
; set of diffuse sources with the regions defined in 
; a single region file 
;
; regfile - region file name, created by sou_fits_reg, including
;           background annulus
; lhdr - the header of the event file
; mlist - event list
; tstart - the reference time of the observation used for calculating
;          the arf
; mfname - name of the mask file, used for calculating the backscal
;          parameter
; bmlist - stowed background event list, useful for extracting background
;          spectra (not used yet; don't know how to produce merged
;          stowed event file)
; wemin,wemax - lower and upper energy limits of the weight for the
;               rsp calculation
; ctonth - background-subtracted signal-to-noise ratio used for
;          spectral grouping (used by sp_group). Def=2.5
; nogroup - if set no group will be performed (e.g., for background
;           spectra). Otherwise, the corresponding background spectra
;           should be already available in the directory
; chlow, chhigh - the lower and upper channel numbers of the spectral
;                 range for the grouping. def=22 (0.3 keV), 548 (8
;                 keV) for chandra acis data
; bspecfname - background spectrum file name. If give, will be used 
; grouponly - if set, no spectra are extracted, but the existing
;             spectra as specified in the region file will be grouped,
;             useful for a changed background spectral file, for example.
;
;*OUTPUTS:
; files of ungrouped and grouped spectra of each source, its local
; background spectrum, and rsp file
;
; written by wqd, June 18, 2007
;-
if N_params() eq 0 then begin
  print,'CALLING SEQUENCE - dif_reg_spec,regfile,lhdr,mlist,tstart,mfname'
 print,',bmlist=bmlist,wemin=wemin,wemax=wemax,ctonth=ctonth,nogroup=nogroup'
 print,',chlow=chlow,chhigh=chhigh,bspecfname=bspecfname,grouponly=grouponly'
 return
endif
if n_elements(wemin) eq 0 then wemin=500
if n_elements(wemax) eq 0 then wemax=2000
if n_elements(ctonth) eq 0 then ctonth=2.5
;if n_elements(bmlist) eq 0 then nobacksp=1 else nobacksp=0
if n_elements(bspecfname) eq 0 then bspecfname=sfroot+'_b.pi'
    
openr,refun,regfile,/get
while eof(refun) eq 0 do begin
    filter_reg,filter,shdr,refun=refun,mfname=mfname,sname=sname
    sfroot=sname
   if  keyword_set(grouponly) eq 0 then begin
    sel=where(filter gt 0,nsel)
    if nsel eq 0 then stop,'no none-zero bin in the filter!!!'
    expt=avg(filter(sel)) 
        ;averaging is essential to correct the flux for the badpixel removal
    backscal=nsel*(sxpar(shdr,'cdelt2')*3600./!size_pixel/8192.)^2 
                    ;8192 is for an ACIS spectrum
    ;now get the source spectrum:
    list_image_para,lhdr,shdr,sxmin,symin,sdim,block=block
    list_image,mlist,sxmin,symin,cb,sdim(0),sdim(1),block=block $
      ,filter=filter,sel=selc
if !debug eq 3 then stop
    multi_spec,mlist(selc),sfroot,exp=expt,backscal=backscal,wemin=wemin $
      ,wemax=wemax,tstart=tstart,bspecfname=bspecfname
;    if nobacksp eq 0 then $ ;need appropriate merged stowed data
;    multi_spec,bmlist(selc),sfroot+'_b',exp=expt,backscal=backscal,/norsp   
if !debug eq 3 then stop
   endif
if keyword_set(nogroup) eq 0 then begin
    ;group the spectrum
    if n_elements(ctonth) eq 0 then ctonth=2.5
    if n_elements(chlow) eq 0 then chlow=22 ;0.3 keV
    if n_elements(chhigh) eq 0 then chhigh=548 ;8 keV for ACIS
    sp_group,sfroot+'.pi',bspecfname,fout='group.grp',ctonth=ctonth $
      ,chlow=chlow,chhigh=chhigh
    spawn,'grppha '+sfroot+'.pi '+sfroot+'_g.pi '+'clobber=yes ' $
      +'"comm=group group.grp & show group & exit"'
endif
endwhile
free_lun,refun
return
end
