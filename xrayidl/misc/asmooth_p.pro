pro asmooth_p,infile,smap,outfile=outfile,perclimit=perclimit,filter=filter
;+
; calculate a smoothing scale map based on the off-axis dependent PSF
;
;*INPUTS:
; infile - input scale file name from a csmooth smoothing, used as a reference
;          
;*OUTPUTS:
; smap - calculated smoothing scale map
;
;*OPTIONAL Inputs:
; filter - map of the same size: only pixels > 0 will be calculated,
;          while others will be assigned the value max(the original
;          scale map)
; perclimit - PSF EER fraction (def=0.7) used for calculating the scale.
; outfile - output scale map file name
;
; written by wqd 8/4/2007
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - asmooth_p,infile,smap,outfile=outfile,perclimit=perclimit,filter=filter'
return
endif
osmap=readfits(infile,inhead)
crpix=sxpar(inhead,'crpix*')
pixsize=sxpar(inhead,'cdelt2')
mdim=sxpar(inhead,'naxis*')
dist_circle,dd,mdim,(crpix(0)-1.),(crpix(1)-1.)
dd=dd*(pixsize*60.) ; in units of arcmin
if n_elements(perclimit) eq 0 then perclimit=0.7
  psf_params,dd,rs,perclimit=perclimit 
 ;,psffile=psffile,blow=bmin(eband-1),bhigh=bmax(eband-1),spfile=spfile
smap=osmap
smap=rs

msmap=max(osmap)
if n_elements(filter) ne 0 then sel=where(filter le 0.,nsel) else $
  sel=where(osmap eq msmap,nsel)
if nsel ne 0 then smap(sel)=msmap

if n_elements(outfile) eq 0 then outfile=infile+'_p'
writefits,outfile,smap,inhead
if !debug eq 3 then stop
return
end
