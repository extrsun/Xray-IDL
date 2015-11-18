;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; scan_map
;
;*PURPOSE:
; scan sources through an image with an input background map
;
;*CALLING SEQUENCE:
; scan_map,cra,cdec,array_c,array_b,array_t,xoff,yoff,block=block 
;,outr=outr,inr=inr,threshold=threshold,outfile=outfile 
;,ssr=ssr,fac_ssr=fac_ssr,sigma_a=sigma_a,cntr_a=cntr_a,in_sigma=in_sigma 
;,sfrac=sfrac,append=append,ebmap=ebmap,sr=sr,psffile=psffile
; ,blow=blow,bhigh=bhigh,spfile=spfile,expth=expth
;
;*PARAMETERS:
;*INPUTS:
; cra, cdec - the ra and dec of the image center (deg)
; array_c, array_t, and array_b - counts, exposure, and background images
; xoff, yoff - offset of the image center from the axis (aiming point)
;		needed for ACIS observations
; block - the bin size of the image in units of !pixel_size (def=!block)
; inr, outr - the inner and outer radii (in units of arcmin) of the region
; 	(relative to the center) within which 
;	   the program is going to make the source analysis
;	(def = 54 arcmin). Annulus_out is cut to make sure sources within
;	the images are to be analyzed. But the region should be
;	smaller than that of the image.
; threshold - the lower limit of the signal-to-noise ratio of a peak which
;	is considered to be a source candidate (def = !threshold)
; outfile - the output source file name. 
; sfrac - the source energy-enclosed fraction for the source searching for the 
;	local maxima of signal-to-noise ratios. (def = 90% PSF radius)
; sr - input source search radius in pixel ;April 4, 2006
; fac_ssr -  fraction of sr for the source search radius
; append - if set, the output source list will be appended to the existing
;	file (def = 'sou_map'+strtrim(blow,2)+strtrim(bhigh,2))
; in_sigma - if given, input sigma_a will be used for source searching
; ebmap - error map for the background map, supplied with in_sigma
; psffile - psf file to be used (def may be specified in psf_param.pro)
; expth - exposure threshold (units = sec), above which source detection is
;	to be performed (Def = 0.1*max(array_t))
;
;*OUTPUTS:
; sigma_a, cntr_a - the s/var(s) and count rate images which can be
;	reused, for example, for searching sources with a different fac_ssr.
; sprob - log of the false detection probability, used for simulations
;
;*PROCEDURE
; The standard map detection, but with pixel-dependent PSF.
;
;*EXAMPLES:
; scan_map,cra,cdec,cb,cbm*tb,tb,block=block,outr=54
; ,outf='t4.dat',perc=0.5,sigma=s,cntr=c,/in_si 
;
;*RESTRICTIONS:
; The radius for the source analysis should be smaller than the half size
; of the image minus the annulus_out at the radius
;
;*NOTES:
; The source detection parameters are presented in the procedure psf_params.
;
; When the psf is smaller than the bin size, the count rate can be greatly
; overestimated by a factor of up to 1./sfrac.
; 
;*SUBROUTINES CALLED:
; get_sigma_map
; star_search_map
; psf_params
;
;*MODIFICATION HISTORY:
; writen April 21, 1996 (WQD) 
; modified for using psf_params. wqd, 6/2/2001
; include the search based on the poisson probability. wqd, 4/21/2002 
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro scan_map,cra,cdec,array_c,array_b,array_t,xoff,yoff,block=block $
,outr=outr,inr=inr,threshold=threshold,outfile=outfile $
,fac_ssr=fac_ssr,sigma_a=sigma_a,cntr_a=cntr_a,in_sigma=in_sigma $
,sfrac=sfrac,append=append,ebmap=ebmap,sr=sr,psffile=psffile,blow=blow,bhigh=bhigh,spfile=spfile,expth=expth,sprob=sprob,silent=silent,imcoff=imcoff,impsf=impsf
;
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - scan_map,cra,cdec,array_c,array_b,array_t'
print,',xoff,yoff,block=block,outr=outr,inr=inr,threshold=threshold'
print,',outfile=outfile,fac_ssr=fac_ssr,sigma_a=sigma_a'
print,',cntr_a=cntr_a,in_sigma=in_sigma,sfrac=sfrac,append=append,ebmap=ebmap,sr=sr,expth=expth,sprob=sprob,silent=silent,imcoff=imcoff'
return
endif
;
if n_elements(expth) eq 0 then expth = 0.1*max(array_t)
if n_elements(threshold) eq 0 then threshold=2.5
if n_elements(block) eq 0 then block=!block

if n_elements(outr) eq 0 then outr=1.e10  ;the entire field
if n_elements(inr) eq 0 then inr=0. ;arcmin

; get bins to be search for sources
sz=size(array_c)
hdimx=(sz(1)-1.)*0.5 ; IDL image center coodinates
hdimy=(sz(2)-1.)*0.5
loc=lindgen(sz(1),sz(2))
i=loc mod sz(1)
j=loc/sz(1)
dis=sqrt((i-hdimx)^2+(j-hdimy)^2)*(block*!size_pixel/60.)
bin_sel=where(array_t gt expth and dis le outr and dis ge inr,ns)
loc=loc(bin_sel)
if n_elements(impsf) ne 0 then sr=impsf(loc)/float(block) > 1. ;>1 to avoid the pixeling effect

if ns eq 0 then stop,'stop: no source within the radius'

if n_elements(xoff) eq 0 then begin
	if !instr eq 'aciss' or !instr eq 'acisi' then begin
		print,'You need to give xoff and yoff for ACIS observations!' 
		return
	endif 
	dis=dis(bin_sel)
endif else begin
	dis=sqrt((i(bin_sel)-(hdimx-xoff/block))^2 $
	 +(j(bin_sel)-(hdimy-yoff/block))^2)*(block*!size_pixel/60.)
endelse

; get source sizes 
if n_elements(sr) eq 0 then begin
	psf_params,dis,prs,perclimit=sfrac,psffile=psffile,blow=blow,bhigh=bhigh,spfile=spfile
	rs = prs /block > 1. ;>1 to avoid the pixeling effect
endif else rs=dis*0.+sr
;
if keyword_set(in_sigma) eq 0 then $ ;otherwise use input sigma_a and cntr_a
	get_sigma_map, array_c,array_b,array_t,sigma_a,cntr_a, $
	core_size=rs,bin_sel=bin_sel,ebmap=ebmap $
	,core_count=core_count,core_back=core_back,core_time=core_time $
        ,core_bin=core_bin,sfrac=sfrac
;rs and bin_sel can be changed by get_sigma_map
poisig_v,core_back,core_count,prob

if n_elements(fac_ssr) eq 0 then fac_ssr=1.
ssr=fac_ssr*rs > 1. ;at least to include the nearest 4 bins in the search

; find the sources corresponding the local sigma maxima in the field
star_search_map,sigma_a,cntr_a,ssr,ns,x_core,y_core,snr,cntrb $
	,threshold=threshold,bin_sel=bin_sel,prob=prob,s_sel=s_sel

if n_elements(s_sel) eq 0 then return

sprob=prob(s_sel)
core_back=core_back(s_sel)
core_count=core_count(s_sel)
core_time=core_time(s_sel)
core_bin=core_bin(s_sel)
prs=rs(s_sel)*block ;now prs is only for the detected sources

;get position in a standard SASS image
ra_dist = float(x_core) - hdimx
dec_dist =float(y_core) - hdimy

if n_elements(imcoff) ne 0 then begin
    ra_dist=ra_dist+imcoff(0)
    dec_dist=dec_dist+imcoff(1)
endif
trans_loct,ra_dist,dec_dist,cra,cdec,star_ra,star_dec,/deg $
	,pixsize=float(block)*!size_pixel

;convert the RA and DEC into unit of hour, degree:

trans_degree,star_ra,star_dec,ra_hour, $
 ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
dis=sqrt(ra_dist^2+dec_dist^2)*(block*!size_pixel/60.) 
	;off image center distance (arcmin),
	;may be different from the off-axis distance

;record these source into output file:
sn=indgen(ns)+1
if keyword_set(silent) ne 1 then begin
for k=0,(ns-1) do begin
 	print, sn(k), ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k),snr(k),cntrb(k),dis(k),prs(k) $
	,sprob(k),core_back(k),core_count(k) $
	,format='(I4, 2(2i4, f7.2), f9.2, f11.5,5f9.2)'
    endfor
endif
if n_elements(outfile) ne 0 then begin
row={sn:0,ra:0.0D0,dec:0.0D0,snr:0.0,cntrb:0.0,offaxis:0.0,sradius:0.0,count:0,bcount:0.0,expt:0.0,prob:0.0}
slist =replicate(row,ns)
slist.sn=sn
slist.ra=star_ra
slist.dec=star_dec
slist.snr=snr
slist.cntrb=cntrb
slist.offaxis=dis
slist.sradius=prs ;source pixel radius in the band
slist.count=core_count
slist.bcount=core_back*((prs/block)^2*!pi/core_bin) ;scaled to be used in ml_anal
slist.expt=core_time/core_bin ;average exposure
slist.prob=sprob ;-20.
if n_elements(outfile) ne 0 then begin
	if keyword_set(append) eq 0 then sou_struct_fits,slist,outfile $
	 else sou_struct_fits,slist,outfile,/append
endif

endif
return
end
