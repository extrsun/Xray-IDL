pro detect_params,dis,core_size,ann_in,ann_out,fac_c=fac_c $
,blow=blow,bhigh=bhigh,psffile=psffile,gsigma=gsigma,gfrac=gfrac $
,gaus=gaus,acore_size=acore_size,afac_in=afac_in,afac_out=afac_out, $
perclimit=perclimit,afix=afix
;-
; get the on-source and annulus radii based the encircled energy radii of 
; the PSPC PSF or the size of the Gaussian component.
;
; dis - off-axis distances (arcmin) of the sources
; core_size, annulus_in, annulus_out - the output radii (arcmin)
; blow, bhigh - the lower and upper boundaries of the PSPC bands (def = 4, 7)
; psffile -  the file name of the PSF file (including the directory)
; fac_c - the fac_c of the 90% radius of the PSPC PSF
; or the size of the Gaussian component to used as the on-source radius
; gaus - if set, the gaussian size will be used instead of the defaut 90%
;	radius
; gsigma, gfrac - the Gaussian size and count fraction of the PSF 
; acore_size - the vectors containing the PSF as the function of off-axis angle
; afac_in, afac_out - scalar or vector containing the fraction(s) of the
;		PSF energy (def = 0.85, 0.9) for calculating ann_in and 
;		ann_out, or if afix is set, the factors in units of core_size.
; perclimit - scalar or vector containing the fraction(s) of the PSF energy 
; 		(def = 0.85) for calculating core_size
;
; The PSF file can be produced by psf_frac.pro in ~/rosatshell
; 
; reference: Hasinger et al. 1994, Legacy, 4, 40
; writen by wqd, April 16, 1996
; modified to including the interpretaion of encircled energy radii.
; Radius between 80% and 95\% can now be calculated as a function of off axis.
;+
if n_params() eq 0 then begin
print,'detect_params,dis,core_size,ann_in,ann_out,fac_c=fac_c'
print,',blow=blow,bhigh=bhigh,psffile=psffile,gsigma=gsigma,gfrac=gfrac'
print,',acore_size=acore_size,gaus=gaus,afac_in=afac_in,afac_out=afac_out'
print,',perclimit=perclimit'
return
endif
if n_elements(perclimit) eq 0 then perclimit=0.85
if n_elements(blow) eq 0 then blow=4
if n_elements(bhigh) eq 0 then bhigh=7
; need to consider what could happen at off-axis angle = 60 to choose a larger
; value for the factor
if n_elements(psffile) eq 0 then $
	psffile='~/rosatdata/source/'+ $
	'psf_pw_2_0.03_'+strtrim(blow,2)+strtrim(bhigh,2)+'.dat'
	; for source spectrum with a power law of energy slope =1 and
	; an N_H = 3e20
openr,unin,psffile,/get_lun
readf,unin,nv,nl
perc=fltarr(nl)
readf,unin,perc
offang=findgen(nv)
radius=findgen(nv,nl)
agsigma=findgen(nv)
agfrac=findgen(nv)
readf,unin,offang,radius,agsigma,agfrac
free_lun,unin

if n_elements(gaus) ne 0 then begin
	acore_size=(1./60.)*agsigma 
	linterp,offang,acore_size,dis,core_size
endif else begin
	acore_size=radius
	binterp,offang,perc,radius,dis,dis*0.+perclimit,core_size
	; binterp automatically set outliers into the boundary values 
	;	(ie., 90%-99%)
endelse

linterp,offang,agsigma,dis,gsigma
linterp,offang,agfrac,dis,gfrac
if n_elements(fac_c) ne 0. then core_size=fac_c*core_size
if n_elements(afix) ne 0 then begin
	if n_elements(afac_in) eq 0 then afac_in=1.2 ;in units of core_size
	if n_elements(afac_out) eq 0 then afac_out=2. 
	ann_in=afac_in*core_size
	ann_out=afac_out*core_size ;so that ann_out would be too large
endif else begin
	if n_elements(afac_in) eq 0 then afac_in=0.85 
	if n_elements(afac_out) eq 0 then afac_out=0.9 
	binterp,offang,perc,radius,dis,dis*0.+afac_in,ann_in
	binterp,offang,perc,radius,dis,dis*0.+afac_out,ann_out
endelse

if !debug eq 1 then stop
end