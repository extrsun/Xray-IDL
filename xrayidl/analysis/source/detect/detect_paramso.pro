pro detect_params,dis,core_size,ann_in,ann_out,fac_c=fac_c $
,blow=blow,bhigh=bhigh,fname=fname,dir=dir,gsigma=gsigma,gfrac=gfrac $
,gaus=gaus,acore_size=acore_size,afac_in=afac_in,afac_out=afac_out
;-
; get the on-source and annulus radii based the 90% radius of the PSPC PSF
; or the size of the Gaussian component.
;
; dis - off-axis distances (arcmin) of the sources
; core_size, annulus_in, annulus_out - the output radii (arcmin)
; blow, bhigh - the lower and upper boundaries of the PSPC bands (def = 4, 7)
; dir, fname - the directory and file name of the PSF file
; fac_c - the fac_c of the 90% radius of the PSPC PSF
; or the size of the Gaussian component to used as the on-source radius
; gaus - if set, the gaussian size will be used instead of the defaut 90%
;	radius
; gsigma, gfrac - the Gaussian size and count fraction of the PSF 
; acore_size - the vectors containing the PSF as the function of off-axis angle
; afac_in, afac_out - the factors used to for calculating the inner and outer
;		radii of the annulus (in units of core_size)
;
; The PSF file can be produced by psf_frac.pro in ~/rosatshell
; 
; reference: Hasinger et al. 1994, Legacy, 4, 40
; writen by wqd, April 16, 1996
;+
if n_params() eq 0 then begin
print,'detect_params,dis,core_size,ann_in,ann_out,fac_c=fac_c'
print,',blow=blow,bhigh=bhigh,fname=fname,dir=dir,gsigma=gsigma,gfrac=gfrac'
print,',acore_size=acore_size,gaus=gaus,afac_in=afac_in,afac_out=afac_out'
return
endif
if n_elements(blow) eq 0 then blow=4
if n_elements(bhigh) eq 0 then bhigh=7
if n_elements(fac_c) eq 0 then fac_c=1.
; need to consider what could happen at off-axis angle = 60 to choose a larger
; value for the factor
if n_elements(dir) eq 0 then dir='~/rosatdata/source/'
if n_elements(fname) eq 0 then $
	fname='psf_pw_2_0.03_'+strtrim(blow,2)+strtrim(bhigh,2)+'.dat'
	; for source spectrum with a power law of energy slope =1 and
	; an N_H = 3e20
openr,unin,dir+fname,/get_lun
readf,unin,nv
offang=findgen(nv)
radius=findgen(nv)
agsigma=findgen(nv)
agfrac=findgen(nv)
readf,unin,offang,radius,agsigma,agfrac
free_lun,unin

if n_elements(gaus) ne 0 then acore_size=(fac_c/60.)*agsigma else $
	acore_size=fac_c*radius

linterp,offang,acore_size,dis,core_size
linterp,offang,agsigma,dis,gsigma
linterp,offang,agfrac,dis,gfrac
if n_elements(afac_in) eq 0 then afac_in=1.2 ;in units of core_size
if n_elements(afac_out) eq 0 then afac_out=2. ;in units of arcminuts
ann_in=afac_in*core_size
ann_out=afac_out*core_size ;so that ann_out would be too large
;
if !debug eq 1 then stop
end