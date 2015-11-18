pro fit_ab,nhd,npb,ba,et,nlow,ndelta,nc,minxflux,minnorm, $
  fitband=fitband,insel=insel,sel=sel,chibin=chibin,etlow=etlow, $
  opacity,count,back,time,yfit,opfile=opfile,opband=opband,wclimit=wclimit
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; fit_ab
;**PURPOSE:
; A two-component flux model fit to X-ray data to get foreground and 
; background fluxes as well as the normalization of absorption (if chosen)
;**CALLING SEQUENCE:
; fit_ab,nhd,npb,ba,et,nlow,ndelta,nc,minxflux,minnorm, 
;  fitband=fitband,insel=insel,sel=sel,chibin=chibin,etlow=etlow, 
;  opacity,count,back,time,yfit,opfile=opfile,opband=opband
;**PARAMETERS:
;**INPUTS:
; nhd == The column density image (in units of 10^22 cm^-2);
; npb, ba, et == Containing count, background, and exposure images in
;       different energy bands. e.g., npb= array(60,60,3) includes images
;    	in three bands: 0=S, 1=M, and 2=H;
; nlow, ndelta, nc == The lower limit and delta of the abs normalization in the 
;      		nc step calculations.
;**OPTIONAL INPUTS or OUTPUTS:
; fitband - vector containing bands to be used in the least fit;
; sel == Containing the locations of the elements, to be used in the fit
;     if INSEL is set, or as an output;
; etlow == The lower limit of the exposure image value to be selected
;    	default= (16*16)*1.e3, i.e 4 arcmin bin assumed;
; chibin == Output chi square image when INSET is set;
; opband - if given, will be used for calculating the source subtracting area
;	(band range 0-2). Otherwise the band used to create the images will
;	be used, resulting different subtracting area in different bands
; 	
;**OUTPUTS:
; xflux == Two component X-ray fluxes in individual energy bands for the last
;    	normalization value, array(2,3) in units of (count-back)/et.
; minxflux - minimum xflux values from the fit
; minnorm == The normalization corresponding to the minimum chi square value
;     	of the fit.
;**PROCEDURE:
; First select elements from the images and then use the least square fit
; to obtain the best estimate of the fluxes for each normalization value
;**EXAMPLES:
; fit_ab,ir4*0.01,npb,ba,et,0.8,0.2,10,minxflux,minnorm,fitband=[0],sel=sel
;**RESTRICTIONS:
; The absorption is defined for only three energy bands, 0 for soft, 1 for
; medium, and 2 for hard.
;**NOTES:
;**SUBROUTINES CALLED:
; sel_data
; sh_opac
; funcfitw
;**MODIFICATION HISTORY:
; written by WQD, 4/11, 1993
; Modified by kachun 27 July, 1993; made number of energy bands a parameter;
;   changed opacity subroutine from get_opacity to opac.  Name of program
;   changed from fit_ab.pro to f_ab.pro.
; Modified by wqd and kachun 4 August, 1993; shortened program and made band
;   determination process simpler; added in opfile parameter.
; Modified to use a vector fitband to replace bnlo and bnhi keywords and 
; the name of the program is changed back to fit_ab.pro. wqd, Oact 20, 1993
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- fit_ab,nhd,npb,ba,et,nlow,ndelta,nc,'
print,'minxflux,minnorm,fitband=fitband,insel=insel,sel=sel,chibin=chibin,'
print,'etlow=etlow,opacity,count,back,time,yfit,opfile=opfile,opband=opband'
print,'wclimit=wclimit'
return
endif
;if n_elements(opfile) eq 0 then opfile='~wqd/rosat/shadow/phoct_dat.normal.'
if n_elements(fitband) eq 0 then fitband=[0,1,2]
if n_elements(etlow) eq 0 then etlow = (16*16)*1.e3
  ;** roughly one 16 cts assuming 4'*4' bin size
if keyword_set(quad) ne 0 then quad = 1 else quad = 0
nh_sub = nhd

if n_elements(wclimit) eq 0 then wclimit = 6.
ncomp = 2 ;two component fit
norm = nlow
minchi = 1.e22
minnorm = 0.

;** Compute the number of bands program will be calculating:
array_sz = size(et)
n_dim = array_sz(0)
n_size = array_sz(1)
n_fitband=n_elements(fitband)

if n_dim eq 2 then n_band = 1 else n_band = array_sz(3)
xflux = fltarr(ncomp,n_band)
filter=et(*,*,fitband(0))
;** Do the fits for nc normalization values:
for ni = 1,nc do begin
  if keyword_set(insel) ne 0  then begin
    nsel = n_elements(sel)
    chibin = fltarr(nsel)
    getchibin = 1
  endif else getchibin = 0
  chi = 0.
  ndf = 0.
  for n = 0, n_fitband-1 do begin
    k=fitband(n)
      ;** Obtain opacity due to absorption; Divide by 100. since data
      ;** file has column density data in 10^22 units:
    if n_elements(opband) eq 0 then opk=k else opk=opband
    sh_opac,norm*nh_sub,image_op,k,quad=quad,filter=filter $
	,opfile=opfile,/nolog
    if keyword_set(insel) eq 0 then begin
      sel = where(filter gt etlow,nsel) 
      if nsel eq 0 then stop,'No bins with non-zero exposure'
    endif 	
    sel_data,npb,ba,et,sel,k,count,back,time
    weight = 1./(count > wclimit)
    func = fltarr(nsel,ncomp)
      func(*,0) = time
      func(*,1) = time*image_op(sel)
      coef = funcfitw(count,count,weight,func,ncomp,yfit,yband,sigma,var $
        ,comp_sub = back)
;      weight=1./(yfit > wclimit)
;      coef = funcfitw(count,count,weight,func,ncomp,yfit,yband,sigma,var $
;        ,comp_sub = back)
      for kk = 0,(ncomp-1) do begin
        print,'coef = ', coef(kk),' +- ',sqrt(var(kk,kk))
      endfor
      ndf = ndf+(nsel-ncomp)
      chi = chi+sigma*sigma*(nsel-ncomp)
;     print,chi,(nsel-ncomp)
    if getchibin ne 0  then $
      chibin = chibin+(count-yfit)*(count-yfit)*weight
    ;** (yfit > wclimit)
    if n_band ne 1 then xflux(*,k) = coef else xflux(*) = coef(*)
  endfor

  print,'chi, ndf, norm = ',chi,ndf,norm
    ;** Get the minimum chi square value:
  if chi lt minchi then begin
    minchi = chi
    minnorm = norm
    minxflux=xflux
  endif
  norm = norm + ndelta
endfor
print,'minimum chi ^2  = ',minchi,' with norm = ',minnorm
if nc ne 1 then print,'minxflux = ',minxflux
if !debug eq 1 then stop
end
