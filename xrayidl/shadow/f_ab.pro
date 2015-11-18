pro f_ab,nhd,npb,ba,et,nlow,ndelta,nc,xflux,minnorm,bnlo=bnlo, $
  bnhi=bnhi,insel=insel,sel=sel,chibin=chibin,etlow=etlow, $
  nhback=nhback,op_limit=op_limit,quad=quad, $
  opacity,count,back,time,yfit,sel0,opfile=opfile,opband=opband
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; fit_ab
;**PURPOSE:
; A two-component flux model fit to X-ray data to get foreground and 
; background fluxes as well as the normalization of absorption
;**CALLING SEQUENCE:
; fit_ab,nhd,npb,ba,et,nlow,ndelta,nc,xflux,minnorm,bnlo=bnlo $
; ,bnhi=bnhi,insel=insel,sel=sel,chibin=chibin,etlow=etlow $
; ,nhback=nhback,op_limit=op_limit,quad=quad,opfile=opfile
;**PARAMETERS:
;**INPUTS:
; nhd == The column density image (in units of 10^22 cm^-2);
; npb, ba, et == Containing count, background, and exposure images in
;       different energy bands. e.g., npb= array(60,60,3) includes images
;    	in three bands: 0=S, 1=M, and 2=H;
; nlow, ndelta, nc == The lower limit and delta of the abs normalization in the 
;      		nc step calculations.
;**OPTIONAL INPUTS or OUTPUTS:
; bnlo,bnhi == The energy band lower and upper limits, default = 0 and 0;
; sel == Containing the locations of the elements, to be used in the fit
;     if INSEL is set, or as an output;
; etlow == The lower limit of the exposure image value to be selected
;    	default= (16*16)*1.e3, i.e 4 arcmin bin assumed;
; chibin == Output chi square image when INSET is set;
; nhback == The background subtracted from the absorption image before a fit;
; op_limit == The upper limit on opacity of a pixel to be included in the fit, 
;    	default = 6.;
; quad == If set, use quadratic interpolation instead of linear interpolation
;   for getting absorption data;
; opband - if given, will be used for calculating the source subtracting area
;	(band range 0-2). Otherwise the band used to create the images will
;	be used, resulting different subtracting area in different bands
; 	
;**OUTPUTS:
; xflux == Two component X-ray fluxes in individual energy bands for the last
;    	normalization value, array(2,3) in units of (count-back)/et.
; minnorm == The normalization corresponding to the minimum chi square value
;     	of the fit.
;**PROCEDURE:
; First select elements from the images and then use the least square fit
; to obtain the best estimate of the fluxes for each normalization value
;**EXAMPLES:
; fit_ab,ir4,npb,ba,et,0.8,0.2,10,xflux,minnorm,bnlo=0,bnhi=1,sel=sel
;**RESTRICTIONS:
; The absorption is defined for only three energy bands, 0 for low, 1 for
; medium, and 2 for high.
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
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- fit_ab,nhd,npb,ba,et,nlow,ndelta,nc,xflux,minnorm'
print,',bnlo=bnlo,bnhi=bnhi,insel=insel,sel=sel,chibin=chibin,etlow=etlow'
print,',nhback=nhback,op_limit=op_limit,quad=quad,'
print,'opfile=opfile,opband=opband'
return
endif
if n_elements(opfile) eq 0 then opfile='~wqd/rosat/shadow/phoct_dat.normal.'
if n_elements(bnlo) eq 0 then bnlo = 0
if n_elements(bnhi) eq 0 then bnhi = 0
if n_elements(nhback) eq 0. then nhback = 0.
if n_elements(op_limit) eq 0 then op_limit = 6.
if n_elements(etlow) eq 0 then etlow = (16*16)*1.e3
  ;** roughly one 16 cts assuming 4'*4' bin size
if keyword_set(quad) ne 0 then quad = 1 else quad = 0
;nh_sub = nhd-nhback
nh_sub = nhd

wclimit = 10.
ncomp = 2 ;two component fit
norm = nlow
minchi = 1.e22
minnorm = 0.

;** Compute the number of bands program will be calculating:
array_sz = size(et)
n_dim = array_sz(0)
n_size = array_sz(1)

if n_dim eq 2 then n_band = 1 else n_band = array_sz(3)
xflux = fltarr(ncomp,n_band)

;** Do the fits for nc normalization values:
for ni = 1,nc do begin
  if keyword_set(insel) ne 0  then begin
    nsel = n_elements(sel)
    chibin = fltarr(nsel)
    getchibin = 1
  endif else getchibin = 0
  chi = 0.
  ndf = 0.
  for k = bnlo,bnhi do begin
      ;** Obtain opacity due to absorption; Divide by 100. since data
      ;** file has column density data in 10^22 units:
    if n_elements(opband) eq 0 then opk=k else opk=opband
    sh_opac,norm*nh_sub,image_op,k,quad=quad,filter=et $
	,opfile=opfile,/nolog
    if keyword_set(insel) eq 0 then begin
      sel = where(et(*,*,k-bnlo) gt etlow and image_op lt op_limit,nsel) 
      if nsel eq 0 then stop,'No bins with non-zero exposure'
    endif 	
;    opacity = -image_op(sel)
    sel_data,npb,ba,et,sel,k,count,back,time
    weight = 1./(count > wclimit)
    func = fltarr(nsel,ncomp)
      func(*,0) = time
      func(*,1) = time*image_op(sel)
      coef = funcfitw(count,count,weight,func,ncomp,yfit,yband,sigma,var $
        ,comp_sub = back)
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
  endif
  norm = norm + ndelta
endfor
print,'minimum chi ^2  = ',minchi,' with norm = ',minnorm
print,'xflux = ',xflux
sel0 = sel
if !debug eq 1 then stop
end
