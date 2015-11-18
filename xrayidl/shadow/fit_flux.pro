pro fit_flux,nhd,npb,ba,et,plow,pdelta,npvalue,xflux,minnorm $
	,bnlo=bnlo,bnhi=bnhi,filter=filter $
	,nhlow=nhlow,nhhigh=nhhigh,nhinc=nhinc $
	,opfile=opfile,nhvc=nhvc,flux=flux,fluxe=fluxe $
	,mflux=mflux,mimage=mimage
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; fit_ab
;**PURPOSE:
; A two-component flux model fit to X-ray data to get foreground and 
; background fluxes as well as the normalization of absorption
;**CALLING SEQUENCE:
; fit_flux,nhd,npb,ba,et,plow,pdelta,npvalue,xflux,minnorm $
;	,bnlo=bnlo,bnhi=bnhi,filter=filter $
;	,nhlow=nhlow,nhhigh=nhhigh,nhinc=nhinc $
;	,opfile=opfile,nhvc=nhvc,flux=flux,fluxe=fluxe $
;	,mflux=mflux,mimage=mimage
;**PARAMETERS:
;**INPUTS:
; nhd == an image with intensity proportional to the X-ray absorbing 
;	column density (in units of 10^20 cm^-2);
; npb, ba, et == images containing count, background, and exposure in
;       different energy bands. e.g., npb= array(60,60,3) includes images
;    	in three bands: 0=S, 1=M, and 2=H;
; plow, pdelta, npvalue == The lower limit and delta of the abs 
;		normalization in the npvalue  step calculations.
;
;**OPTIONAL INPUTS:
; bnlo, bnhi - the lower and upper limits of the bands to be included (def:1)
; filter - an image with negative bins to be excluded from the fit
; nhlow, nhhigh, nhinc - the lower, upper, and increment in dividing 
;		nhd intervals for the fit
; opf - the opacity file containing the broad band opacity as a
;	function of column density
;
;**OPTIONAL INPUTS or OUTPUTS:
; nhvc,flux,fluxe,mflux - vectors containing IR column density, count flux,
; its error, and model flux
; mimage - the model image
;**OUTPUTS:
; xflux == Two component X-ray fluxes in individual energy bands for the last
;    	normalization value, array(2,3) in units of (count-back)/et.
; minnorm == The normalization corresponding to the minimum chi square value
;     	of the fit.
;**PROCEDURE:
; First select elements from the images and then bin the data according to
; the input column density values, and finally use the least square fit
; to obtain the best estimate of the fluxes for each normalization value
;**EXAMPLES:
; fit_ab,ir4,npb,ba,et,0.8,0.2,10,xflux,minnorm,bnlo=0,bnhi=1,mi=mimage
;**RESTRICTIONS:
; The absorption is defined for only three energy bands, 0 for low, 1 for
; medium, and 2 for high.
;**NOTES:
;**SUBROUTINES CALLED:
; sh_opac
; funcfitw
;**MODIFICATION HISTORY:
; written by WQD, Aug 4, 1993
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- fit_flux,nhd,npb,ba,et,plow,pdelta,npvalue,xflux,'
print,'minnorm,bnlo=bnlo,bnhi=bnhi,filter=filter'
print,',nhlow=nhlow,nhhigh=nhhigh,nhinc=nhinc'
print,',opfile=opfile,nhvc=nhvc,flux=flux,fluxe=fluxe'
print,',mflux=mflux,mimage=mimage'
return
endif

if n_elements(bnlo) eq 0 then bnlo = 0
if n_elements(bnhi) eq 0 then bnhi = 0
if n_elements(nhback) eq 0. then nhback = 0.
if n_elements(nhlow) eq 0 then nhlow = 0.
if n_elements(nhhigh) eq 0 then nhhigh = 100. < max(nhd)
if n_elements(nhinc) eq 0 then nhinc=0.002
if n_elements(expmin) eq 0 then expmin = 1.e3
if n_elements(filter) eq 0 then filter =et(*,*,0)
if keyword_set(quad) ne 0 then quad = 1 else quad = 0
wclimit = 10.
ncomp = 2 ;two component fit
minchi = 1.e22

;** Compute the number of bands program will be calculating:
array_sz = size(et)
n_dim = array_sz(0)
if n_dim eq 2 then n_band = 1 else n_band = array_sz(3)
xflux = fltarr(ncomp,n_band)

; select the bins in the images
sel=where(nhd ge nhlow and nhd le nhhigh and filter gt expmin,nsel)
if nsel eq 0 then stop,'stop in f_ab because no bin is selected.
nhv=nhd(sel)/100. ; to be used in units of 10^22 cm^{-2}
countv=fltarr(nsel,n_band)
backv=fltarr(nsel,n_band)
timev=fltarr(nsel,n_band)
for k=0,n_band-1 do begin
	aa=npb(*,*,k)
	countv(*,k)=aa(sel)
	aa=ba(*,*,k)
	backv(*,k)=aa(sel)
	aa=et(*,*,k)
	timev(*,k)=aa(sel)
endfor

nhmin=fix(min(nhv))
nhmax=max(nhv)
ncol=fix((nhmax-nhmin)/nhinc)+1

; divide the selected bins into different nh intervals
countvc=fltarr(ncol,n_band)
backvc=fltarr(ncol,n_band)
timevc=fltarr(ncol,n_band)
nhvc=fltarr(ncol)
kk=0
for k=0,ncol-1 do begin
	nhl=nhmin+k*nhinc
	nhu=nhl+nhinc
	c=where(nhv ge nhl and nhv lt nhu,nc)
	if nc ne 0 then begin
             for band =0, n_band-1 do begin
		countvc(kk,band)=total(countv(c,band))
		backvc(kk,band)=total(backv(c,band))
		timevc(kk,band)=total(timev(c,band))
	     endfor
		nhvc(kk)=total(nhv(c))/nc
		if total(countvc(kk,*)) gt 10. then 	kk=kk+1
		; 10 is an arbitrary number to exclude bins with too few counts
	endif
endfor
ncol=kk
kk=kk-1
countvc=countvc(0:kk,*)
backvc=backvc(0:kk,*)
timevc=timevc(0:kk,*)
nhvc=nhvc(0:kk)
;
op=fltarr(ncol,n_band)
for band=bnlo,bnhi do begin
    	sh_opac,nhvc,opacity,band,quad=quad,opfile=opfile
	op(*,band-bnlo)=opacity
endfor

;** Do the fits for nc normalization values:
func = fltarr(ncol,ncomp)
coefb=fltarr(ncomp,n_band)
ecoefb=fltarr(ncomp,n_band)
for ni = 1,npvalue do begin

  chi = 0.
  ndf = 0.
  norm=plow+(ni-1)*pdelta

  for k=0,n_band-1 do begin
      func(*,0) = timevc(*,k)
      func(*,1) = timevc(*,k)*exp(norm*op(*,k))
      w = 1./(countvc(*,k) > wclimit)
      coef = funcfitw(countvc(*,k),countvc(*,k),w,func,ncomp,yfit,yband, $
	sigma,var,comp_sub = backvc(*,k))
      coefb(*,k)=coef
      for nc=0,ncomp-1 do begin
	ecoefb(*,k)=sqrt(var(nc,nc))
	print,'coef = ',coefb(nc,k), '+/- ',ecoefb(nc,k)
      endfor
      ndf = ndf+(ncol-ncomp)
      chi = chi+sigma*sigma*(ncol-ncomp)
  endfor
  print,'chi, ndf, norm = ',chi,ndf,norm
  ;** Get the minimum chi square value:
  if chi lt minchi then begin
    minchi = chi
    minnorm = norm
    xflux = coefb
    xfluxe=ecoefb
  endif
endfor
print,'minimum chi ^2  = ',minchi,' with norm = ',minnorm
print,'xflux = ',xflux
if n_band eq 1 then begin
	flux=(countvc-backvc)/timevc
	fluxe=sqrt(countvc-backvc)/timevc
	mflux=xflux(0)+xflux(1)*exp(minnorm*op)
	;get the model image
	sh_opac,(minnorm*nhd)*0.01,image_op,bnlo,quad=0,opfile=opfile
	mimage=xflux(0)+xflux(1)*exp(image_op)
	c=where(filter le 0.,nc)
	if nc ne 0 then mimage(c)=0.
endif
stop
if !debug eq 1 then stop
end
