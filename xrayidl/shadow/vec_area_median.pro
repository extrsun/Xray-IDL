pro vec_area_median,nhd,npb,ba,et,nhvc,countvc,backvc,timevc,nhmin=nhmin $
,nhmax=nhmax,ncol=ncol,flux=flux,eflux=eflux,nhl=nhl,nhu=nhu,gnhvc=gnhvc,gnhd=gnhd,siglevel=siglevel,flo=flo,fhi=fhi
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; get_vec
;**PURPOSE:
; bin the data according to the corresponding column density values
;**CALLING SEQUENCE:
; get_vec,nhd,npb,ba,et,nhvc,countvc,backvc,timevc,nhmin=nhmin 
; ,nhmax=nhmax,nhinc=nhinc,flux=flux,eflux=eflux
;
;**PARAMETERS:
;**INPUTS:
; nhd - an image with intensity proportional to the X-ray absorbing 
;	column density (in units of 10^20 cm^-2);
; npb, ba, et == images containing count, background, and exposure in
;       different energy bands. e.g., npb= array(60,60,3) includes images
;    	in three bands: 0=S, 1=M, and 2=H;
;
;**OPTIONAL INPUTS:
; nhmin, nhmax, nhinc - the lower, upper, and increment in dividing 
;		nhd intervals for the fit
; nhl, nhu - the group containing the lower and upper boundaries for individual
;		column density intervals
; wclimit - the count limit per column density bin to be considered as useful.
; gnhd - diffuse (background or foreground, eg. galactic) column density image
;**OUTPUTS:
; nhvc,countvc,backvc,timevc - vectors containing column density, count, backg,
;		and exposure
; flux,eflux - vectors containing IR column density, count flux and its error
; gnhvc - vector containing diffuse (background or foreground)
;	column density values constructed in the same way as nhvc
;
;**PROCEDURE:
; First select elements from the images and then bin the data according to
; the input column density values.
;**EXAMPLES:
; get_vec,ir4,npb,ba,et,cv,bv,tv,nhmin=0.5,flux=flux,eflux=eflux,ncol=10,nhlo=1
; nhhi=1
;**RESTRICTIONS:
; no 
;**NOTES:
;**SUBROUTINES CALLED:
; no
;**MODIFICATION HISTORY:
; written by WQD, Sept 23, 1993
; add the choice of a user supplied column density grounp for
;	lower and upper boundaries. WQD, Sept 30, 1993
; modification to calculate the vectors based on rough equal sky areas
; in each elements and to include a Galactic absorption component which
; may be used differently (e.g., with different metallicity) in fit_nlvc.
; wqd, Jan 1994.
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- get_vec_area,nhd,npb,ba,et,nhvc,countvc,backvc'
print,',timevc,nhmin=nhmin,nhmax=nhmax,ncol=ncol,flux=flux,eflux=eflux'
print,',nhl=nhl,nhu=nhu,gnhvc=gnhvc,gnhd=gnhd'
return
endif
;*parameter values used
if n_elements(siglevel) eq 0 then siglevel=0.16
if n_elements(nhmin) eq 0 then nhmin = 0. >min(nhd)
if n_elements(nhmax) eq 0 then nhmax = 1000. < max(nhd)

; select the bins in the images
sel=where(nhd ge nhmin and nhd le nhmax and et gt 0.,nsel)
if nsel eq 0 then stop,'stop in f_ab because no bin is selected.

nhv=nhd(sel) ; to be used in units of 10^20 cm^{-2}
if n_elements(gnhd) eq 0 then gnhd=nhd*0.
gnhv=gnhd(sel)
fluxa=imdiv((npb-ba),et)
fluxa=fluxa(sel)

if n_elements(ncol) eq 0 then ncol=25

sc=sort(nhv)
nhv=nhv(sc)
gnhv=gnhv(sc)
fluxa=fluxa(sc)

if nhmin lt 0. then begin
	c=where(nhv le 0.,nc)
	negcol=1
endif else begin
	negcol=0
	nc=0
endelse
npix=(nsel-nc)/(ncol-negcol)
pixlo=lindgen(ncol-negcol)*npix+nc
pixhi=pixlo+npix-1

if nhmin lt 0. then begin
	pixlo=[0,pixlo]
	pixhi=[nc-1,pixhi]
endif
pixhi(ncol-1)=nsel-1

nhl=nhv(pixlo)
nhu=nhv(pixhi+1)
nhu(ncol-1)=nhv(nsel-1)

flux=fltarr(ncol)
flo=fltarr(ncol)
fhi=fltarr(ncol)
nhvc=fltarr(ncol)
gnhvc=fltarr(ncol)

for k=0,ncol-1 do begin
	avg_median,fluxa(pixlo(k):pixhi(k)),fm,f1,f2,siglevel=siglevel
	flux(k)=fm
	flo(k)=f1
	fhi(k)=f2
	nhvc(k)=total(nhv(pixlo(k):pixhi(k)))/(pixhi(k)-pixlo(k)+1)
	gnhvc(k)=total(gnhv(pixlo(k):pixhi(k)))/(pixhi(k)-pixlo(k)+1)
endfor
eflux=(fhi-flo)*0.5
if nhmin lt 0. then nhvc(0)=0.
end