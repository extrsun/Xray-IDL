pro get_vec_area,nhd,npb,ba,et,nhvc,countvc,backvc,timevc,nhmin=nhmin $
,nhmax=nhmax,ncol=ncol,flux=flux,eflux=eflux,nhl=nhl,nhu=nhu,gnhvc=gnhvc,gnhd=gnhd
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
if n_elements(nhmin) eq 0 then nhmin = 0.
if n_elements(nhmax) eq 0 then nhmax = 1000. < max(nhd)
filter=et(*,*,0)
;** Compute the number of bands program will be calculating:
if n_elements(wclimit) eq 0 then wclimit=10
array_sz = size(et)
n_dim = array_sz(0)
if n_dim eq 2 then n_band = 1 else n_band = array_sz(3)

; select the bins in the images
sel=where(nhd ge nhmin and nhd le nhmax and filter gt 0.,nsel)
if nsel eq 0 then stop,'stop in f_ab because no bin is selected.

nhv=nhd(sel) ; to be used in units of 10^20 cm^{-2}
if n_elements(gnhd) eq 0 then gnhd=nhd*0.
gnhv=gnhd(sel)
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

if n_elements(ncol) eq 0 then ncol=25

sc=sort(nhv)
nhv=nhv(sc)
gnhv=gnhv(sc)
countv=countv(sc,*)
backv=backv(sc,*)
timev=timev(sc,*)

if nhmin lt 0. then begin
	c=where(nhv le 0.,nc)
	negcol=1
endif else begin
	negcol=0
	nc=0
endelse
npix=(nsel-nc)/double(ncol-negcol)
pixlo=dindgen(ncol-negcol)*npix+nc
pixhi=long(pixlo+npix) < nsel-1
pixlo=long(pixlo)
if nhmin lt 0. then begin
	pixlo=[0,pixlo]
	pixhi=[nc-1,pixhi]
endif
pixhi(ncol-1)=nsel-1
nhl=nhv(pixlo)
nhu=nhv(pixhi+1)
nhu(ncol-1)=nhv(nsel-1)

countvc=fltarr(ncol,n_band)
backvc=fltarr(ncol,n_band)
timevc=fltarr(ncol,n_band)
nhvc=fltarr(ncol)
gnhvc=fltarr(ncol)

for k=0,ncol-1 do begin
             for band =0, n_band-1 do begin
		countvc(k,band)=total(countv(pixlo(k):pixhi(k),band))
		backvc(k,band)=total(backv(pixlo(k):pixhi(k),band))
		timevc(k,band)=total(timev(pixlo(k):pixhi(k),band))
	     endfor
		nhvc(k)=total(nhv(pixlo(k):pixhi(k)))/(pixhi(k)-pixlo(k)+1)
		gnhvc(k)=total(gnhv(pixlo(k):pixhi(k)))/(pixhi(k)-pixlo(k)+1)
endfor
flux=(countvc-backvc)/timevc
eflux=sqrt(countvc > backvc)/timevc
if nhmin lt 0. then nhvc(0)=0.
end