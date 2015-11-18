pro get_vec,nhd,npb,ba,et,nhvc,cv=cv,bv=bv,tv=tv,nhmin=nhmin $
,nhmax=nhmax,nhinc=nhinc,flux=flux,eflux=eflux,nhl=nhl,nhu=nhu $
,wclimit=wclimit,gnhvc=gnhvc,gnhd=gnhd
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; get_vec
;**PURPOSE:
; bin the data according to the corresponding column density values
;**CALLING SEQUENCE:nhv
; get_vec,nhd,npb,ba,et,nhvc,cv,bv,tv,nhmin=nhmin 
; ,nhmax=nhmax,nhinc=nhinc,flux=flux,eflux=eflux,gnhvc=gnhvc,gnhd=gnhd
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
; gnhd - galactic column density image
;**OUTPUTS:
; nhvc,cv,bv,tv - vectors containing column density, count, backg,
;		and exposure
; flux,eflux - vectors containing IR column density, count flux and its error
; gnhvc - vector containing column density values constructed in the same way
;	as nhvc
;**PROCEDURE:
; First select elements from the images and then bin the data according to
; the input column density values.
;**EXAMPLES:
; get_vec,ir4,npb,ba,et,cv,bv,tv,nhmin=0.5,nhinc=0.5,flux=flux,eflux=eflux
;**RESTRICTIONS:
; no 
;**NOTES:
;**SUBROUTINES CALLED:
; no
;**MODIFICATION HISTORY:
; written by WQD, Sept 23, 1993
; add the choice of a user supplied column density grounp for
;	lower and upper boundaries. WQD, Sept 30, 1993
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- get_vec,nhd,npb,ba,et,nhvc,cv=cv,bv=bv,tv=tv'
print,',nhmin=nhmin,nhmax=nhmax,nhinc=nhinc,wclimit=wclimit'
print,',flux=flux,eflux=eflux,gnhvc=gnhvc,gnhd=gnhd'
return
endif
;*parameter values used
if n_elements(nhmin) eq 0 then nhmin = 0.
if n_elements(nhmax) eq 0 then nhmax = 100. < max(nhd)
if n_elements(nhinc) eq 0 then nhinc=0.5
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

nhmin=fix(min(nhv))
nhmax=max(nhv)
if n_elements(nhl) ne 0 then ncol=n_elements(nhl) else $
	ncol=fix((nhmax-nhmin)/nhinc)+1
; divide the selected bins into different nh intervals
cv=fltarr(ncol,n_band)
bv=fltarr(ncol,n_band)
tv=fltarr(ncol,n_band)
nhvc=fltarr(ncol)
gnhvc=fltarr(ncol)
kk=0
if n_elements(nhl) eq 0 then nhl=nhmin+findgen(ncol)*nhinc
if n_elements(nhu) eq 0 then nhu=nhl+nhinc
for k=0,ncol-1 do begin
	c=where(nhv ge nhl(k) and nhv lt nhu(k),nc)
	if nc ne 0 then begin
             for band =0, n_band-1 do begin
		cv(kk,band)=total(countv(c,band))
		bv(kk,band)=total(backv(c,band))
		tv(kk,band)=total(timev(c,band))
	     endfor
		nhvc(kk)=total(nhv(c))/nc
		gnhvc(kk)=total(gnhv(c))/nc
		if total(cv(kk,*)) ge wclimit then kk=kk+1
	endif
endfor
ncol=kk
kk=kk-1
cv=cv(0:kk,*)
bv=bv(0:kk,*)
tv=tv(0:kk,*)
nhvc=nhvc(0:kk)
gnhvc=gnhvc(0:kk)
if n_band eq 1 then begin
	flux=(cv-bv)/tv
	eflux=sqrt(cv > bv)/tv
endif
end