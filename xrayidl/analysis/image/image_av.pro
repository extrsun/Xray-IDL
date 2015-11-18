;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
;	image_av
;
;*PURPOSE:
;	obtain a smoothed flux image by making arithmetic average of the data
;	including  adjacent bins in count, background, and exposure images.
;
;*CALLING SEQUENCE:
;	image_av,maxbox,npb,ba,et,f,
;	bin_size=bin_size,frac=frac
;
;*PARAMETERS:
; INPUTS:
;	maxbox - one-side dimension of the maximum average box
;	npb,ba,et - count, background, exposure images
;	bin_size - pixel size (before a merge; in units of 0",5)
;	frac - the fraction of the mean flux > local flux used as the
;	criterion of the flux error in the average
;
; OUTPUTS
;	f - flux image (in units of ct / s / arcmin^2)
;
;*PROCEDURE:
;       The box size increases from 1 to maxbox (in regions where sources 
; are subtracted the limitation on maxbox is relaxed). The flux is calculated
; when  the flux error criterion is met. Otherwise, the exposure of that bin
; is set equal to zero
;
;*EXAMPLES:
;	image_av,5,array_c,array_b,array_t,f,frac=0.2
;
;*RESTRICTIONS:
;
;*NOTES:
;
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;	writen 3 AUG 1992 (WQD)
;
;-
;---------------------------------------------------------------------------
pro image_av,maxbox,npb,ba,et,f,etn,bin_size=bin_size,frac=frac,lmin=lmin
;
; maxbox --- the maximum bin number in one side of the average
;------------------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALL SEQUENCE - image_av,maxbox,npb,ba,et,f,etn'
print,'[bin_size,frac,lmin=]'
return
endif
;
if n_elements(lmin) eq 0 then lmin=0
etn=et
sz=size(npb)
xdim=sz(1)
ydim=sz(2)
if n_elements(bin_size) eq 0 then bin_size=!block
spixmin=(0.5*float(bin_size)/60.)^2
;
c=where(et gt 0.)
flux_mean=total(npb(c)-ba(c))/total(et(c))/spixmin
print,'the mean flux = ',flux_mean,' ct/s/arcmin^2'
if n_elements(frac) eq 0 then frac=0.15
print,frac*100.,'\% of the mean flux > the local flux is used'

;
if !debug eq 1 then stop
etm=et*spixmin ; normalized to the units of arcmin^2
f=et*0.
;---------------------------------------------------
for j=0,(ydim-1) do begin & for i=0,(xdim-1) do begin
 if ba(i,j) NE 0. then begin ; exposure at this bin
	Lm=Lmin                     
  repeat begin 
	Lm=Lm+1
;	mkk=(2*Lm+1)*(2*Lm+1)
	mink=(i-Lm) > 0
	maxk=(i+Lm) < (xdim-1)
	minm=(j-Lm) > 0
	maxm=(j+Lm) < (ydim-1)
	etms=etm(mink:maxk,minm:maxm)
	npbs=npb(mink:maxk,minm:maxm)
	bas=ba(mink:maxk,minm:maxm)
	good=where(etms GT 0.,count)
	if count eq 0 then goto, relax
	tc=total(npbs(good))
	tb=total(bas(good))
	tt=total(etms(good))
	eflux=sqrt(tc > 1.0)/tt
	flux=(tc-tb)/tt
;
	if eflux LT frac*(flux_mean > flux)  then begin
	f(i,j)=flux &  goto, back
	endif
;
	if etm(i,j) EQ 0. then begin
relax:	Lmax=maxbox+10
	endif else Lmax=maxbox
;
  endrep until Lm EQ Lmax
	etn(i,j)=0.
 endif
;
back: 
endfor & endfor
if !debug eq 1 then stop
;
return
end

