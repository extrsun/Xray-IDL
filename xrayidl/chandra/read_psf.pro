pro read_psf,psffile,frac,psf2,ev,thetav,rv,instr=instr,psf=psf
;+
;
; read the psf table from psfsize_20000830.fits
;
; psffile - input psf file
; frac - vector containing energy-encircled fractions
; psf2 - array contains the off-source radius as a function of off-axis angle,
; 	energy, and energy-encircled fraction
; ev - y-axis, energy in units of keV
; thetav - x-axis, off-axis angle in units of arcmin
; psf - orginal array contains the energy-encircled fraction
; rv - z-axis, off-source angle (arcsec) in psf
; 
;*Restriction:
; using the psf file psfsize_20000830.fits
; 
; written by wqd, 6/6/2001
;-
;
;----------------------------
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - read_psf,psffile,frac,psf2,ev,thetav,rv,instr=instr,psf=psf'
return
endif
if n_elements(instr) eq 0 then instr=!instr
instrv=['acisi', 'aciss', 'hrci', 'hrcs']
ins=where(instr eq instrv,nins)
if nins ne 1 then begin
	print,'needs one of the instruments: ',instrv
	return
endif else begin
	if nins eq 0 then begin
		print,'sorry that the psf is not implemented here'
		return
	endif 
endelse 
;----------------------------------------------
;the extension structure of the available psf table
;containing extensions for [acisi, aciss, hrci, hrcs] and 
;the last one as the start table for the fraction extensions

case nins of
	0: ext=[2,3,4,5,6] 
	1: ext=[2,3,4,5,51]
	2: ext=[2,3,4,5,96]
	3: ext=[2,3,4,5,141]
endcase
nev=5 ;number of energy bins in the table
; The above information needs to be supplied if a new psf table is used
;----------------------------------------------------
if !debug eq 1 then stop
iext=n_elements(ext)
extn=ext(ins)
tab=readfits(psffile,hdr,ext=extn(0))
ev=fits_get(hdr,tab,'energy',indgen(nev))
table=fits_get(hdr,tab,'table')
nt=n_elements(table)
ntheta=nt/nev
;thetav=fits_get(hdr,tab,'theta',indgen(ntheta)*nev)
;the av value in the above header is apparently not correct, whereas the
; values in headers of individual files seem to be consistent with the
; figure in the manual. But the lack of the energy dependence at large
; off axis angles in this file is apparently wrong, compared to the figure
thetav=[0.,1.41,2.83,4.24,7.07,9.90,14.14,21.21,28.28]
extn=ext(iext-1)
tab=readfits(psffile,hdr,ext=extn(0))
nr=sxpar(hdr,'naxis2')
rv=fits_get(hdr,tab,'radius') ;radii are the same for all tables
psf=fltarr(nt,nr)
tloc=lindgen(nt)
for k=0,nt-1 do begin
	extn=tloc(k)+ext(iext-1)
	tab=readfits(psffile,hdr,ext=extn(0))
	psf(k,*)=fits_get(hdr,tab,'fraction')
endfor
psf=reform(psf,ntheta,nev,nr)

nfrac=n_elements(frac)
if nfrac ne 0 then begin
sz=size(psf)
psf2=fltarr(sz(1),sz(2),nfrac)
afrac=fltarr(sz(3))
for k=0,sz(1)-1 do begin
 for m=0,sz(2)-1 do begin
	afrac(*)=psf(k,m,*)
	for n=0,nfrac-1 do begin
		linterp,afrac,rv,frac(n),rf
		psf2(k,m,n)=rf
	endfor
 endfor
endfor
endif
return
end