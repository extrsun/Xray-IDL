pro get_ref_median,binref,binval,binvalc,binvalclo,binvalchi,binrefm,binreflo,binrefhi,ncol=ncol,nbin=nbin
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; get_ref_median
;**PURPOSE:
; bin the data according to their reference bin values and calculate
; the median quantities in reference intervals, which has the number bins, and
; dispersion of the bin values in individual intervals (but not the 
; dispersions of the median values)
;**CALLING SEQUENCE:
; get_ref_median,binref,binval,binvalc,binvalclo,binvalchi,binrefm,binreflo
; ,binrefhi,ncol=ncol,nbin=nbin
;
;**PARAMETERS:
;**INPUTS:
; binref - vectors containing reference bin values (e.g., Galatic latitudes) 
; binval - vectors containing bin values for calculating the median quantities
;
;**OPTIONAL INPUTS:
; ncol - number of columns to be used to get reference intervals
;**OUTPUTS:
; binvalc, binvalclo,binvalchi - vector containing the median quantities and
;		68% dispersions of the bin values in corresponding reference
;		intervals
; binrefm, binreflo, binrefhi - vectors containing mean, lower, and upper 
;	values of the reference intervals
; nbin - number of bins per interval
;**PROCEDURE:
; First divide the total reference bins into ncol intervals containing
; same number of bins, then obtain the median quantity and lower and
; upper dispersions  for each individual interval 
;**RESTRICTIONS:
; each interval needs to contain enough bins (e.g., 100)
;**NOTES:
;**SUBROUTINES CALLED:
; no
;**MODIFICATION HISTORY:
; written by WQD, April 17, 1994
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- get_ref_median,binref,binval,binvalc,binvalclo'
print,',binvalchi,binrefm,binreflo,binrefhi,ncol=ncol,nbin=nbin'
return
endif
if n_elements(ncol) eq 0 then ncol=10
nsel=n_elements(binref)
nbin=nsel/ncol
print,'pixel number per interval is: ',nbin
sc=sort(binref)
binref=binref(sc)
binval=binval(sc)

pixlo=lindgen(ncol)*nbin
pixhi=pixlo+(nbin-1)
binreflo=binref(pixlo)
binrefhi=binref((pixhi+1) < (nsel-1))
binrefm=fltarr(ncol)

binvalc=fltarr(ncol)
binvalclo=fltarr(ncol)
binvalchi=fltarr(ncol)
for k=0,ncol-1 do begin
	binrefm(k)=total(binref(pixlo(k):pixhi(k)))/float(nbin)
	data=binval(pixlo(k):pixhi(k))
	avg_median,data,data_m,data_me1,data_me2
;	print,minmax(data)
;	print,data_m,data_me1,data_me2
	binvalc(k)=data_m
	binvalclo(k)=data_me1
	binvalchi(k)=data_me2
endfor
end