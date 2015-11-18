;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   limit_mex
;
;*PURPOSE:
; A procedure to derive the correction factors for a mean exposure map,
; and set a maximum threshold to the exposure correction
;
;*CALLING SEQUENCE:
;   limit_mex,mex,time,thresh,corrfacs
;
;*PARAMETERS:
; INPUTS:
;   MEX      Mean exposure map (in seconds), e.g., as calculated by MAKE_EMAP
;   TIME     Total exposure time (in seconds)
;   THRESH   Minimum threshold for mean exposure map, as a fraction of the
;            total exposure time 
;
; OUTPUTS:
;   CORRFACS 2d array of exposure correction factors
;            the corrected image = image * corrfacs
;
;*EXAMPLES: 
;        IDL> limit_mex,mex,time,thresh,corrfacs
;
;*RESTRICTIONS:
;
;*NOTES:
;  An empirical good value for THRESH is 0.05.
;
;*SUBROUTINES CALLED:
; DIST_CIRCLE
;
;*MODIFICATION HISTORY:
;    written 2 Nov 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro limit_mex,mex,time,thresh,corrfacs
;
; Correction factors: corrimg = image*corrfacs
;
npar=n_params(0)
if (npar eq 0) then begin
  print,' LIMIT_MEX, mex, time, thresh, CORRFACS'
  retall
endif
;
izero = where(mex le thresh*time,nzero) 
if (nzero le 0) then begin
  arrtemp = mex
endif else begin
  maskmap = mex
  maskmap(izero) = 0
  izero = 0
  izero = where(maskmap gt thresh*time,nzero) 
  if (nzero le 0) then begin
    arrtemp = mex*0. + thresh*time
  endif else begin
    maskmap(izero) = 1
    dist_circle,arrtemp,512,255.5,255.5 
    radlim = max(maskmap*arrtemp) 
    if (!debug gt 3) then print,' Limiting radius = ',radlim
    maskmap = 0
;
    izero = 0
    izero = where(arrtemp lt radlim,nzero) 
    if (nzero gt 0) then arrtemp(izero) = 1
    izero = 0
    izero = where(arrtemp ge radlim,nzero) 
    if (nzero gt 0) then arrtemp(izero) = 0
    arrtemp = temporary(arrtemp*mex)
;
    izero = 0
    izero = where(arrtemp le thresh*time,nzero) 
    ;arrtemp(izero) = time/thresh             ;this must be wrong??
    if (nzero gt 0) then arrtemp(izero) = thresh*time
    if (!debug gt 3) then print,' Array limits: ',minmax(time/arrtemp)
  endelse
endelse
;
corrfacs = time/arrtemp
;
return
end           ;pro limit_mex
