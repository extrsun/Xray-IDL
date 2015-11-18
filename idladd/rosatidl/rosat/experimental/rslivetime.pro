;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rslivetime
;
;*PURPOSE:
; A procedure to calculate the Rosat PSPC livetime factor from event
; rates data and deadtime parameter.
;
;*CALLING SEQUENCE:
;       rslivetime,rates,flive,ierr,deadtp=deadtp
;
;*PARAMETERS:
; INPUTS:
;        RATES  -  Rates structure variable (output from rsgetevr)
;
; OPTIONAL INPUTS:
;        DEADTP -  Dead time parameter (default = 234.)
;        IERR   -  Vector containing error flags
;
; OUTPUTS:
;        FLIVE  -  Vector containing PSPC livetime factor
;
;*NOTES:
; ** THIS PROGRAM IS A SUN FORTRAN TRANSLATION OF THE ORIGINAL VAX FORTRAN
; PROGRAM CREATED BY G.R. HASSINGER OF MPE. **
;
;              - jeffrey a. mendenhall
;
; The PSPC livetime factor, a value between 0 and 1 which has to be
; multiplied to the exposure time to obtain the effective live exposure time,
; is calculated from a product of two values: 
;
; FLIVE1 using the input A1-lower-level-discriminator count rate (A1LL)
; [cts/s] and the deadtime factor (DEADTP) [musec] according to the recipe in
; the TN-ROS-ME-ZA00-025. The deadtime parameter DEADTP, which is actually a
; function of mean energy and PSPC, should be specified from outside as a
; parameter. 
;
; FLIVE2 from the ratio between the accepted and evaluated X-ray event rate
; (AEXE) and the accepted X-ray event rate (AXE). A difference between those
; two indicates loss of events in the telemetry stream because of a busy
; readout. 
;
;      A1LL  (rates.a1l)     EE-A1LL count rate from HK-data [cts/s]
;                            = IA1_EVR (US), EE_A1LL (MPE), A1_AL (RDF)
;      AXE   (rates.acc)     EE-AXE  count rate from HK-data [cts/s]
;                            = IAX_EVR (US), EE_AXE (MPE), XACC (RDF)
;      AEXE  (rates.trans)   EE-AEXE  count rate from HK-data [cts/s]
;                            = IQE_EVR (US), EE_AEXE (MPE), XTRANSM (RDF)
;      DEADTP                Deadtime Parameter (ca. 190-260 [musec])
;      FLIVE1                PSPC Livetime Factor (between 0 and 1)
;      FLIVE2                Livetime Factor (between 0 and 1)
;      FLIVE                 Total Livetime Factor (between 0 and 1)
;      IERR                  O = 0 no error
;                              = 1 negative square root ARG (FLIVE1=1)
;                              = 2 denominator = 0 (FLIVE2=1)
;                              = 3
;
;*MODIFICATION HISTORY:
;    adapted from livtim.pro, written by J. A. Mendenhall (Penn State)
;      livtim.pro was adapted from Fortran code, written April 1992 
;      by G. R. Hasinger (MPE) 
;    Converted to IDL 5-19-92 by MFC
;    modified 02 Sep 1992 (GAR) for inclusion in Rosat IDL Library
;    modified 23 Sep 1993 (GAR) to include the additional 0.0001 sec
;      deadtime per acepted event, which compensates for the removal
;      of all events less than 0.35 ms after a previous event (done to 
;      remove some of the the AP background).
;    modified 28 Dec 1993 (GAR) to be compatible with new version of
;      RSGETEVR that also reads even rates data from RDF format ancillary
;      files
;-
;-------------------------------------------------------------------------------
pro rslivetime,rates,flive,ierr,deadtp=deadtp
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'RSLIVETIME, rates, FLIVE, IERR, deadtp=deadtp (234.)'
  retall 
endif
if (n_elements(deadtp) eq 0) then deadtp = 234.     ;default value
;
a1ll = rates.a1l
axe = rates.acc
aexe = rates.trans
nrat = n_elements(a1ll)
ierr = intarr(nrat)
flive = fltarr(nrat)
flive2 = flive
;
; Calculate PSPC livetime FLIVE1 (call it FLIVE to save memory)
;
arg = 2.0E-6 * deadtp * a1ll 
ind = where( (arg ge 0) and (arg le 1) )
if (ind(0) ge 0) then flive(ind) = sqrt(1.0 - arg(ind))
;
; Check for error condition
;
ind = where( (arg lt 0) or (arg GT 1) )
if (ind(0) ge 0) then begin
  ierr(ind) = 1
  flive(ind) = 1.
endif
;
; Calculate livetime FLIVE2
;
ind = where(axe ne 0)
if (ind(0) ge 0) then flive2(ind) = aexe(ind)/(axe(ind)*1.0)
;
; Check for error condition
;
ind = where(axe eq 0)
if (ind(0) ge 0) then begin
  ierr(ind) = 2
  flive2(ind) = 1.
endif
;
; Multiply the two values (use tempoaray function to save memory)
; 
flive = temporary(flive * flive2)
;
; From Steve Snowden's code CAST_EXP 23 Sep 1993:
; Calculate the live time fraction with the additional 0.0001 sec
; deadtime per acepted event.  This is to compensate for the removal
; of all events less than 0.35 ms after a previous event which removes
; some of the the AP background.  The nominal deadtime correction is
; 0.25 ms per event.
;
; In the Fortran code, this is FLIVE1 = 1. - 0.0001*AEXE
;
flive2 = 0
flive2 = 1. - 0.0001*axe
;
; Multiply the two values again
; 
flive = temporary(flive * flive2)
;
return
end         ;pro rslivetime
