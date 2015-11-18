function acis_pha_to_kev, plist
;+
; NAME:
;
; ACIS_PHA_TO_KEV
;
; PURPOSE:
;
; Estimates energies for an ACIS PHA values
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
; E = acis_pha_to_kev(plist)
; 
; INPUTS:
;
; plist - ACIS (or HETG or LETG) photon list
;
; OPTIONAL INPUTS:
;
;
;	
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
; energies in keV
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
; Requires full photons list rather than just pha values since the
; front and back illuminated chips have different responces.  The conversion
; was estimated by simple ploting pha vs. energy for the front and
; back illuminated after a MARX sim.
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
; Created by A. Ptak, 12/97
;-

if n_params(0) eq 0 then begin
  print, 'E = acis_pha_to_kev(plist)'
  retall
endif

e = fltarr(n_elements(plist))
w = where(plist.ccdid eq 5 or plist.ccdid eq 7, n)
if n gt 0 then e(w) = plist(w).pha * 0.00452
w = where(plist.ccdid ne 5 and plist.ccdid ne 7, n)
if n gt 0 then e(w) = plist(w).pha * 0.00401
return, e
end
