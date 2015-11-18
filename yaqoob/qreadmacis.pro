pro qreadmacis, fitsfil, plist, h, gti
if n_params(0) eq 0 then begin
 print,'qreadmacis, fitsfil, plist, h, gti'
 retall
end
;+
; NAME:
;
; QREADMACIS
;
; PURPOSE:
;
; Quick-and-dirty routine to read output of MARX into a photon list array
;
; CATEGORY:
;
; X-ray analysis
;
; CALLING SEQUENCE:
;
; qreadmacis, fitsfil, plist, h, gti
; 
; INPUTS:
;
; fitsfil - name of fits file created by "marx2fits"
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
; plist - array of acisevents, letgevents of hetgevents
; h - header from events extension
; gti - gti in a dblarr(*,2)
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
; Reads events extension from fitsfil, creates array of acisevents,
; letgevents, or hetgevents based on value of "GRATING" keyword.
; letgevents and hetgevents are the same as acisevents but also
; contain order and shell fields (seperate structure names are used
; for letg and hetg to allow for some sanity checking later on.)
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
; Written by A. Ptak, ptak@astro.phys.cmu.edu, 12/97
;-

tab = readfits(fitsfil, ext=1, h, /silent)
gstart = tbget(h,tab,'start')
gstop = tbget(h,tab,'stop')
gti = dblarr(n_elements(gstart),2)
gti(*,0) = gstart
gti(*,1) = gstop
tab = readfits(fitsfil, ext=2, h, /silent)
instr = sxpar(h, 'INSTRUME')
if strmid(instr, 0, 4) ne 'ACIS' then begin
  print, 'Error... this is not an ACIS events file'
  retall
endif
grating = SXPAR(h,'GRATING')
grating = strmid(grating,0,4)
if (grating eq 'NONE') then acislist, h, tab, plist
if (grating eq 'HETG') then hetglist, h, tab, plist
if (grating eq 'LETG') then letglist, h, tab, plist
print,grating
return
end

