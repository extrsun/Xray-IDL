pro acislist, h, tab, plist
;+
; NAME:
;
; ACISLIST
;
; PURPOSE:
;
; Creates an events list of acis events from fits table tab
;
; CATEGORY:
;
; X-ray analysis
;
; CALLING SEQUENCE:
;
; acislist, h, tab, plist
; 
; INPUTS:
;
; h - header of events extension
; tab - binary table of events extension
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
; plist - the photon list array
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
; Meant to be called by other routines, see qreadmacis.pro
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
; Written by A. Ptak, 12/97
;-

if n_params(0) eq 0 then begin
  print, 'acislist, h, tab, plist'
  print, 'Places MARX fits event list into a plist'
  retall
endif

row = {acisevent, time:0d0, chipx:0, chipy:0, tdetx:0, tdety:0, detx:0, $
       dety:0, raw_phas:intarr(9), ccdid:0, pha:0, grade:0, $
       fltgrade:0, ypos:0., zpos:0., energy:0.}
ncts = sxpar(h, 'naxis2')
print, strn(ncts) + ' events'
plist = replicate(row, ncts)
plist.time = tbget(h, tab, 'time')
plist.chipx = tbget(h, tab, 'chipx')
plist.chipy = tbget(h, tab, 'chipy')
plist.tdetx = tbget(h, tab, 'tdetx')
plist.tdety = tbget(h, tab, 'tdety')
plist.detx = tbget(h, tab, 'detx')
plist.dety = tbget(h, tab, 'dety')
plist.raw_phas = tbget(h, tab, 'raw_phas')
plist.ccdid = tbget(h, tab, 'ccdid')
plist.pha = tbget(h, tab, 'pha')
plist.grade = tbget(h, tab, 'grade')
plist.fltgrade = tbget(h, tab, 'fltgrade')
plist.ypos = tbget(h, tab, 'ypos')
plist.zpos = tbget(h, tab, 'zpos')
plist.energy = tbget(h, tab, 'energy')
return
end

