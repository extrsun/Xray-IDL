pro letglist, h, tab, plist
;+
; NAME:
;
; LETGLIST
;
; PURPOSE:
;
; Creates an events list of letg events from fits table tab
;
; CATEGORY:
;
; X-ray analysis
;
; CALLING SEQUENCE:
;
; letglist, h, tab, plist
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
  print, 'letglist, h, tab, plist'
  print, 'Places MARX fits event list into a plist'
  retall
endif

row = {letgevent, time:0d0, chipx:0, chipy:0, tdetx:0, tdety:0, detx:0, $
       dety:0, raw_phas:intarr(9), ccdid:0, pha:0, grade:0, $
       fltgrade:0, ypos:0., zpos:0., energy:0., shell:0, order:0}
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
plist.shell = tbget(h, tab, 'shell')
plist.order = tbget(h, tab, 'order')
return
end
