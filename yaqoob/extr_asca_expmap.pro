pro extr_asca_expmap, root, det, timeflt, date=date, loglun=loglun

if n_params(0) eq 0 then begin
  print, 'extr_asca_expmap, root, det, timeflt, date=date, loglun=loglun
  print, 'Produces an exposure map'
  print, 'Intended to be called from extr_asca'
  return
endif

if n_elements(loglun) eq 0 then loglun = -1 

if det ge 2 then begin
  printf, loglun, 'Cannot produce expouse map for the GIS'
  return
endif


flist = findfile(timeflt, count=count)
if count eq 0 then begin
  printf, loglun, 'extr_asca_expmap: could not find gti file ' + timeflt
  retall
endif

; Raw events lists
unf = findfile(root + '*unf*', count=count)
if count eq 0 then begin
  print, loglun, 'extr_asca_expmap: could not find any files of the form ' + $
    root + '*unf*'
  return
endif

extr_asca_gunzip, unf
unf = findfile(root + '*unf', count=count)
openw, lun, root + 'unflist.dat'
for i=0, count-1 do printf, lun, unf(i)
close, lun

; Filtered events lists containing hot pixel lists
flt = findfile(root + '*hc*evt* ' + root + '*mkf.evt*', count=count)
if count eq 0 then begin
  printf, loglun, 'extr_asca_expmap: could not find any files of the form ' + $
    root + '*hc*evt* ' + root + '*mkf.evt*'
  return
endif
extr_asca_gunzip, flt
flt = findfile(root + '*hc*evt ' + root + '*mkf.evt', count=count)
openw, lun, root + 'fltlist.dat'
for i=0, count-1 do printf, lun, flt(i)
close, lun
extr_hot, root + 'fltlist.dat', root + 'hotpix.dat'

telem_str = "telemgti, " + root + "_unflist.dat, satfile='" + root + $
  "', gtiname='" + timeflt + "', goodfile='" + root + "_good.dat', dir=''"
printf, loglun, telem_str
flush, loglun

if not execute(telem_str) then begin
  printf, loglun, 'telemgti did not run successfully'
  return
endif

free_lun, lun
return
end

