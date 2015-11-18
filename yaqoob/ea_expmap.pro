pro ea_expmap, root, det, timeflt, date=date, loglun=loglun, echo=echo, aux=aux

if n_params(0) eq 0 then begin
  print, 'ea_expmap, root, det, timeflt, date=date, loglun=loglun, echo=echo, aux=aux
  print, 'Produces an exposure map'
  print, 'Intended to be called from extr_asca'
  return
endif

if n_elements(loglun) eq 0 then loglun = -1 

if det ge 2 then begin
  printf, loglun, 'Cannot produce expouse map for the GIS'
  if keyword_set(echo)  then print, 'Cannot produce expouse map for the GIS'
  return
endif


flist = findfile(timeflt, count=count)
if count eq 0 then begin
  printf, loglun, 'ea_expmap: could not find gti file ' + timeflt
  if keyword_set(echo) then print, 'ea_expmap: could not find gti file ' + timeflt
  return
endif
gti = read_fitsgti(timeflt)

; Raw events lists
unf = findfile('*s' + strn(det) + '*unf*', count=count)
if count eq 0 then begin
  printf, loglun, 'ea_expmap: could not find any files of the form ' + $
   '*s' + strn(det) + '*unf*'
  if keyword_set(echo) then print, 'ea_expmap: could not find any files of the form ' + $
   '*s' + strn(det) + '*unf*'
  return
endif

ea_gunzip, unf
unf = findfile('*s' + strn(det) + '*unf', count=count)
openw, lun, root + '_unflist.dat', /get_lun
for i=0, count-1 do printf, lun, unf(i)
close, lun

; Filtered events lists containing hot pixel lists
flt = findfile(root + '*hc*evt* ' + root + '*mkf.evt*', count=count)
if count eq 0 then begin
  printf, loglun, 'ea_expmap: could not find any files of the form ' + $
    root + '*hc*evt* ' + root + '*mkf.evt*'
  if keyword_set(echo) then print, 'ea_expmap: could not find any files of the form ' + $
    root + '*hc*evt* ' + root + '*mkf.evt*'
  return
endif

ea_gunzip, flt
flt = findfile(root + '*hc*evt ' + root + '*mkf.evt', count=count)
openw, lun, root + '_fltlist.dat'
for i=0, count-1 do printf, lun, flt(i)
close, lun
extr_hot, root + '_fltlist.dat', root + '_hotpix.dat'

; Ascertain ccd mode
qreadasca, flt(0), plist
ccdmode = n_elements(where(histogram(plist.ccd) gt 0))

if n_elements(aux) eq 0 then aux = '../aux/'

; Make sure SIS caldb files are present
flist = findfile(aux + "s" + strn(det) + "_teldef_ascalin.fits*", count=count)
if count eq 0 then begin
  printf, loglun, "Cannot find SIS caldb file " + aux + "s" + strn(det) + $
    "_teldef_ascalin.fits*"
  printf, loglun, "Returning"
  if keyword_set(echo) then begin
    print, "Cannot find SIS caldb file " + aux + "s" + strn(det) + $
      "_teldef_ascalin.fits*"
    print, "Returning"
  endif
  return
endif
ea_gunzip, flist

telem_str = "telemgti2, '" + root + "_unflist.dat', satfile='" + root + $
  "', gtiname='" + root + "_unsat.gti', goodfile='" + root +$
  "_good.dat', dir=''"
printf, loglun, telem_str
if keyword_set(echo) then print, telem_str
flush, loglun

if not execute(telem_str) then begin
  printf, loglun, 'telemgti did not run successfully'
  if keyword_set(echo) then print, 'telemgti did not run successfully'
  return
endif

sisexp_str = "sisexp2, " + strn(det) + ", gti, satfile='" + root + $
  ".sat', hotfile='" + root + "_hotpix.dat', dir=aux, expname='" + root $
  + "', ccdmode=" + strn(ccdmode)
printf, loglun, sisexp_str
if keyword_set(echo) then print, sisexp_str
if not execute(sisexp_str) then begin
  printf, loglun, "sisexp2 did not run successfully"
  if keyword_set(echo) then print, "sisexp2 did not run successfully"
  return
endif

gz = 'gzip -v *sat*'
printf, loglun, gz
if keyword_set(echo) then print, gz
spawn, gz

free_lun, lun
return
end
