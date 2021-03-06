pro ea_getsrcbgd, evtfil, srcreg, bgdreg, xybin=xybin, loglun=loglun, echo=echo
if n_params(0) eq 0 then begin
  print, 'ea_getsrcbgd, evtfil, srcreg, bgdreg, xybin=xybin, loglun=loglun, echo=echo'
  print, 'Creates an xselect script to extract srcreg and bgdreg from evtfil'
  print, 'Intended to be called from extr_asca'
  retall
endif

if n_elements(xybin) eq 0 then xybin = 1
if n_elements(loglun) eq 0 then loglun = -1 

dot = strpos(evtfil, '.')
if dot lt 0 then stop
sname = strmid(evtfil, 0, dot) + '_srcbgd.xco'
flist = findfile(sname, count=count)
if count gt 0 then begin
  printf, loglun, sname + ' already exists, delete it if you want the source'
  printf, loglun, ' and bgd events to be re-extracted'
  if keyword_set(echo) then begin
    print, sname + ' already exists, delete it if you want the source'
    print, ' and bgd events to be re-extracted'
  endif
  return
endif

printf, loglun, 'Extracting source and background events using ' + $
  evtfil + " with "
printf, loglun, srcreg + " and " + bgdreg
if keyword_set(echo) then begin
  print, 'Extracting source and background events using ' + $
    evtfil + " with "
  print, srcreg + " and " + bgdreg
endif
 
ea_gunzip, [evtfil + '*']

openw, lun, sname, /get_lun

srcevt = strmid(evtfil, 0, dot) + '_src.evt'
bgdevt = strmid(evtfil, 0, dot) + '_bgd.evt'

printf, lun, "xsel"
printf, lun, "read eve " + evtfil + " ."
printf, lun, "set xybin " + strn(xybin)
printf, lun, "filter region " + srcreg
printf, lun, "extr events"
printf, lun, "save events " + srcevt + " use_events=no clobber=yes"
printf, lun, "clear events"
printf, lun, "clear region"
printf, lun, "filter region " + bgdreg
printf, lun, "extr events"
printf, lun, "save events " + bgdevt + " use_events=no clobber=yes"
printf, lun, "exit"
printf, lun, "no"
free_lun, lun

; Run xselect and echo result to screen or log file
printf, loglun, 'xselect @' + sname
if keyword_set(echo) then print, 'xselect @' + sname
spawn, 'xselect @' + sname, result, count=count
for i=0, count-1 do printf, loglun, result(i)
if keyword_set(echo) then for i=0, count-1 do print, result(i)

return
end

