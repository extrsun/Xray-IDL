pro ea_mergescr, obsname, det, date=date, loglun=loglun, echo=echo

if n_params(0) eq 0 then begin
    print,'ea_mergescr, obsname, det, date=date, loglun=loglun, echo=echo'
    print,'Creates initial xselect processing script, called from extr_asca'
    retall
endif

if n_elements(date) eq 0 then date = ''
if n_elements(loglun) eq 0 then loglun = -1 

det_name = ['sis0', 'sis1', 'gis2', 'gis3']
short_det_name = ['s0', 's1', 'g2', 'g3']
root = obsname + "_" + short_det_name(det) + "_" + date

; Open script file
sname = obsname + "_" + short_det_name(det) + "_screened_" + date + ".xco"
flist = findfile(sname, count=count)
if count gt 0 then begin
  printf, loglun, sname + ' already exists, delete it if you want the screened'
  printf, loglun, ' files to be reprocessed'
  if keyword_set(echo) then begin 
    print, sname + ' already exists, delete it if you want the screened'
    print, ' files to be reprocessed'
  endif
  return
endif

evtlist = ea_fingun("ad*" + short_det_name(det) + "*evt*", count=count)
if count eq 0 then stop

openw, lun, sname, /get_lun

printf, lun, "xsel"
printf, lun, "set instr " + det_name(det)

; First read the individual evt files in
for i=0, n_elements(evtlist) - 1 do $
  printf, lun, "read eve " + evtlist(i) + " ."

; Now save the merged events list with the proper file name
printf, lun, "extr eve"
printf, lun, "save eve " + root + "_mkf.evt use_events=yes clobber=yes"
printf, lun, "exit"
printf, lun, "no"

free_lun, lun
spawn,'xselect @'+sname

return
end

