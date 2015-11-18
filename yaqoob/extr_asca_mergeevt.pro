pro extr_asca_mergescr, obsname, det, evtlist, date=date, loglun=loglun

if n_params(0) eq 0 then begin
    print,'extr_asca_mergescr, obsname, det, evtlist, date=date, loglun=loglun'
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
  return
endif

openw, lun, sname, /get_lun

printf, lun, "xsel"
printf, lun, "set instr " + det_name(det)

; First read the individual evt files in
for i=0, n_elements(evtlist) - 1 do $
  printf, lun, "read eve " + evtlist(i) + " ."

; Now save the merged events list with the proper file name
printf, lun, "extr eve"
printf, lun, "save eve " + root + "_mkf.evt use_events=yes clobber=yes"

free_lun, lun
spawn,'xselect @'+sname

return
end

