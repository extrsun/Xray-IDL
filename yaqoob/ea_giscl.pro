pro ea_giscl, obsname, date=date, loglun=loglun, echo=echo

if n_params(0) eq 0 then begin
    print,'ea_giscl, obsname, date=date, loglun=loglun, echo=echo'
    print,'Creates new GIS *evt files and run gisclean on them
    retall
endif

if n_elements(date) eq 0 then date = ''
if n_elements(loglun) eq 0 then loglun = -1 

det_name = ['sis0', 'sis1', 'gis2', 'gis3']
short_det_name = ['s0', 's1', 'g2', 'g3']
for det=2,3 do begin
  root = obsname + "_" + short_det_name(det) + "_" + date

  newroot = root + "_giscl"
  sname =  newroot + ".xco"
  flist = findfile(sname, count=count)
  if count gt 0 then begin
    printf, loglun, sname + ' already exists, delete it if you want gisclean'
    printf, loglun, ' to be run again'
    if keyword_set(echo) then begin 
      print, sname + ' already exists, delete it if you want gisclean to be'
      print, ' run again'
    endif
    return
  endif

  evtlist = ea_fingun(root + "_mkf.evt*", $
    count=count)
  if count eq 0 then begin
    print, 'ea_giscl: cound not find any ' + root + '_mkf.evt* files'
    stop
  endif

  if count gt 1 then begin
    print, 'ea_giscl: found more than one ' + root + '_mkf.evt* file'
    stop
  endif

  flist = findfile(newroot + '_mkf.evt', count=count)
  if count gt 0 then begin
    cmd = 'rm ' + newroot + '_mkf.evt'
    spawn, cmd, /sh
    printf, loglun, cmd
    if keyword_set(echo) then print, cmd
  endif

; Open script file
  openw, lun, sname, /get_lun

  printf, lun, "xsel"
  printf, lun, "set instr " + det_name(det)

; First read the individual evt files in
  printf, lun, "read eve " + evtlist(0) + " ."

  printf, lun, "gisclean"
  printf, lun, "extr eve"
  printf, lun, "save eve " + newroot + "_mkf.evt use_events=yes clobber=yes"
  printf, lun, "extr im"
  printf, lun, "save im " + newroot + "_mkf.img"
  printf, lun, "exit"
  printf, lun, "no"

  free_lun, lun
  spawn,'xselect @'+sname

endfor

return
end

