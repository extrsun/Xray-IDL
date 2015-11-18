pro ea_mergeraw, obsname, det, bitrates, date=date, loglun=loglun, echo=echo

if n_params(0) eq 0 then begin
    print,'ea_mergeraw, obsname, det, bitrates, date=date, loglun=loglun, echo=echo'
    print,'Creates initial xselect processing script, called from extr_asca'
    retall
endif

if n_elements(date) eq 0 then date = ''
if n_elements(loglun) eq 0 then loglun = -1 

det_name = ['sis0', 'sis1', 'gis2', 'gis3']
short_det_name = ['s0', 's1', 'g2', 'g3']
bitrate_name = ['low', 'medium', 'high']

; Open script file
sname = obsname + "_" + short_det_name(det) + "_mergeraw_" + date + ".xco"
flist = findfile(sname, count=count)
if count gt 0 then begin
  printf, loglun, sname + ' already exists, delete it if you want the raw'
  printf, loglun, ' files to be reprocessed'
  if keyword_set(echo) then begin
    print, sname + ' already exists, delete it if you want the raw'
    print, ' files to be reprocessed'
  endif
  return
endif

; Check to see if files need to be decompressed
root = "*" + strupcase(short_det_name(det))
if det < 2 then root = root + "*02" else root = root + "*"
flist = findfile(root + "?.fits*", count=count)
ea_gunzip, flist

openw, lun, sname, /get_lun

; First combine raw files by bit rate
printf, lun, "xsel"
printf, lun, "set instr " + det_name(det)
printf, lun, "set DUMPCAT"
for i=0, n_elements(bitrates)-1 do begin
  printf, lun, "make obs"  
  printf, lun, "."
  printf, lun, "DEF"
  choose = 'choose "'
  if det lt 2 then choose = choose + "DATAMODE.EQ.'BRIGHT' .AND. "
  choose = choose + "BIT_RATE.EQ.'" + strupcase(bitrate_name(bitrates(i))) + $
    "'" + '"'
  printf, lun, choose
  printf, lun, "extr eve"
  printf, lun, "save eve " + obsname + "_" + short_det_name(det) + "_" + $
    bitrate_name(bitrates(i)) + "_" + date + ".unf use_events=yes clobber=yes"
  printf, lun, "clear all"
  printf, lun, "yes"
  printf, lun, "set instr " + det_name(det)
endfor

printf, lun, 'exit'
printf, lun, 'no'

free_lun, lun
printf, loglun, 'xselect @'+sname
if keyword_set(echo) then print, 'xselect @'+sname 
spawn,'xselect @'+sname
spawn,'gzip ' + root + "?.fits"
return
end


