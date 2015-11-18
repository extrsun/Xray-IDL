;============================================================================
;;; $Id: open_unique_file.pro 1369 2001-03-07 11:04:57Z patb $
;;; Open a file with a name that's unique.
;;; 
;;; The options should be pretty obvious.
;============================================================================

PRO open_unique_file, unit, pathname, DIRECTORY=directory, SUFFIX=suffix, DELETE=delete

if NOT keyword_set(directory) then directory = "."
if NOT keyword_set(suffix)    then suffix    = ".tmp"

Error = 1
for attempt=0,1 do begin
  filename = strcompress(string(systime(1),suffix,F='(F12.1,A)'),/REMOVE_ALL)
  pathname = directory + "/" + filename
  
  if file_test(pathname) then begin
    print, 'OPEN_UNIQUE_FILE: Warning, file exists: ',pathname
  endif else begin
    openw, unit, pathname, /GET_LUN, DELETE=keyword_set(delete), ERROR=Error
    if (Error NE 0) then begin
      print, 'OPEN_UNIQUE_FILE: Error opening ',pathname
      print, '  ', !ERROR_STATE.MSG
    endif else begin
      break
    endelse
  endelse
 
 
  pathname = filepath(filename, /TMP)
  
  if file_test(pathname) then begin
    print, 'OPEN_UNIQUE_FILE: Warning, file exists: ',pathname
  endif else begin
    openw, unit, pathname, /GET_LUN, DELETE=keyword_set(delete), ERROR=Error
    if (Error NE 0) then begin
      print, 'OPEN_UNIQUE_FILE: Error opening ',pathname
      print, '  ', !ERROR_STATE.MSG
    endif else begin
      break
    endelse
  endelse
endfor

if (Error NE 0) then message, "OPEN_UNIQUE_FILE: Cannot create file."

return
end
