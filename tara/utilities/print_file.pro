;+
;========================================================================
;;;
;;; Routine to print & delete a file $Id: print_file.pro 1641 2002-08-24 16:31:45Z patb $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;-
PRO print_file, filename

if (n_elements(filename) EQ 0) then filename = '~/idl.ps'

dest = getenv( 'LPDEST' )
if (dest NE '') then begin
  cmd = 'lpr -P ' + dest + ' ' + filename
endif else begin
  cmd = 'lpr ' + filename
endelse

print, cmd
spawn, cmd, result
print, result
file_delete, filename, /QUIET

return
end
