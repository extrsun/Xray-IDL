function exist,filename
;+
; NAME:
;   EXIST
; PURPOSE:
;   A very simple check to see if a file exists...
; CALLING SEQEUNCE:
;   result=exist(filename)'
; INPUT:
;   FILENAME  This is the filename or search spec. that should be checked.
; OUTPUT:
;   RESULT    The returned result is the number of files which match the
;               search spec, and 0 if no file exists.
; EXAMPLE:
;   if exist('input.dat') then print,'Yes' else print,'No'
;   if not exist('input.dat') then print,'Create'
;   result=exist('*.hhh') & print,strn(count),' Header files available'
; NOTES:
;   A search spec of multiple files uses `findfile()`.  A search for a
;   a single file uses `openr` with an error trap because it is much faster.
;   Unreadable files will in that case appear not to exist.
; HISTORY:
;   1992-07-27 Header added to old routine  (E. W. Deutsch)
;   1999-04-16 Rewritten to be faster and tidier  (E. W. Deutsch)
;-

  if (n_elements(filename) ne 1) then begin
    print,'Call> result=exist(filename)'
    print,"e.g.> result=exist('input.dat')"
    print,"e.g.> if not exist('input.dat') then stop"
    print,"e.g.> result=exist('*.hhh')"
    return,0
    endif


  if (strpos(filename(0),'*') ne -1) or $
    (strpos(filename(0),'?') ne -1) then begin
    files=findfile(filename(0),count=count)
    return,count
    endif


  on_ioerror,CANTOPEN
  openr,lun,filename(0),/get_lun
  close,lun & free_lun,lun
  return,1


CANTOPEN:
  return,0


end
