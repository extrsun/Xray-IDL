function spec_dir,filename,extension
;+
; NAME:
;   SPEC_DIR
; PURPOSE:
;   Provide a complete file specification by appending a default disk
;   or directory if necessary.
; CALLING SEQUENCE:
;   File_spec = SPEC_DIR(filename,[extension])
; INPUT:
;   filename - character string giving partial specification of a file
;              name.  VMS examples include 'UIT$USER2:TEST.DAT', '[.SUB]TEST',
;              or 'ROCKET:M33UV10B'
; OPTIONAL INPUT:
;    exten - string giving the default file name extension to be used if
;            filename does not contain one.  Do not include the period.
; OUTPUT:
;   File_spec - Complete file specification using default disk or 
;              directory when necessary.  If the default directory
;              is UIT$USER1:[UITIDL] then, for the above examples, the
;              output would be 'UIT$USER2:[UITIDL]TEST.DAT'
;              'UIT$USER2:[UITIDL.SUB]TEST', and 
;              'UIT$USER0:[UITTEST.DATA.ROCKET]M33UV10B'.
; METHOD:
;   For Unix, SPEC_DIR will simply append the default directory obtained
;   from the CD command (if necessary).   Under VMS one must also determine if
;   the disk and/or directory is already specified.
; REVISION HISTORY:
;   Written W. Landsman         STX         July, 1987
;    Revised for use on VAXes and on SUNs, W.  Landsman, N. Collins, STX
;                                                  November, 1990
;-
fdecomp,filename,disk,dir,name,ext             ;Decompose filename
if (ext eq '') and (n_params(0) gt 1) then $   ;Use supplied default extension?
                    ext = extension
if (dir eq '') and (!VERSION.ARCH ne "vax") then begin
    cd,current=dir
    if name ne '' then dir = dir + '/'    ;Get current default directory
endif else begin
  if (disk eq '') or (dir eq '') then begin
    cd,current=defdir                          ;Get current default directory
    fdecomp,defdir,curdisk,curdir
    if disk eq '' then disk = curdisk
    if dir eq '' then dir = curdir
endif
endelse
if ext ne '' then ext = '.'+ext
if !VERSION.ARCH ne "vax" then return,dir+name+ext else  $           ;Unix
                               return,strupcase(disk+dir+name+ext)   ;VMS
end
