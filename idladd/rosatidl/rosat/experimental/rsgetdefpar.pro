;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:	RSGETDEFPAR
;
;*PURPOSE: Procedure to get default parameters from parameter file
;
;*CALLING SEQUENCE:
;	rsgetdefpar,dfile,names,dpar,descr,nmax
;
;*PARAMETERS:
; INPUT:
;	dfile - default parameter file name without extension
;		extension will be .def in derectory zdef:
;       nmax  - maximum parameter number of parameters to be returned
; OUTPUT:
;	names - names of the parameters
;	dpar - default values
;	descr - descriptions
;
; METHOD:
;	The default parameters are read from file !defdir+dfile
;          [getenv('ZDEF')+dfile+'.def' if all defaults are used]
;
;*MODIFICATION HISTORY:
; HISTORY:
;	taken from GHRS program getdefpar written by D. Lindler  05 Oct 1991
;       modified 13 Apr 1992 to return up to parameter nmax (GAR)
;       modified 14 Jul 1993 to use new system variable !defdir = default 
;          directory for .def files, so user can change this within IDL
;          !defdir is initially set to getlog('ZDEF') by ROSATLIB upon
;          starting IDL (GAR)
;       modified 01 Oct 1993 to be compatible with new definition of 
;          !defdir = getenv('ZDEF') - this means checking the operating
;          system variable !version.os to see whether Unix is being used,
;          and if so, to append a '/' to the end
;-
;-------------------------------------------------------------------------------
pro rsgetdefpar,dfile,names,dpar,descr,nmax
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'RSGETDEFPAR, dfile, NAMES, DPAR, DESCR, nmax'
  retall
endif
;
; define default parameters file: zdef is directory, but syntax is
; different under Unix as opposed to VMS
;
; But add getlog('ZDEF') only if no other directory is specified in DFILE
; and use !defdir because user may wish to change this within IDL
;
zdef = ''
fdecomp,dfile,disk,dir,root,ext,ver
if ((disk+dir) eq '') then zdef = !defdir
if (!version.os ne 'vms') then zdef = zdef+'/'
deffile = zdef+dfile
if (ext eq '') then deffile = deffile+'.def'
get_lun,unit
openr,unit,deffile
;
st=''
readf,unit,st                           ; read past header
;
n=0
readf,unit,n                            ; read size of parameter array
if (n_elements(nmax) eq 0) then nmax = n    ;default is return all
if (nmax gt n) then nmax = n
;
; create output arrays
;
names=strarr(n)
dpar=strarr(n)
descr=strarr(n)
;
; read each parameter and place in arrays
;
while not eof(unit) do begin
  readf,unit,st
;
  pos=gettok(st,',')              ; get position
  pos=fix(pos)
;
  name=gettok(st,'=')             ; get name
  name=strupcase(name)
  names(pos)=name
;
; get value
;	check for "comments field" delimiter which can either be a , or ;
;
  if (strpos(st,';') ne -1) then val=strtrim(gettok(st,';')) else $
     val=strtrim(gettok(st,','))
;
; For non-VMS, resolve the path.
;
  if (!version.os ne 'vms') then begin
    location = gettok( val,':')
    if (strtrim(val,2) eq '') then val= location else begin
      path = getlog(location)
      val = path + val
    endelse
  endif
  dpar(pos)=val
;
; whats left is the description
;
  descr(pos)=st
endwhile
;
; close file
;
close,unit
free_lun,unit
;
if (nmax ne n) then begin
  names = names(0:nmax)
  dpar = dpar(0:nmax)
  descr = descr(0:nmax)
endif  
;
return
end               ;pro rsgetdefpar
