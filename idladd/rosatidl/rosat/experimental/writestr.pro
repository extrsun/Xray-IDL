;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       writestr
;
;*PURPOSE:
; A procedure to write a vector of ASCII strings to an output file
;   (one element per line)
;
;*CALLING SEQUENCE:
;       writestr,file,str
;
;*PARAMETERS:
; INPUTS
;   file   Name of file (if no extension given, default is .tab)
;   str    Input ASCII string (or string vector)
;
; OUTPUTS
;   An ASCII file; each line is one element of the string vector
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    written 06-09-92 by GAR
;-
;-------------------------------------------------------------------------------
pro writestr,file,str
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' WRITESTR, file, str'
  return
endif
;
fdecomp,file,disk,dir,root,ext,ver
if (ext eq '') then ext = 'tab'
name = disk + dir + root + '.' + ext 
;if (ver ne '') then name = name + ';' + ver
;
print,' Writing input string to output file '
openw, unit, name, /get_lun
;
nstr = n_elements(str)
for ii=0,nstr-1 do printf,unit,str(ii)
;
close,unit
free_lun,unit
;
return         ;pro writestr
end
