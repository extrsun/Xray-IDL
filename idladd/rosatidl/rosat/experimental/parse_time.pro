;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       parse_time
;
;*PURPOSE:
; A procedure to convert an ASCII descriptor for time filtering (e.g. from
; PROS/IRAF) into start and stop times for filtering
;
;*CALLING SEQUENCE:
;       parse_time,file,TBEG,TEND,linenum=linenum
;
;*PARAMETERS:
; INPUTS
;   file     ASCII file containing PROS-style string(s) giving start and
;            stop times to be included
; 
; OPTIONAL INPUTS
;   linenum  Number(s) of lines in file to be included.
;            In PROS style files, time intervals can be specified in more
;            than one line. 
;            If not specified, all lines are included.
;            Note: line numbers start from 1 (not 0)
; OUTPUTS
;   tbeg     Beginnings of time intervals
;   tend     Ends of time intervals
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  STRPOS
;  GETTOK
;  GETOPT
;
;*MODIFICATION HISTORY:
;    written 06-09-92 by GAR
;-
;-------------------------------------------------------------------------------
pro parse_time,file,tbeg,tend,linenum=linenum
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' PARSE_TIME, file, TBEG, TEND, linenum=linenum'
  return
endif
;
if (n_elements(linenum) eq 0) then lines = 0 else lines = linenum
if (n_elements(lines) eq 1) then lines = lines + intarr(1)
;
; first, make sure that the file exists (or give user a chance to bail out)
;
fdecomp,file,disk,dir,rootname,ext,ver
ext = '.'+ext
match_files,dir,rootname,ext,name,nlist
name = name(0)
;
get_lun,un
openr,un,name
check = -1
dum = ''
while ( (check lt 0) and (not EOF(un)) ) do begin   ;find first time descriptor
  readf,un,dum
  check = strpos(dum,'time=')
endwhile
;
if (check lt 0) then begin
  print,' No time intervals found before end of file. Returning.'
  retall
endif 
;
; Now read the whole file until the end. Include only time strings
;
timstr = dum
while (not EOF(un)) do begin
  readf,un,dum
  check = strpos(dum,'time=')
  if (check ge 0) then timstr = [timstr,dum]
endwhile
close,un
nstr = n_elements(timstr)
;  
if (lines(0) eq 0) then lines = indgen(nstr) + 1
nlines = n_elements(lines)
tbeg = [-1.D0]
tend = tbeg
for nn = 0,nlines-1 do begin            ;parse strings for selected times  
  nct = lines(nn) - 1
  dum = timstr(nct)
  try = gettok(dum,'(')
  while (dum ne '') do begin            ;may have > 1 time interval per line
    try = gettok(dum,',')
    if (dum eq '') then try=gettok(try,')')     ;get rid of final )
    tbeg = [tbeg,double(gettok(try,':'))]
    tend = [tend,double(try)]
  endwhile
endfor
tbeg = tbeg(1:*)
tend = tend(1:*)
;
return
end           ;pro parse_time
