;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   rdactfil
;*PURPOSE:
; A procedure to read ASCII file of accepted time intervals, with one
; interval (start and stop times) per line
;
;*CALLING SEQUENCE:
;   rdactfil,actfil,actbeg,actend,nact
;
;*PARAMETERS:
; INPUTS:
;    actfil - String variable giving the name of the file
;
; OUTPUTS:
;    actbeg   - Start times of the actfil time intervals
;    actend   - Stop times of the actfil time intervals
;    nact     - Number of actfil time intervals
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  MPE_GTIMES
;  US_GTIMES
;
;*MODIFICATION HISTORY:
;    written 12 Aug 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro rdactfil,actfil,actbeg,actend,nact
;
openr,un,actfil,/get_lun    ;get free logical unit & open input ASCII file 
actbeg = [-1.D0]
actend = [-1.D0]
tb = 0.D0
te = 0.D0
check = 1
while ( (not eof(un)) and (check) ) do begin
  readf,un,tb,te
  check = (tb+te) gt 0
  if (check) then begin
    actbeg = [actbeg,tb]
    actend = [actend,te]
  endif
endwhile
actbeg = actbeg(1:*)
actend = actend(1:*)
;
return
end            ;pro rdactfil
