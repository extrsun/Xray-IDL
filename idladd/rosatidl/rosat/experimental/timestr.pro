;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       timestr
;
;*PURPOSE:
; A procedure to convert start and stop time vectors into a PROS style ASCII
; string for time filtering
;
;*CALLING SEQUENCE:
;       str = timestr(tbeg,tend)
;
;*PARAMETERS:
; INPUTS
;   tbeg     Beginnings of time intervals
;   tend     Ends of time intervals
;
; OUTPUTS
;   timestr  ASCII string of form 'time=(tbeg(0):tend(0),...)'
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
function timestr,tbeg,tend
;
timestr = 'time=('
ntimes = n_elements(tbeg) < n_elements(tend)     ;match to shorter vector
for nn = 0,ntimes-2 do $
  timestr = timestr + strtrim( string(tbeg(nn),'$(f16.4)'),2 ) + ':' $
            + strtrim( string(tend(nn),'$(f16.4)'),2 ) + ','
nn = ntimes - 1
timestr = timestr + strtrim( string(tbeg(nn),'$(f16.4)'),2 ) + ':' $
          + strtrim( string(tend(nn),'$(f16.4)'),2 ) + ')'
;
return,timestr
end           ;function timestr
