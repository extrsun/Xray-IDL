;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   ut2sct
;
;*PURPOSE:
;   A procedure to convert UT  times read from Rosat files (given in seconds
;   from beginning of day) to spacecraft time
;
;*CALLING SEQUENCE:
;   sctime = ut2sct(ut,obinfo)
;
;*PARAMETERS:
; INPUTS:
;   ut     - UT times read from Rosat file (seconds from beginning of day)
;   obinfo - data structure containing dates, times, & indices of
;            beginnings & ends of obi segments
;            has the structure of replicate(row,num), where
;            row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                 sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                 ibeg:0L,iend:0L}
;            and num is the total number of intervals
;            ibeg and iend separate the OBIs
;
; OUTPUTS:
;   sct - spacecraft times given in seconds since launch
;
;*NOTES: 
;   Obinfo must be supplied
;
;*MODIFICATION HISTORY:
;    written 04 Jun 1992 by GAR
;-
;-------------------------------------------------------------------------------
function ut2sct,ut,obinfo
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' SCTIME = UT2SCT( ut, obinfo )'
  retall
endif
if (npar lt 2) then begin
  print,' UT2SCT requires 2 parameters - UT time & obinfo. Returning.'
  retall
endif
;
sctbeg = obinfo.sctbeg
utbeg = obinfo.utsbeg
ibeg = obinfo.ibeg 
iend = obinfo.iend
nobi = n_elements(ibeg)
;
sctime = double(ut*0.)
for nn=0,nobi-1 do begin
  ib=ibeg(nn) 
  ie=iend(nn) 
;
  sctime(ib) = sctbeg(nn) - utbeg(nn) + ut(ib:ie)
endfor
;
return,sctime        ;function ut2sct
end
