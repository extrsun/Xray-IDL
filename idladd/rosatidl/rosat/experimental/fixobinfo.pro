;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;  fixobinfo
;
;*PURPOSE:
; A procedure to "fix" the information in the OBI information structure
; variable constructed from MPE-processed Rosat data
;
;*CALLING SEQUENCE:
;	fixobinfo,obinfo,sctime,newobinfo
;
;*PARAMETERS:
; INPUTS:
;       obinfo - data structure containing dates, times, & indices of
;                beginnings & ends of obi segments
;                has the structure of replicate(row,num), where
;                row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                     sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                     ibeg:0L,iend:0L}
;                and num is the total number of intervals
;         ibeg - indices of values in SCTIME equal to values in SCTBEG
;         iend - indices of values in SCTIME equal to values in SCTEND
;       sctime - spacecraft time 
;
; OUTPUTS:
;       newobinfo - New OBI information data structure
;
;*NOTES:
;    In the MPE format, aspect history, event rates, etc. data are stored
;    in FITS files consisting of one table extension. All OBIs are merged
;    into a single table extension. This procedure checks for gaps in the
;    spacecraft time, and assumes that any gaps found mark the boundaries
;    between OBIs. A new data structure is defined.
;
;*MODIFICATION HISTORY:
;    written  03 Oct 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro fixobinfo,obinfo,sctime,newobinfo
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' FIXOBINFO, obinfo, sctime, NEWOBINFO'
  return
endif
;
delt = avg(sctime(1:20)-sctime)
ind = where((sctime(1:*)-sctime) gt 5.*delt,ndiff)
if (ndiff eq 0) then newobinfo = obinfo else begin
  iend = obinfo.iend
  nobi = n_elements(iend)
  iend = [ind,iend(nobi-1)]
  ibeg = [0,ind+1]
  nobi = n_elements(ibeg)
;
  sctold = obinfo.sctbeg
  sctbeg = sctime(ibeg)
  utsec = obinfo.utsbeg
  utsec = utsec(0)
  utday = obinfo.daybeg
  utday = utday(0)
  utyr = obinfo.yrbeg
  utyr = utyr(0)
  daysec = 24.*3600.
  if (utyr eq 4.*fix(utyr/4)) then daynum = 366. else daynum = 365.
;
  diff = sctbeg - sctold(0)
  utsbeg = utsec + diff
  daydiff = utsbeg/daysec
  daybeg = utday + fix(daydiff)
  utsbeg = utsbeg - daysec*fix(daydiff)
  yrdiff = float(daybeg/daynum)
  yrbeg = utyr + fix(yrdiff-.00001)
  daybeg = daybeg - daynum*fix(yrdiff-.00001)
;
  sctold = obinfo.sctend
  sctend = sctime(iend)
  utsec = obinfo.utsend
  utsec = utsec(0)
  utday = obinfo.dayend
  utday = utday(0)
  utyr = obinfo.yrend
  utyr = utyr(0)
  if (utyr eq 4.*fix(utyr/4)) then daynum = 366. else daynum = 365.
;
  diff = sctend - sctold(0)
  utsend = utsec + diff
  daydiff = utsend/daysec
  dayend = utday + fix(daydiff)
  utsend = utsend - daysec*fix(daydiff)
  yrdiff = float(dayend/daynum)
  yrend = utyr + fix(yrdiff-.00001)
  dayend = dayend - daynum*fix(yrdiff-.00001)
;
  row = {obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,$
        sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,ibeg:0L,iend:0L}
  newobinfo = replicate(row,nobi)
  newobinfo.sctbeg = sctbeg
  newobinfo.sctend = sctend
  newobinfo.ibeg = ibeg
  newobinfo.iend = iend
  newobinfo.yrbeg = yrbeg
  newobinfo.daybeg = daybeg
  newobinfo.utsbeg = utsbeg
  newobinfo.yrend = yrend
  newobinfo.dayend = dayend
  newobinfo.utsend = utsend
endelse
;
return
end         ;pro fixobinfo
