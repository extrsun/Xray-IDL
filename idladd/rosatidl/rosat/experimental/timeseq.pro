;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   timeseq
;
;*PURPOSE:
;   A procedure to convert times read from Rosat files (given in seconds
;   from beginning of day) to day number and fraction
;
;*CALLING SEQUENCE:
;   timeseq,time,tcorr,obinfo=obinfo
;
;*PARAMETERS:
; INPUTS:
;   time - times read from Rosat file (seconds from beginning of day)
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
;   tcorr - times given in fractional days
;
;*NOTES: 
;   If obinfo is not supplied, then time is converted to fractions of days
;
;*MODIFICATION HISTORY:
;    written 04 Jun 1992 by GAR
;-
;-------------------------------------------------------------------------------
pro timeseq,time,tcorr,obinfo
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' TIMESEQ, time, TCORR, obinfo'
  retall
endif
;
if (n_elements(obinfo) eq 0) then tcorr = double(time)/3600./24. else begin
  yrbeg = obinfo.yrbeg 
  yrend = obinfo.yrend
  daybeg = obinfo.daybeg 
  dayend = obinfo.dayend
  ibeg = obinfo.ibeg 
  iend = obinfo.iend
  nobi = n_elements(ibeg)
;
  tcorr = time*0.
  for nn=0,nobi-1 do begin
    ib=ibeg(nn) 
    ie=iend(nn) 
    tsel = double(time(ib:ie))/24./3600.
;
    if (nobi gt 1) then ipos = where(daybeg(1:*) lt daybeg) else begin
       if (yrend ne yrbeg) then ipos = [0] else ipos = [-1]
    endelse
    if (ipos(0) ge 0) then begin                ;if cross year boundary
      ipos = ipos(0)
      yr = yrbeg(nn)
      ndays = ymd2dn(yr,12,31)                  ;calculate no. days in year
      if (nobi gt 1) then begin
        daybeg(ipos+1) = daybeg(ipos+1:*) + ndays
        dayend(ipos+1) = dayend(ipos+1:*) + ndays
      endif else dayend = dayend + ndays
    endif
;
    if (daybeg(nn) ne dayend(nn)) then begin    ;ie, if cross day boundary
      ipos = where( tsel(1:*) lt tsel) 
      ipos = ipos(0)
      tsel(ipos+1) = tsel(ipos+1:*) + 1.
    endif
;
    tcorr(ib) = daybeg(nn) + tsel
  endfor
;
endelse
;
return        ;pro timeseq
end
