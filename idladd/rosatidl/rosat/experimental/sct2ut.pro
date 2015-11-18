;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   sct2ut
;
;*PURPOSE:
;   A procedure to convert spacecraft clock times read from Rosat files 
;   (given in seconds since launch) to UT 
;
;*CALLING SEQUENCE:
;   sct2ut,sctime,obinfo,yr,daynum,utsec,mjd,sctdel=sctdel
;
;*PARAMETERS:
; INPUTS:
;   sct    - spacecraft times given in seconds since launch
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
;   yr     - year
;   daynum - day number of year
;   utsec  - seconds since beginning of day
;   mjd    - modified Julian date (used in conversion)
;   sctdel - vector of corrections to be added to UTS to force obinfo.utsbeg
;            and obinfo.utsend to correspond exactly to obinfo.sctbeg and
;            obinfo.sctend (in sec)
;
;*NOTES: 
;   Obinfo must be supplied
;   Program first converts to MJD, and then to UT date
;
;*SUBROUTINES:
;  DAYCNV
;
;*MODIFICATION HISTORY:
;    written  10 Jan 1994 by GAR
;    modified 19 Jan 1994 (GAR) to correct reference MJD 
;-
;-------------------------------------------------------------------------------
pro sct2ut,sctime,obinfo,yr,daynum,utsec,mjd,sctdel=sctdel
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' SCT2UT, sctime, obinfo, YR, DAYNUM, UTSEC, MJD, SCTDEL=SCTDEL'
  retall
endif
if (npar lt 2) then begin
  print,' SCT2UT requires at least 2 parameters - S/C clock time & obinfo.'
  print,' Returning.'
  retall
endif
;
mjdrefi = 48043L                    ;MJD integer SC clock start
;mjdreff = 8.7973375D-01             ;MJD fraction SC clock start
mjdreff = 8.79745370370074D-01      ;MJD fraction SC clock start
;
ibeg = obinfo.ibeg
iend = obinfo.iend
nobi = n_elements(ibeg)
sctdel = fltarr(nobi)
for ii=0,nobi-1 do begin
  ib = ibeg(ii)
  ie = iend(ii)
;
  mjdb = mjdrefi + mjdreff + obinfo(ii).sctbeg/86400.D0 
  daycnv,mjdb+2400000.5,yr,mn,day,hr
  tdb = obinfo(ii).utsbeg - hr*3600.
  mjde = mjdrefi + mjdreff + obinfo(ii).sctend/86400.D0 
  daycnv,mjde+2400000.5,yr,mn,day,hr
  tde = obinfo(ii).utsend - hr*3600.
  sctdel(ii) = (tdb + tde)/2.               ;take average of difference
  if (!debug gt 5) then print,' SCT2UT: ',ii,tdb,tde,sctdel(ii)
endfor
;
mjd = mjdrefi + mjdreff + sctime/86400.D0     ;Modified Julian Days
daycnv,mjd+2400000.5D0,yr,mn,day,hr  ;find the calendar date for each SC time
;
; Correct UTSEC by the value of SCTDEL for the appropriate OBI
;
utsec = 3600.*hr
check = 0
for ii=0,nobi-1 do begin
  jobi = where( (sctime ge obinfo(ii).sctbeg) and $
                (sctime le obinfo(ii).sctend),nj )
  if (nj gt 0) then utsec(jobi) = temporary( sctdel(ii) + utsec(jobi) )
  check = check + nj
endfor
if (check eq 0) then begin      ;correct using the closest value(s) of SCTDEL
  tabinv,obinfo.sctbeg,sctime,it
  it = fix(it)
  utsec = temporary( sctdel(it) + utsec )
endif
;
ntime = n_elements(sctime)
daynum = intarr(ntime)
for ii=0,ntime-1 do daynum(ii) = ymd2dn(yr(ii),mn(ii),day(ii))
;
return        ;sct2ut
end
