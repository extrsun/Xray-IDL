;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;				rsgetseqinfo
;
;*PURPOSE:
; A procedure to read sequence information from the sequence header for
; a ROSAT FITS file.
;
;*CALLING SEQUENCE:
;	rsgetseqinfo, hdr, NOBI, ACTIM, NOMASP, SEQBEG, SEQEND, OBIBEG, OBIEND
;
;*PARAMETERS:
; INPUTS:
;	hdr - FITS header
;
; OUTPUTS:
;       nobi - Number of separate observation intervals in sequence
;       actim - Sequence total active time (in seconds)
;       nomasp - 3 element vector containing nominal RA, Dec and Roll for
;                sequence (RA and Dec in 0.5 arcsec pixels, Roll in degrees)
;       seqbeg - 4 element vector containing sequence start time in
;                spacecraft time (seconds) and in UT (year, day number of 
;                day number of year, and sceonds of day).
;       seqend - 4 element vector containing sequence stop time in
;                spacecraft time (seconds) and in UT (year, day number of 
;                day number of year, and sceonds of day).
;       obibeg - dates & times of beginnings of obi segments
;       obiend - dates & times of ends of obi segments
;                These are 2D arrays consisting of 4 rows. Each row
;                contains as many elements as there are obi segments.
;                Row 0 contains the spacecraft time (in sec).
;                Row 1 contains the year of observation (e.g., 1990).
;                Row 2 contains the day number of the year (e.g., 207).
;                Row 3 contains the UT observation time (in sec). 
;
;*MODIFICATION HISTORY:
;	written 07 Oct 1991 by GAR
;       modified 14 Feb 1992 to use GETHISTVAL (GAR)
;       modified 01 Mar 1992 to use chatter=0 in call to GETHISTVAL (GAR)
;       modified 22 Oct 1992 to fix bug when file contains >9 OBIs (GAR)
;-
;-------------------------------------------------------------------------------
pro rsgetseqinfo,hdr,nobi,actim,nomasp,seqbeg,seqend,obibeg,obiend
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGETSEQINFO, hdr, NOBI, ACTIM, NOMASP, SEQBEG, SEQEND, '
  print,'               OBIBEG, OBIEND'
  return
endif
;
gethistval,hdr,'Sequence',value,lnum,tok=token,ch=0
pos = max(lnum)
if (pos lt 0) then begin
  print,' This header does not contain sequence info. Returning.'
  return
endif 
;
nhdr = n_elements(hdr)
token = '='
if (npar ge 4) then begin                ;aspect information comes first
  nomasp = dblarr(3)
  gethistval,hdr,'Nominal Observation RA',value,lnum,tok=token,ch=0
  nomasp(0) = value
;
  gethistval,hdr,'Nominal Observation DEC',value,lnum,tok=token,ch=0
  nomasp(1) = value
;
  gethistval,hdr,'Nominal Observation Roll Angle',value,lnum,tok=token,ch=0
  nomasp(2) = value/2./3600.    ;return in deg
endif          ;of getting nomasp
;
if (npar ge 3) then $                      ;look for active time
  gethistval,hdr,'Sequence total active time',actim,lnum,tok=token,ch=0
;
if (npar ge 2) then $                      ;look for number of OBI
  gethistval,hdr,'Number of separate observation intervals',nobi,lnum,$
             tok=token,ch=0
;
if (npar ge 5) then begin                  ;look for sequence start times
  seqbeg = dblarr(4)
  gethistval,hdr,'Sequence start time in spacecraft time',value,lnum,$
             tok=token,ch=0
  seqbeg(0) = value
;
  gethistval,hdr,'Sequence start time UTC',utyr,lnum,tok=token,ch=0
  gethistval,hdr,'Sequence start within YYMMDD',utsec,lnum,tok=token,ch=0
  utsec = utsec/1000.
;
  yy = fix(utyr/1.e4)  
  mm = fix( (utyr-yy*1.e4)/1.e2)
  dd = fix(utyr-yy*1.e4-mm*1.e2)
  yy = yy + 1900
  dayn = ymd2dn(yy,mm,dd)
;
  seqbeg(1) = yy
  seqbeg(2) = dayn
  seqbeg(3) = utsec
endif             ;of getting sequence start in spacecraft time
;
if (npar ge 6) then begin                  ;look for sequence stop times
  seqend = dblarr(4)
  gethistval,hdr,'Sequence end time in spacecraft time',value,lnum,$
             tok=token,ch=0
  seqend(0) = value
;
  gethistval,hdr,'Sequence end time UTC',utyr,lnum,tok=token,ch=0
  gethistval,hdr,'Sequence end within YYMMDD',utsec,lnum,tok=token,ch=0
  utsec = utsec/1000.
;
  yy = fix(utyr/1.e4)  
  mm = fix( (utyr-yy*1.e4)/1.e2)
  dd = fix(utyr-yy*1.e4-mm*1.e2)
  yy = yy + 1900
  dayn = ymd2dn(yy,mm,dd)
;
  seqend(1) = yy
  seqend(2) = dayn
  seqend(3) = utsec
endif             ;of getting sequence end date & time in UT
;
if (npar ge 7) then begin                  ;also want OBI beg times
  obibeg = dblarr(4,nobi)            ;for convenience, will transpose at end
  if (npar ge 8) then obiend = obibeg
;
  gethistval,hdr,'OBI',value,lnum,ch=0
  hcount = max(lnum)
;
  for jj=0,nobi-1 do begin
    hcount = hcount + 1
    split = strtrim(string(jj+1),2)
    nfix = strlen(split) - 1
;
    hdrlin = hdr(hcount)
    if (!debug gt 1) then print,hcount,hdrlin
;
;    tok = gettok(hdrlin,strtrim(string(jj+1),2))  
    tok = gettok(hdrlin,split)                   ;sct OBI jj+1 start, end time
    hdrlin = strmid(hdrlin,nfix,strlen(hdrlin)-nfix)    ;in case >9 OBIs
    hdrlin = strtrim(hdrlin,2)                   ;strip off extra spaces
    tok = gettok(hdrlin,' ')
    obibeg(0,jj) = double(tok)
    if (npar ge 8) then obiend(0,jj) = double(strtrim(hdrlin,2))
    if (!debug gt 1) then $
       stop,'Stopping after defining obibeg,obiend'
;  
    hcount = hcount + 1
    hdrlin = hdr(hcount)

;    tok = gettok(hdrlin,strtrim(string(jj+1),2))  
    tok = gettok(hdrlin,split)                   ;sct OBI jj+1 start, end time
    hdrlin = strmid(hdrlin,nfix,strlen(hdrlin)-nfix)    ;in case >9 OBIs
    hdrlin = strtrim(hdrlin,2)                   ;strip off extra spaces
    tok = gettok(hdrlin,' ')
    utyr = double(tok)
    hdrlin = strtrim(hdrlin,2)                   ;strip off extra spaces
    tok = gettok(hdrlin,' ')
    utsec = double(tok)/1000.
;
    yy = fix(utyr/1.e4)  
    mm = fix( (utyr-yy*1.e4)/1.e2)
    dd = fix(utyr-yy*1.e4-mm*1.e2)
    yy = yy + 1900
    if (!debug gt 1) then $
       stop,'Stopping after defining yy,mm,dd for obibeg'
    dayn = ymd2dn(yy,mm,dd)
;
    obibeg(1,jj) = yy
    obibeg(2,jj) = dayn
    obibeg(3,jj) = utsec
;
    if (npar ge 8) then begin
      hdrlin = strtrim(hdrlin,2)                   ;strip off extra spaces
      tok = gettok(hdrlin,' ')
      utyr = double(tok)
      hdrlin = strtrim(hdrlin,2)                   ;strip off extra spaces
      tok = gettok(hdrlin,' ')
      utsec = double(tok)/1000.
;
      yy = fix(utyr/1.e4)  
      mm = fix( (utyr-yy*1.e4)/1.e2)
      dd = fix(utyr-yy*1.e4-mm*1.e2)
      yy = yy + 1900
    if (!debug gt 1) then $
       stop,'Stopping after defining yy,mm,dd for obiend'
      dayn = ymd2dn(yy,mm,dd)
;
      obiend(1,jj) = yy
      obiend(2,jj) = dayn
      obiend(3,jj) = utsec
    endif         ;of getting OBI end time for OBI jj+1
  endfor          ;of getting OBI times for each OBI
endif             ;of getting OBI beg & end times
;
if (!debug gt 0) then stop,'stopping at end of program'
return
end         ;pro rgetseqinfo
