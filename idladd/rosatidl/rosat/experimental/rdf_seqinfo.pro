;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rdf_seqinfo
;
;*PURPOSE:
; A procedure to read sequence information from the sequence header for
; a ROSAT (RDF format) FITS file.
;
;*CALLING SEQUENCE:
;       rdf_seqinfo,fitsname,NOBI,ACTIM,NOMASP,SEQBEG,SEQEND,OBIBEG,OBIEND,$
;                   DTCOR,ONTIME
;
;*PARAMETERS:
; INPUTS:
;	fitsname - Full name of FITS file including directory & extension
;
; OUTPUTS:
;       nobi -   Number of separate observation intervals in sequence
;       actim -  Sequence total active time (in seconds)
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
;       dtcor  - Dead time correction factor
;       ontime - Total on time (seconds)
;
;*MODIFICATION HISTORY:
;	written  16 Dec 1993 by GAR (adapted from us_seqinfo)
;-
;-------------------------------------------------------------------------------
pro rdf_seqinfo,fitsname,nobi,actim,nomasp,seqbeg,seqend,obibeg,obiend,dtcor,$
                ontime
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDF_SEQINFO, fitsname, NOBI, ACTIM, NOMASP, SEQBEG, SEQEND, '
  print,'              OBIBEG, OBIEND, DTCOR, ONTIME'
  return
endif
;
hdr = headfits(fitsname)          ;this should be the primary header
;
if (npar ge 2) then nobi = fxpar(hdr,'NUM_OBIS')       ;look for number of OBIs
;
if (npar ge 3) then actim = fxpar(hdr,'LIVETIME')      ;look for active time
;
if (npar ge 4) then begin                ;aspect information comes first
  nomasp = dblarr(3)
  value = fxpar(hdr,'RA_NOM')
  nomasp(0) = value*3600.*2.         ;nominal RA returned in 0.5 arcsec pixels
;
  value = fxpar(hdr,'DEC_NOM')
  nomasp(1) = value*3600.*2.         ;nominal Dec returned in 0.5 arcsec pixels
;
  value = fxpar(hdr,'ROLL_NOM')      ;nominal roll returned in dec. degrees
  nomasp(2) = value
endif          ;of getting nomasp
;
if (npar ge 5) then begin                  ;look for sequence start times
  seqbeg = dblarr(4)
  value = fxpar(hdr,'SCSEQBEG')            ;sequence start time in SC sec
  seqbeg(0) = value
;
  value = fxpar(hdr,'TIME-OBS')
  hh = gettok(value,':')
  mm = gettok(value,':')
  utsec = hh*3600. + mm*60. + value
;
  value = fxpar(hdr,'DATE-OBS')
  dd = gettok(value,'/')
  mm = gettok(value,'/')
  yy = 1900 + value
  dayn = ymd2dn(yy,mm,dd)
;
  seqbeg(1) = yy
  seqbeg(2) = dayn
  seqbeg(3) = utsec
endif             ;of getting sequence start in spacecraft time
;
if (npar ge 6) then begin                  ;look for sequence stop times
  seqend = dblarr(4)
  value = fxpar(hdr,'SCSEQEND')            ;sequence stop time in SC sec
  seqend(0) = value
;
  value = fxpar(hdr,'TIME_END')
  hh = gettok(value,':')
  mm = gettok(value,':')
  utsec = hh*3600. + mm*60. + value
;
  value = fxpar(hdr,'DATE_END')
  dd = gettok(value,'/')
  mm = gettok(value,'/')
  yy = 1900 + value
  dayn = ymd2dn(yy,mm,dd)
;
  seqend(1) = yy
  seqend(2) = dayn
  seqend(3) = utsec
endif             ;of getting sequence end date & time in UT
;
if (npar ge 7) then begin                  ;also want OBI beg times
  obibeg = dblarr(4,nobi)
;
  for jj=0,nobi-1 do begin
    ftype = 'OBIB'+strtrim(string(jj+1),2)
    value = fxpar(hdr,ftype)
    obibeg(0,jj) = double(value)
;
    ftype = 'BTIM'+strtrim(string(jj+1),2)
    value = fxpar(hdr,ftype)
    hh = gettok(value,':')
    mm = gettok(value,':')
    utsec = hh*3600. + mm*60. + value
;
    ftype = 'BDAY'+strtrim(string(jj+1),2)
    value = fxpar(hdr,ftype)
    dd = gettok(value,'/')
    mm = gettok(value,'/')
    yy = 1900 + value
    dayn = ymd2dn(yy,mm,dd)
;
    obibeg(1,jj) = yy
    obibeg(2,jj) = dayn
    obibeg(3,jj) = utsec
  endfor          ;of getting OBI start times for each OBI
endif             ;of getting OBI start times
;  
if (npar ge 8) then begin
  obiend = dblarr(4,nobi)
;
  for jj=0,nobi-1 do begin
    ftype = 'OBIE'+strtrim(string(jj+1),2)
    value = fxpar(hdr,ftype)
    obiend(0,jj) = double(value)
;
    ftype = 'ETIM'+strtrim(string(jj+1),2)
    value = fxpar(hdr,ftype)
    hh = gettok(value,':')
    mm = gettok(value,':')
    utsec = hh*3600. + mm*60. + value
;
    ftype = 'EDAY'+strtrim(string(jj+1),2)
    value = fxpar(hdr,ftype)
    dd = gettok(value,'/')
    mm = gettok(value,'/')
    yy = 1900 + value
    dayn = ymd2dn(yy,mm,dd)
;
    obiend(1,jj) = yy
    obiend(2,jj) = dayn
    obiend(3,jj) = utsec
  endfor          ;of getting OBI stop times for each OBI
endif             ;of getting OBI stop times
;
if (npar ge 9) then dtcor = fxpar(hdr,'DTCOR')     ;dead time correction factor
;
if (npar ge 10) then ontime = fxpar(hdr,'ONTIME')  ;total on time (seconds)  
;
if (!debug gt 3) then stop,'stopping at end of program'
return
end         ;pro rdf_seqinfo
