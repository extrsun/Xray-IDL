pro rsbegtimes,h,sctbeg,utbeg,utend
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSBEGTIMES, h, SCTBEG, UTBEG, UTEND'
  return
endif
;
utbeg = fltarr(3)                 ;year, day number of year, seconds of day
utend = utbeg
;
nhdr = n_elements(h)
hcount = 0
hdrlin = ''
while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
  hdrlin = h(hcount)
  tok = gettok(hdrlin,'HISTORY')         ;look for HISTORY lines
  if (hdrlin eq '') then hcount = hcount + 1
endwhile
;
hdrlin = ''
while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
  hdrlin = h(hcount)
  tok = gettok(hdrlin,'OBI start')         ;look for OBI sct start times
  if (hdrlin eq '') then hcount = hcount + 1
endwhile
;
tok = gettok(hdrlin,'=')                   ;look for time 
if (hdrlin ne '') then sctbeg = double(hdrlin) else sctbeg = 0.
;
if (npar ge 2) then begin                  ;also want UT beg times
  hdrlin = ''
  while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
    hdrlin = h(hcount)
    tok = gettok(hdrlin,'OBI start time UTC')    ;look for OBI UT start times
    if (hdrlin eq '') then hcount = hcount + 1
  endwhile
;
  tok = gettok(hdrlin,'=')                   ;look for time 
  if (hdrlin ne '') then utb = double(hdrlin) else utb = 0.
;
  hdrlin = ''
  while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
    hdrlin = h(hcount)
    tok = gettok(hdrlin,'OBI start within')    ;look for millisec
    if (hdrlin eq '') then hcount = hcount + 1
  endwhile
;
  tok = gettok(hdrlin,'=')                   ;look for time 
  if (hdrlin ne '') then utsec = double(hdrlin)/1000. else utsec = 0.
;
  yy = fix(utb/1.e4)  
  mm = fix( (utb-yy*1.e4)/1.e2)
  dd = fix(utb-yy*1.e4-mm*1.e2)
  yy = yy + 1900
  dayn = ymd2dn(yy,mm,dd)
;
  utbeg(0) = yy
  utbeg(1) = dayn
  utbeg(2) = utsec
endif
;
if (npar ge 3) then begin                  ;also want UT end times
  hdrlin = ''
  while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
    hdrlin = h(hcount)
    tok = gettok(hdrlin,'OBI end time UTC')    ;look for OBI UT end times
    if (hdrlin eq '') then hcount = hcount + 1
  endwhile
;
  tok = gettok(hdrlin,'=')                   ;look for time 
  if (hdrlin ne '') then utb = double(hdrlin) else utb = 0.
;
  hdrlin = ''
  while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
    hdrlin = h(hcount)
    tok = gettok(hdrlin,'OBI end within')    ;look for millisec
    if (hdrlin eq '') then hcount = hcount + 1
  endwhile
;
  tok = gettok(hdrlin,'=')                   ;look for time 
  if (hdrlin ne '') then utsec = double(hdrlin)/1000. else utsec = 0.
;
  yy = fix(utb/1.e4)  
  mm = fix( (utb-yy*1.e4)/1.e2)
  dd = fix(utb-yy*1.e4-mm*1.e2)
  yy = yy + 1900
  dayn = ymd2dn(yy,mm,dd)
;
  utend(0) = yy
  utend(1) = dayn
  utend(2) = utsec
endif
;
return
end         ;pro rsbegtimes
