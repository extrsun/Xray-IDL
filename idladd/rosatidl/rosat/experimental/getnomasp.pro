pro getnomasp,hdr,nomra,nomdec,nomroll,proc=proc
;
if (n_elements(proc) eq 0) then proc = 'US'
procuc = strupcase(proc)
if ( (procuc ne 'US') and (procuc ne 'MPE') ) then begin
  print,proc,' is not a valid Rosat processing format.'
  print,' Valid options are US, MPE. Returning.'
  retall
endif
;
flag = 0
nhdr = n_elements(hdr)
hcount = 0
hdrlin = ''
while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
  hdrlin = hdr(hcount)
  tok = gettok(hdrlin,'HISTORY')         ;look for HISTORY lines
  if (hdrlin eq '') then hcount = hcount + 1
endwhile
;
hdrlin = ''
while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
  hdrlin = hdr(hcount)
  entry = 'Nominal Observation RA'       
  if (procuc eq 'MPE') then entry = 'POINT_LONG'
  tok = gettok(hdrlin,entry)             ;look for nominal RA
  if (hdrlin eq '') then hcount = hcount + 1
endwhile
if (procuc eq 'MPE') then begin
  hdrlin = strtrim( hdr(hcount+1),2 )   ;value is on the next line down
  tok = gettok(hdrlin,' ')
  hdrlin = strtrim(hdrlin,2)
  tok = gettok(hdrlin,'H')
  hh = fix(tok)
  tok = gettok(hdrlin,'M')
  mm = fix(tok)
  tok = gettok(hdrlin,'S')
  ss = float(tok)
  nomra = double(ten(hh,mm,ss))*15.*3600.*2.
endif else begin
  tok = gettok(hdrlin,'=')                   ;look for value
  if (hdrlin ne '') then nomra = double(hdrlin) 
endelse
;
hdrlin = ''
while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
  hdrlin = hdr(hcount)
  entry = 'Nominal Observation DEC'
  if (procuc eq 'MPE') then entry = 'POINT_LAT'
  tok = gettok(hdrlin,entry)             ;look for nominal Dec
  if (hdrlin eq '') then hcount = hcount + 1
endwhile
if (procuc eq 'MPE') then begin
  hdrlin = strtrim( hdr(hcount+1),2 )   ;value is on the next line down
  tok = gettok(hdrlin,' ')
  hdrlin = strtrim(hdrlin,2)
  tok = gettok(hdrlin,'D')
  dd = fix(tok)
  tok = gettok(hdrlin,'M')
  mm = fix(tok)
  tok = gettok(hdrlin,'S')
  ss = float(tok)
  nomdec = double(ten(dd,mm,ss))*3600.*2.
endif else begin
  tok = gettok(hdrlin,'=')                   ;look for value
  if (hdrlin ne '') then nomdec = double(hdrlin) 
endelse
;
if (procuc eq 'MPE') then begin      ;no value given in MPE FITS header
  nomroll = 0.0D0
endif else begin
  hdrlin = ''
  while ( (hdrlin eq '') and (hcount lt nhdr) ) do begin
    hdrlin = hdr(hcount)
    entry = 'Nominal Observation Roll Angle'
    tok = gettok(hdrlin,entry)             ;look for nominal roll angle
    if (hdrlin eq '') then hcount = hcount + 1
  endwhile
  tok = gettok(hdrlin,'=')                   ;look for value
  if (hdrlin ne '') then nomroll = double(hdrlin) /2./3600.    ;return in deg
endelse
;
return
end          ;pro getnomasp
