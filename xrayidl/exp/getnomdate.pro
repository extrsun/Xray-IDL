pro getnomdate,hdr,yy,mm,dd
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
   entry = 'OBS_DATE'
  tok = gettok(hdrlin,entry)             ;look for nominal RA
  if (hdrlin eq '') then hcount = hcount + 1
endwhile
month=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
  hdrlin = strtrim( hdr(hcount+1),2 )   ;value is on the next line down
  tok = gettok(hdrlin,' ')
  hdrlin = strtrim(hdrlin,2)
  tok = gettok(hdrlin,'-')
  dd = fix(tok)
  tok = gettok(hdrlin,'-')
  sel=where(tok eq month,nsel)
  if nsel ne 1 then stop,'No month of the obs_date is found in the header'
  mm = sel(0)+1
  tok = gettok(hdrlin,' ')
  yy = fix(tok)
;
return
end          ;pro getnomdate
