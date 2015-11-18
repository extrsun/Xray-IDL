pro histvalue,hdr,entry,value,hcount,flag
;
flag = 0
nhdr = n_elements(hdr)
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
  tok = gettok(hdrlin,entry)             ;look for entry to be matched
  if (hdrlin eq '') then hcount = hcount + 1
endwhile
;
tok = gettok(hdrlin,'=')                   ;look for value
if (hdrlin ne '') then value = double(hdrlin) else flag = -1
;
return
end          ;pro histvalue
