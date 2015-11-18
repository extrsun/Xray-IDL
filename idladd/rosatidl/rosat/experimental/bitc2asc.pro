function bitc2asc,bitcode,flags,setvals,ncheck
;
ntot = strlen(bitcode(0))
if (n_elements(ncheck) eq 0) then ncheck = ntot     ;default is to check all
ascii = ''
;
nset = 0
for jj=0,ncheck-1 do begin
  flagj = strmid(bitcode,ntot-1-jj,1)      ;extract jth bit from bit 0
  if (flagj eq setvals(jj)) then begin
    ascii = flags(jj) + ', ' + ascii
    nset = nset + 1
  endif
endfor
;
if (nset eq 0) then ascii = 'No flags set' else begin
  len = strlen(ascii)
  ascii = strmid(ascii,0,len-2)            ;strip off final comma at end
endelse
;
; add symbol to show end of ASCII translation
;
ascii = ascii + '  //'                   
;
return,ascii
end               ;function bitc2asc
