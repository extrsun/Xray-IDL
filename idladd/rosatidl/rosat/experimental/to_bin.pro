FUNCTION TO_BIN, D, NCHAR
;+
; NAME:
;   TO_BIN
; PURPOSE:
;   Translate a non-negative decimal integer to a binary string
; CALLING SEQUENCE:
;   BINARY = TO_BIN(D,[NCHAR])
; INPUTS:
;   D - non-negative decimal integer, scalar.  All leading blanks are
;       removed.
; OPTIONAL INPUT:
;   NCHAR - number of characters in the output hexadecimal string.
;           If not supplied, then the hex string will contain no 
;           leading zeros.
; OUTPUT:
;   BINARY - hexadecimal translation of input integer, string
; EXAMPLES:
;   A = TO_BIN(11)    ==>   A = '1011'
;   A = TO_BIN(100,10) ==>   A = '0001100100'
; METHOD:
;   TO_HEX is used to convert to a hexidecimal string.
;   No parameter checking is done.
; REVISION HISTORY:
;   Adapted from TO_HEX  (W. Landsman) 22 Nov 1991 (GAR)
;   Modified to allow string arrays with more than 128 elements
;-
hextobin = ['0000','0001','0010','0011','0100','0101','0110','0111','1000'$
           ,'1001','1010','1011','1100','1101','1110','1111']
sd = size(d) 
nd = sd(n_elements(sd)-3)
if (nd eq 0) then dbin = '' else dbin = strarr(nd)
;
dhex = to_hex_large(d)
if (nd eq 0) then begin
  hexlen = strlen(dhex)
  for ii=0,hexlen-1 do begin
    term = strmid(dhex,ii,1)
    if (term eq 'A') then term = '10'
    if (term eq 'B') then term = '11'
    if (term eq 'C') then term = '12'
    if (term eq 'D') then term = '13'
    if (term eq 'E') then term = '14'
    if (term eq 'F') then term = '15'
    dbin = dbin + hextobin(fix(term))
  endfor
;
  if (n_elements(nchar) ne 0) then begin
    nadd = nchar - strlen(dbin)
    if (nadd gt 0) then for ii=0,nadd-1 do dbin = '0' + dbin
  endif
;
endif else begin
;
  for nn = 0,nd-1 do begin
    hexlen = strlen(dhex(nn))
    for ii=0,hexlen-1 do begin
      term = strmid(dhex(nn),ii,1)
      if (term eq 'A') then term = '10'
      if (term eq 'B') then term = '11'
      if (term eq 'C') then term = '12'
      if (term eq 'D') then term = '13'
      if (term eq 'E') then term = '14'
      if (term eq 'F') then term = '15'
      dbin(nn) = dbin(nn) + hextobin(fix(term))
    endfor
;
    if (n_elements(nchar) ne 0) then begin
      nadd = nchar - strlen(dbin(nn))
      if (nadd gt 0) then for ii=0,nadd-1 do dbin(nn) = '0' + dbin(nn)
    endif
;
  endfor
endelse  
;
RETURN,dbin
END
