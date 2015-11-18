FUNCTION TO_HEX_LARGE, D, NCHAR
;+
; NAME:
;   TO_HEX_LARGE
; PURPOSE:
;   Translate a non-negative decimal integer to a hexadecimal string
; CALLING SEQUENCE:
;   HEX = TO_HEX_LARGE(D,[NCHAR])
; INPUTS:
;   D - non-negative decimal integer, scalar.  All leading blanks are
;       removed.
; OPTIONAL INPUT:
;   NCHAR - number of characters in the output hexadecimal string.
;           If not supplied, then the hex string will contain no 
;           leading zeros.
; OUTPUT:
;   HEX - hexadecimal translation of input integer, string
; EXAMPLES:
;   A = TO_HEX_LARGE(11)    ==>   A = 'B'
;   A = TO_HEX_LARGE(100,3) ==>   A = '064'
; METHOD:
;   The hexadecimal format code '(Z)' is used to convert.  No parameter
;   checking is done.
; REVISION HISTORY:
;   Same as TO_HEX by W. Landsman, except that it also works for string
;   arrays with > 128 elements
;   Written   W. Landsman         November, 1990
;   modified to handle large string arrays Nov 25 1991 (GAR)
;-
sd = size(d) 
nd = sd(n_elements(sd)-3)
if (nd eq 0) then dhex = '' else dhex = strarr(nd)
;
if (n_elements(nchar) eq 0) then format = '(Z)' else begin
      ch = strtrim(nchar,2)
      format = '(z' + ch +'.' + ch + ')'
endelse
;
if (nd lt 128) then dhex = strtrim(string(d, form = format),2) $
   else begin
  nsets = fix(nd/128)
  nleft = nd - nsets*128
  if (nleft ne 0) then nsets = nsets + 1
;
  for jj=0,nsets-1 do begin
    jbeg = jj*128
    jend = (jbeg + 128) < (nd - 1)
    dhex(jbeg) = strtrim(string(d(jbeg:jend), form = format),2) 
  endfor
end
;
return,dhex
end
