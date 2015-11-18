;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;				gethistval
;
;*PURPOSE:
; A procedure to read a FITS header and return the value of a quantity
; given in a history record. The quantity is specified by a string variable
; to be matched.
;
;*CALLING SEQUENCE:
;	gethistval,hdr,match,lnum,token=token,chatter=chatter
;
;*PARAMETERS:
; INPUTS:
;	hdr     - FITS header
;       match   - string containing text to be matched
;       token   - optional separator between match and value. i.e., could be
;                 "=". If not given, then program returns rest of string
;                 after text to be matched.
;       chatter - Controls program feedback to user (default = 1)
;
; OUTPUTS:
;       value   - value of quantity given in specified history record.
;       lnum    - the line number containing the matched string.
;                 if lnum = -1, then no elements are returned.
;                 if n_elements(lnum) > 1, then the program returns the
;                 string remainders for each line.
;
;*RESTRICTIONS
;       none
;
;*MODIFICATION HISTORY:
;	written 14 Feb 1992 by GAR
;-
;-------------------------------------------------------------------------------
pro gethistval,hdr,match,value,lnum,token=token,chatter=chatter
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' gethistval, hdr, match, VALUE, LNUM, token=token, chatter=chatter'
  return
endif
if (n_elements(chatter) eq 0) then chatter = 1
;
check = 0
nhdr = n_elements(hdr)
if (nhdr eq 0) then begin
  print,' FITS header not defined. Returning.'
  value = 0
  lnum = -1
  check = -1
endif
;
; first check that there are history lines in the header
;
pos = strpos(hdr,'HISTORY')
lnum = where(pos ge 0)
if (lnum(0) lt 0) then begin
  if (chatter eq 1) then $
     print,' No history records in this header? Returning.'
  value = 0
  lnum = -1
  check = -1
endif 
;  
; now look for matching text string in header
;
if (check ge 0) then begin
  pos = strpos(hdr,match)
  lnum = where(pos ge 0)
  if  ((lnum(0) lt 0)) then begin
    if (chatter eq 1) then $
       print,' Specified string not found in history records. Returning.'
    value = 0
    check = -1
  endif 
endif
;
if (check ge 0) then begin
  nmatch = n_elements(lnum)
  pos = pos(lnum)
  if (nmatch gt 1) then begin
    if (chatter eq 1) then $
       print,'Text string found in more than one history record.'
    value = strarr(nmatch)
    for ii=0,nmatch-1 do begin
      hdrlin = hdr(lnum(ii))
      nhc = strlen(hdrlin)
      nmc = strlen(match)
      value(ii) = strmid(hdrlin,pos(ii)+nmc,nhc - (pos(ii)+nmc))
    endfor
  endif else begin
;
    notok = 0
    if (n_elements(token) eq 0) then notok = 1
    hdrlin = hdr(lnum(0))
    if (notok eq 1) then begin            ;no token given, return string
      nhc = strlen(hdrlin)
      nmc = strlen(match)
      value = strmid(hdrlin,pos(0)+nmc,nhc - (pos(0)+nmc))
    endif else begin
      tok = gettok(hdrlin,token)    ;strip off rest
      value = double(hdrlin) 
    endelse
  endelse
endif
;
return
end         ;pro gethistval
