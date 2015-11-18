;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
; NAME:
;    FINDSTR
; PURPOSE:
;    Find all occurences of one substring within another.
; CALLING SEQUENCE:
;    result = findstr(object,sub)
; INPUT PARAMETERS:
;    object = object string 
;    sub    = substring of 'obj' to be found
; OUTPUT PARAMETERS:
;    Result returned as function value.  
; PROCEDURE:
;    Searches for 'sub'. Repeats until all are found.
; EXAMPLE:
;    If a = 'I am what I am' then print,findstr(a,'am')
;    will give [2,12].
; MODIFICATION HISTORY:
;    written by GAR  2 Feb 1992. Based on REPSTR (RS Hill, 12 April 1989).
;-
;-------------------------------------------------------------------------------
function findstr,object,sub
;
l1 = strlen(sub)
last_pos = 0
lo = strlen(object)
pos = 0
found = [-1]
;
while ( (pos lt lo-l1) and (pos ge 0) ) do begin
  pos = strpos(object,sub,last_pos)
  if (pos ge 0) then found = [found,pos]
  last_pos = pos + l1
endwhile
if (n_elements(found) gt 1) then found = found(1:*)     ;string was found
;
return,found
end
