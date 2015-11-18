function sign,number,option=option
;
;+
; Returns -1,0,1 for NUMBER<0, NUMBER=0, NUMBER>0 respectively if OPTION is
;   not set.
;
; Returns -1,1 for NUMBER<0, NUMBER>=0 respectively if OPTION is set.
;
; Will work on arrays and anything other than string or complex variables.
;-
;
if n_params(0) lt 1 then return,0
if keyword_set(option) then return, (number ge 0)*1 - (number lt 0)*1 $
   else return, (number gt 0)*1 - (number lt 0)*1
end

