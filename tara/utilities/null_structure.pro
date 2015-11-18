;=============================================================================
; $Id: null_structure.pro 4298 2012-07-02 18:23:59Z psb6 $

; Utility to set all fields of a structure to an appropriate "null" value. 

; Patrick Broos, March 2011

;=============================================================================

FUNCTION null_structure, input_structure, $
                         BYTE_NULL   =byte_null  , $
                         STRING_NULL =string_null, $
                         INT_NULL    =int_null   , $
                         UINT_NULL   =uint_null  , $
                         LONG_NULL   =long_null  , $
                         FLOAT_NULL  =float_null 

if (n_elements(byte_null)   EQ 0) then byte_null   = 0B
if (n_elements(string_null) EQ 0) then string_null = '...'
if (n_elements(int_null)    EQ 0) then int_null    = -99
if (n_elements(uint_null)   EQ 0) then uint_null   = 0U
if (n_elements(long_null)   EQ 0) then long_null   = -99L
if (n_elements(float_null)  EQ 0) then float_null  = !VALUES.F_NAN


str = input_structure

for ii=0,n_tags(str)-1 do begin
  case   size(str.(ii), /TNAME)   of
    'STRUCT': str.(ii) = null_structure( str.(ii), $
                                         BYTE_NULL   =byte_null  , $
                                         STRING_NULL =string_null, $
                                         INT_NULL    =int_null   , $
                                         UINT_NULL   =uint_null  , $
                                         LONG_NULL   =long_null  , $
                                         FLOAT_NULL  =float_null )
    'BYTE'  : str.(ii) = byte_null  
    'STRING': str.(ii) = string_null
    'INT'   : str.(ii) = int_null   
    'UINT'  : str.(ii) = uint_null   
    'LONG'  : str.(ii) = long_null  
    else    : str.(ii) = float_null 
  endcase
endfor

return, str
end

