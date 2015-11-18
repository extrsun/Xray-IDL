;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; source_excl
;
;*PURPOSE:
; divide a list of sources into two separate lists, which is useful for example; in subtracting some sources from an image.
;
;*CALLING SEQUENCE:
; source_excl,sid,sra,sdec,sidexcl,sraexcl,sdecexcl
;
;*PARAMETERS:
; INPUTS:
; sid  - a vector containing a list of source IDs 
; sra, sdec - the RA and DEC of the sources in the list
; sidexcl - a vecter containing the ID(s) of the source(s) to be
; 		subtracted from the list
;
;
;
;
;*OPTIONAL OUTPUTS:
; sraecl, sdecexcl - the list of sources contained in sidexcl
;
;*PROCEDURE:
; obvious
;
;*EXAMPLES:
; source_excl,sid,sra,sdec,[8],sraexcl,sdecexcl
;  resulting a list of sources excluding the source number 8
;
;*MODIFICATION HISTORY:
; writen Sept 1992 (WQD)
;
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro source_excl,sid,sra,sdec,sidexcl,sraexcl,sdecexcl
;
if n_params() eq 0 then begin
print,'CALL SEQUENCE - source_excl,sid,sra,sdec,sidexcl [,sraexcl,sdecexcl]'
return
endif
;
match,sid,sidexcl,sidsub
if n_elements(sidsub) ne 0 then begin
  print,n_elements(sidsub), ' sources have been removed. They are:'
  print,sid(sidsub)
  if n_params() gt 4 then begin ;optional outputs
	sraexcl=sra(sidsub)
	sdecexcl=sdec(sidsub)
  endif
  remove,sidsub,sid,sra,sdec
endif else print,'No source in the exclusion list'
if !debug eq 3 then stop
end

