;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   get_files
;
;*PURPOSE:
;   A procedure to look for files matching the description fspec
;   Allows user to change fspec if no matching files found
;
;*CALLING SEQUENCE:
;   get_files,fspec,names,nlist
;
;*PARAMETERS:
; INPUTS:
;	fspec - description to be matched (e.g., 'rp150011')
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;       files - vector of file names starting with dir+root.ext
;       nlist - number of files found (= 0 if none were found)
;
;*EXAMPLES:
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*PROCEDURE:
;
;*MODIFICATION HISTORY:
;    based on match_files.pro
;    written 06 June 1992 by GAR
;-
;-------------------------------------------------------------------------------
pro get_files,fspec,files,nlist
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' GET_FILES, fspec, FILES, NLIST'
  retall
endif
;
find = 1                                         ; look for matching files
while (find eq 1) do begin
  files = findfile(fspec,count=nlist)
;
  if (nlist gt 0) then begin                       ; some files were found
    find = 0                                       ; don't look any more
  endif else begin                                 ; no matching files found
    print,' I could not find any files that match ',fspec
    newname = ''
    print,' Please enter a valid root name or type Q to quit'
    read,'    (include the correct directory name) : ',newname
    newname = strtrim(newname,2)
;
    if (strupcase(newname) eq 'Q') then begin
      print,' Returning.'
      retall
    endif else fspec = newname
  endelse
endwhile            ;finished looking for matching files
;
return              ;pro get_files
end
