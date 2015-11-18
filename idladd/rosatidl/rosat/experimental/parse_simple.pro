;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       parse_simple
;
;*PURPOSE:
; A procedure to convert an ASCII region descriptor into a string 
; variable containing the region type and a vector containing the region
; arguments
;
;*CALLING SEQUENCE:
;       parse_simple,region,REGTYP,ARGS,INCL
;
;*PARAMETERS:
; INPUTS
;   region   Input ASCII string containing IRAF style region descriptor.
; 
; OUTPUTS
;   regtyp   ASCII string specifying type of region, e.g., circle, box.
;   args     vector containing arguments which specify region. All optional
;            arguments which are not specified in region are set to 0.0.
;   incl     ASCII string specifying whether region is to be included or
;            excluded. If = 'Y' then include, if = 'N' then exclude.
;
;*RESTRICTIONS:
;   This routine can only handle "simple" regions; i.e., n= format,
;   Boolean operations, concatenation of regions by ;'s not allowed. 
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  REPCHR
;  GETTOK
;  GETOPT
;
;*MODIFICATION HISTORY:
;    written 02-09-92 by GAR
;    modified 10 Jul 1993 (GAR) to understand syntax for excluding regions.
;-
;-------------------------------------------------------------------------------
pro parse_simple,region,regtyp,args,incl
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' PARSE_SIMPLE, region, REGTYP, ARGS, INCL'
  return
endif
;
badchar = [';','=','!','&','^','|']
nbad = n_elements(badchar)
;
for ii=0,nbad-1 do begin              ;check for bad characters
  try = strpos(region,badchar(ii))
  if (try ge 0) then begin
    print,' Only simple regions allowed. Returning.'
    regtyp = 'XXX'
    args = [-1]
    return
  endif
endfor
;
try = strmid(strtrim(region,2),0,1)   ;test first nonblank character for '-'
if (try eq '-') then incl = 'N' else incl = 'Y'
;
newreg = repchr(region,'-')           ;replace minus signs by blanks
newreg = repchr(newreg,',')           ;replace commas by blanks
newreg = repchr(newreg,'(')           ;replace parentheses by blanks
newreg = repchr(newreg,')')
try = gettok(newreg,' ')              ;strip off ASCII part of region descriptor
try = strtrim(try,2)
;
types=['annulus','box','circle','ellipse','pie','point','polygon','rotbox']
ntypes = n_elements(types)
nchar = strlen(types)
nargs = [4,5,3,5,4,200,200,5]
;
trytyp = strarr(ntypes)
jmax = max(n_elements(nchar))
jct = 1
for ii=0,ntypes-1 do trytyp(ii) = strmid(types(ii),0,(jct<nchar(ii)) )
;
regtyp = ''
args = [-1]
while ( (jct lt jmax) and (regtyp eq '') ) do begin     ;look for a match
  indtyp = where( strlowcase(try) eq trytyp )     
  if (indtyp(0) ge 0) then begin      ;matching type found
  if (!debug gt 2) then stop,$
     ' Stopping in parse_simple after indtyp defined.'
;
    if (n_elements(indtyp) gt 1) then begin
      print,' Region descriptor    ',try,'    not unique. '
      print,' Matches: ',types(indtyp),' Returning.'
      regtyp = 'XXX'                  ;set regtyp to get out of loop
    endif else begin
      indtyp = indtyp(0) 
      regtyp = types(indtyp)
    endelse
;
  endif else begin                    ;no match found 
    jct = jct + 1
    for ii=0,ntypes-1 do trytyp(ii) = strmid(types(ii),0,(jct<nchar(ii)) )
  endelse
;
endwhile
if (!debug gt 1) then stop,$
   ' Stopping in parse_simple after regtyp defined.'
;
if ( (regtyp ne 'XXX') and (regtyp ne '') ) then begin     ;unique match found
  narg = nargs(indtyp)
  args = fltarr(narg)
  args(0) = getopt(newreg)
  jct = max( where(args ne 0.) )
  if (!debug gt 2) then stop,$
     ' Stopping in parse_simple after args defined.'
  if ( (narg eq 200) and (jct lt (narg-1)) ) then $      ;strip final zeroes
     args = args(0:jct)    
endif
;
return
end           ;pro parse_simple
