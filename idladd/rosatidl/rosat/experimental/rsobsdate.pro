;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsobsdate
;
;*PURPOSE:
; A procedure to read the observation date from the Rosat photon list FITS
; file headers (e.g., rp123456.fits or wp123456_events.tfits)
;
;*CALLING SEQUENCE:
;	rsobsdate, hdr, obsdate, proc=proc
;
;*PARAMETERS:
; INPUTS:
;	hdr - FITS header from photon list FITS file
;
; OPTIONAL INPUTS:
;       proc - format of processed files. Choices are US, MPE, RDF.
;              if not specified, US is assumed.
;
; OUTPUTS:
;       obsdate - Observation date, in the format yymmdd.
;                 e.g., 910125.
;
;*MODIFICATION HISTORY:
;    written  11 Aug 1993 by GAR
;    modified 16 Dec 1993 (GAR) to read info from RDF format headers
;-
;-------------------------------------------------------------------------------
pro rsobsdate,hdr,obsdate,proc=proc
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSOBSDATE, hdr, OBSDATE, proc=proc (US)'
  return
endif
if (n_elements(proc) eq 0) then proc = 'US'
months = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT',$
          'NOV','DEC']
;
procuc = strupcase(proc)
if ( (procuc ne 'US') and (procuc ne 'MPE') and (procuc ne 'MPEUS') and (procuc ne 'RDF')) then begin
  print,proc,' is not a valid choice for processing format.'
  print,' Valid choices are US, MPE, MPEUS RDF. Returning.'
endif
;
case procuc of
  'US':  begin
         date = sxpar(hdr,'DATE-OBS')
         day = gettok(date,'/')
         mon = gettok(date,'/')
         obsdate = fix(date)*1.e4 + fix(mon)*1.e2 + fix(day)    ;e.g., 910125
         end
;
 'MPEUS': begin ;added by wqd
         date = sxpar(hdr,'DATE-OBS')
         day = gettok(date,'/')
         mon = gettok(date,'/')
         obsdate = fix(date)*1.e4 + fix(mon)*1.e2 + fix(day)    ;e.g., 910125
         end
  'MPE': begin
         token = '='
         matchstr = 'OBS_DATE'
         gethistval,hdr,matchstr,utb,lnum,tok=token,ch=0
;
         htry = strtrim( hdr(lnum(0)+1),2 )
         try = gettok(htry,' ')
         htry = strtrim(htry,2)
         try = gettok(htry,' ')
         yy = fix(strmid(try,7,4))
         mm = strmid(try,3,3)
         im = where(months eq mm)
         mm = im + 1
         dd = fix(strmid(try,0,2))
         obsdate = (yy-1900.)*1.e4 + mm*1.e2 + dd               ;e.g., 910125
         end
;
  'RDF': begin
         date = sxpar(hdr,'DATE-OBS')
         day = gettok(date,'/')
         mon = gettok(date,'/')
         obsdate = fix(date)*1.e4 + fix(mon)*1.e2 + fix(day)    ;e.g., 910125
         end
endcase
obsdate = obsdate(0)
;
return
end            ;rsobsdate
