;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
; rs_seqinfo
;
;*PURPOSE:
; A procedure to read sequence information from the sequence header for
; a ROSAT FITS file.
;
;*CALLING SEQUENCE:
; rs_seqinfo,fitsname,NOBI,ACTIM,NOMASP,SEQBEG,SEQEND,OBIBEG,OBIEND,$
;            DTCOR,ONTIME,proc=proc
;
;*PARAMETERS:
; INPUTS:
;	fitsname - Full name of FITS file including directory & extension
;
; OPTIONAL INPUTS:
;       proc   - format of processed files. Choices are US, MPE.
;                if not specified, US is assumed.
; OUTPUTS:
;       nobi   - Number of separate observation intervals in sequence
;       actim  - Sequence total active time (in seconds)
;       nomasp - 3 element vector containing nominal RA, Dec and Roll for
;                sequence (RA and Dec in 0.5 arcsec pixels, Roll in degrees)
;       seqbeg - 4 element vector containing sequence start time in
;                spacecraft time (seconds) and in UT (year, day number of 
;                day number of year, and sceonds of day).
;       seqend - 4 element vector containing sequence stop time in
;                spacecraft time (seconds) and in UT (year, day number of 
;                day number of year, and sceonds of day).
;       obibeg - dates & times of beginnings of obi segments
;       obiend - dates & times of ends of obi segments
;                These are 2D arrays consisting of 4 rows. Each row
;                contains as many elements as there are obi segments.
;                Row 0 contains the spacecraft time (in sec).
;                Row 1 contains the year of observation (e.g., 1990).
;                Row 2 contains the day number of the year (e.g., 207).
;                Row 3 contains the UT observation time (in sec). 
;       dtcor  - Deadtime correction factor. Set to 1 unless proc='RDF'
;       ontime - Total on time (sec). Set to ACTIM unless proc='RDF'
;
;*NOTES
;   The information for the individual OBIs is not stored in the MPE header,
;     so OBIBEG and OBIEND are set equal to SEQBEG & SEQEND for MPE format
;   The nominal roll angle is not stored in the MPE header, so nomasp(3)
;     is set to zero for MPE format
;   Return dead time correction and total on time for RDF times; otherwise
;     set equal to 1.0 and ACTIM
;
;*MODIFICATION HISTORY:
;   written  13 Aug 1993 by GAR (replaces rsgetseqinfo)
;   modified 16 Dec 1993 by GAR to read info from RDF format files
;-
;-------------------------------------------------------------------------------
pro rs_seqinfo,fitsname,nobi,actim,nomasp,seqbeg,seqend,obibeg,obiend,dtcor,$
               ontime,proc=proc
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RS_SEQINFO, fitsname, NOBI, ACTIM, NOMASP, SEQBEG, SEQEND, '
  print,'               OBIBEG, OBIEND, DTCOR, ONTIME, proc=proc (US)'
  return
endif
if (n_elements(proc) eq 0) then proc = 'US'
procuc = strupcase(proc)
;
case procuc of
  'US':  begin
         us_seqinfo,fitsname,nobi,actim,nomasp,seqbeg,seqend,obibeg,obiend
         dtcor =1.0
         ontime = actim
         end
  'MPE': begin
         mpe_seqinfo,fitsname,nobi,actim,nomasp,seqbeg,seqend
         obibeg = dblarr(4,nobi)
         for ii=0,nobi-1 do obibeg(0,ii) = seqbeg
         obiend = dblarr(4,nobi)    
         for ii=0,nobi-1 do obiend(0,ii) = seqend
         dtcor =1.0
         ontime = actim
         end
  'RDF': rdf_seqinfo,fitsname,nobi,actim,nomasp,seqbeg,seqend,obibeg,obiend,$
                     dtcor,ontime
  else:  begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         end
endcase
;
if (!debug gt 3) then stop,'stopping at end of program'
return
end         ;pro rs_seqinfo
