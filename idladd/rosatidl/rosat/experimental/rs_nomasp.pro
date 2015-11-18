;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rs_nomasp
;
;*PURPOSE:
; A procedure to read nominal aspect information from the sequence header for
; a ROSAT FITS file.
;
;*CALLING SEQUENCE:
;       rs_nomasp,hdr,nomra,nomdec,nomroll,proc=proc
;
;*PARAMETERS:
; INPUTS:
;	hdr    - FITS header
;
; OPTIONAL INPUTS:
;       proc   - format of processed files. Choices are US, MPE.
;                if not specified, US is assumed.
;
; OUTPUTS:
;       nomra   - Nominal pointing RA (in units of 0.5 arcsec pixels)
;       nomdec  - Nominal pointing DEC (in units of 0.5 arcsec pixels)
;       nomroll - Nominal roll angle (in decimal degrees)
;
;*NOTES
;   The nominal roll angle is not stored in the MPE header, so nomasp(3)
;   is set to zero
;
;*SUBROUTINES
;  GETHISTVAL
;
;*MODIFICATION HISTORY:
;	written  13 Aug 1993 by GAR (adapted from GETNOMASP - which this
;         replaces - but logic simplified by calls to GETHISTVAL)
;-
;-------------------------------------------------------------------------------
pro rs_nomasp,hdr,nomra,nomdec,nomroll,proc=proc
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RS_NOMASP, hdr, NOMRA, NOMDEC, NOMROLL, proc=proc (US)'
  return
endif
if (n_elements(proc) eq 0) then proc = 'US'
procuc = strupcase(proc)
;
case procuc of
  'US': begin
    gethistval,hdr,'Nominal Observation RA',value,lnum,token='=',ch=0
    nomra = double(value)
    gethistval,hdr,'Nominal Observation DEC',value,lnum,token='=',ch=0
    nomdec = double(value)
    gethistval,hdr,'Nominal Observation Roll Angle',value,lnum,token='=',ch=0
    nomroll = double(value)/2./3600.    ;return in deg
  end
;
  'MPE': begin
    gethistval,hdr,'POINT_LONG',value,lnum,ch=0
    hdrrec = hdr(lnum(0)+1)
    try = gettok(hdrrec,'  ')
    rah = fix(gettok(hdrrec,'H'))
    ram = fix(gettok(hdrrec,'M'))
    ras = float(gettok(hdrrec,'S'))
    nomra = double(ten(rah,ram,ras))*15.*3600.*2.
;
    gethistval,hdr,'POINT_LAT',value,lnum,ch=0
    hdrrec = hdr(lnum(0)+1)
    try = gettok(hdrrec,'  ')
    decd = fix(gettok(hdrrec,'D'))
    decm = fix(gettok(hdrrec,'M'))
    decs = float(gettok(hdrrec,'S'))
    nomdec = double(ten(decd,decm,decs))*3600.*2.
;
    nomroll = 0.0D0               ;no value given in MPE FITS header
  end
;
  else: begin
    print,proc,' is not a valid Rosat processing format.'
    print,' Valid options are US, MPE. Returning.'
  end
endcase
;
return
end          ;pro rs_nomasp
