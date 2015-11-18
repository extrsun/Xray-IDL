;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       mpe_seqinfo
;
;*PURPOSE:
; A procedure to read sequence information from the sequence header for
; a ROSAT (MPE format) FITS file.
;
;*CALLING SEQUENCE:
;       mpe_seqinfo,fitsname,NOBI,ACTIM,NOMASP,SEQBEG,SEQEND
;
;*PARAMETERS:
; INPUTS:
;	fitsname - Full name of FITS file including directory & extension
;
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
;
;*NOTES
;   The information for the individual OBIs is not stored in the header,
;   so OBIBEG and OBIEND are not returned
;   The nominal roll angle is not stored in the header, so nomasp(3)
;   is set to zero
;
;*SUBROUTINES
;  FITS_INFO
;  HEADFITS
;  RS_NOMASP
;  GETHISTVAL
;  GETTOK
;  RSOBITIMES
;
;*MODIFICATION HISTORY:
;  written  13 Aug 1993 by GAR
;-
;-------------------------------------------------------------------------------
pro mpe_seqinfo,fitsname,nobi,actim,nomasp,seqbeg,seqend
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MPE_SEQINFO, fitsname, NOBI, ACTIM, NOMASP, SEQBEG, SEQEND '
  return
endif
;
fits_info,fitsname,n_ext=nobi,/sil
hdr = headfits(fitsname,ext=1)           ;all headers the same
gethistval,hdr,'ESO-DESCRIPTORS START',value,lnum,ch=0
pos = max(lnum)
if (pos lt 0) then begin
  print,' This header does not contain sequence info. Returning.'
  return
endif 
;
if (npar ge 4) then begin                ;aspect information comes first
  nomasp = dblarr(3)
  rs_nomasp,hdr,nomra,nomdec,nomroll,proc='mpe'
  nomasp(0) = nomra
  nomasp(1) = nomdec
;
; I can't find where the roll angle is stored in the header (if at all)
; Return zero for now
;
endif          ;of getting nomasp
;
if (npar ge 3) then begin       ;look for active time (= exposure duration?)
  gethistval,hdr,'OBS_DUR_SEC',value,lnum,ch=0
  hdrrec = hdr(lnum(0)+1)
  try = gettok(hdrrec,'  ')
  actim = float(hdrrec)  
endif
;
if (npar ge 5) then begin                  ;look for sequence start times
  seqbeg = dblarr(4)
  seqend = dblarr(4)
  rsobitimes,hdr,sctbeg,sctend,utbeg,utend,proc='mpe'
  seqbeg(0) = sctbeg
  seqend(0) = sctend
  seqbeg(1) = utbeg
  seqend(1) = utend
endif             ;of getting sequence start in spacecraft time
;
if (!debug gt 1) then stop,'stopping at end of program'
return
end         ;pro mpe_seqinfo
