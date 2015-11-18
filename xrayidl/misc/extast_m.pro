pro extast_m,hdr,cd,crpix,crval,noparams
;+
;NAME:
;   EXTAST
;PURPOSE:
;   Extract astrometry parameters from a FITS image header.
;   The astrometry in the header can be in either CD (Coordinate
;   description) format, or CROTA and CDELT (AIPS-type) format.
;   However, the output astrometry will always be in CD format.
;CALLING SEQUENCE:
;   EXTAST,HDR,CD                          ;Just extract CD matrix
;   EXTAST,HDR,CD,CRPIX,CRVAL,NOPARAMS     ;Get all astrometry params
;INPUT:
;   HDR - variable containing the FITS header (string array)
;OUTPUTS:
;  CD   - 2 x 2 array containing the astrometry parameters CD001001 CD001002
;         in DEGREES                                       CD002001 CD002002
;  CRPIX - 2 element vector giving X and Y coord of reference pixel
;  CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;          in decimal DEGREES 
;  NOPARAMS -  Scalar indicating the results of EXTAST
;       -1 = Failure - Header missing astrometry parameters
;        0 = Success - Header contains CD astrometry
;        1 = Success - Header contains CROTA (AIPS-type) astrometry
;PROCEDURE
;    EXTAST checks for astrometry parameters in the following order:
;    (1) the CD matrix CD1_1,CD1_2... plus CRPIX and CRVAL.   In this case,
;        FITS_CD_FIX is used to convert these to CD001001, CD001002...
;    (2) the CD matrix CD001001,CD001002...plus CRPIX and CRVAL                
;    (3) CROTA2 (or CROTA1) and CDELT plus CRPIX and CRVAL.
;    See the Memo: World Coordinate Systems Representations within the
;                  FITS Format by Hanisch and Wells.
;PROCEDURES CALLED:
;     ZPARCHECK, SXPAR, FITS_CD_FIX
;REVISION HISTORY
;    Written by B. Boothman 4/15/86
;    Accept CD001001 keywords               1-3-88
;-
if n_params(0) lt 2 then begin
     print,string(7B),'CALLING SEQUENCE - extast,hdr,cd,[crpix,crval,noparams]'
     return
endif
zparcheck,'EXTAST',hdr,1,7,1,'FITS image header'   ;Make sure valid header
noparams = -1                                    ;Assume no astrometry to start
;
; The following two lines can be deleted if the oldstyle CD1_1 keywords are
; never used.
;
CD11 = sxpar(hdr, 'CD1_1')
if !ERR ne -1 then fits_cd_fix,hdr
;
CD11 = sxpar(hdr,'CD001001')
if !ERR eq -1 then begin	    ;If CD parameters don't exist, try CROTA
        CDELT = sxpar(hdr,'CDELT*')
        if !ERR lt 2 then return       ;Must have CDELT1 and CDELT2
        CROTA = sxpar(hdr,'CROTA2')    ;CROTA2 correct rotation keyword
	if !ERR eq -1 then  crota = sxpar(hdr,'CROTA1')
;the following line is commented for iras image
;        if !ERR eq -1 then return      ;Added Feb. 1991, check for no rotation
	if !ERR eq -1 then crota=0.  ;added Aug 1992, assume no rotation
        crota = crota/!radeg	;Convert CROTA parameters to CD parameters
        CD11 = CDELT(0) * cos(crota)
        CD12 = -CDELT(1)* sin(crota)
        CD21 = CDELT(0) * sin(crota)
        CD22 = CDELT(1) * cos(crota)
        noparams = noparams+1             ;AIPS-type astrometry found
   endif else begin
   CD12 = sxpar(hdr, 'CD001002')
   CD21 = sxpar(hdr, 'CD002001')  
   CD22 = sxpar(hdr, 'CD002002')
endelse
crval = sxpar(hdr,'CRVAL*')
  if !ERR lt 2 then return                 ;No CRVAL parameters?
crpix = sxpar(hdr,'CRPIX*')
  if !ERR lt 2 then return                 ;No CRPIX parameters?
cd = [ [cd11,cd21] , [cd12,cd22] ]         ;Error in array order corrected
noparams = noparams+1                      ;Astrometry parameters found
return
end
