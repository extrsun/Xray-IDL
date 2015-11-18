;+
; NAME:
;	RDF_EXTAST
; PURPOSE:
;	Extract astrometry parameters from a FITS image header.
;	The astrometry in the header can be in either CD (Coordinate
;	description) format, or CROTA and CDELT (AIPS-type) format.
;	However, the output astrometry will always be in CD format.
;
; CALLING SEQUENCE:
;	RDF_EXTAST, hdr, [ cd, crpix, crval, noparams ]   
;
; INPUT:
;	HDR - variable containing the FITS header (string array)
;
; OUTPUTS:
;	CD   - 2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;		in DEGREES                                      CD2_1 CD2_2
;	CRPIX - 2 element vector giving X and Y coord of reference pixel
;	CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;          in decimal DEGREES 
;	NOPARAMS -  Scalar indicating the results of EXTAST
;		-1 = Failure - Header missing astrometry parameters
;		0 = Success - Header contains CD astrometry
;		1 = Success - Header contains CROTA (AIPS-type) astrometry
;
; PROCEDURE
;       Based on EXTAST but using astrometry keyword names given in
;       RDF format headers
;
;PROCEDURES CALLED:
;	ZPARCHECK, SXPAR
;REVISION HISTORY
;	Written by B. Boothman 4/15/86
;	Accept CD001001 keywords               1-3-88
;	Accept CD1_1, CD2_1... keywords    W. Landsman    Nov. 92
;-
pro rdf_extast,hdr,cd,crpix,crval,noparams
 On_error,2

if ( N_params() LT 2 ) then begin
  print,'Syntax - extast, hdr, cd, [ crpix, crval, noparams ]'
  return
endif
 
zparcheck,'RDF_EXTAST',hdr,1,7,1,'FITS image header' ;Make sure valid header
noparams = 1

CDELT0 = sxpar( hdr, 'TCDLT1' )
CDELT1 = sxpar( hdr, 'TCDLT2' )
cdelt = [cdelt0,cdelt1]
;
CROTA = sxpar( hdr,'TCROT2')    ;CROTA2 correct rotation keyword
crota = crota / !RADEG	     ;Convert CROTA parameters to CD parameters
signcdelt = fix(cdelt GT 0) - fix(cdelt LT 0)
CD11 = CDELT(0) * cos(crota)
CD12 =  abs( CDELT(1) ) * signcdelt(0)* sin(crota)
CD21 = -abs( CDELT(0) ) * signcdelt(1)* sin(crota)
CD22 = CDELT(1) * cos(crota)

crval0 = sxpar( hdr, 'TCRVL1' )
crval1 = sxpar( hdr, 'TCRVL2' )
crval = [crval0,crval1]

crpix0 = sxpar( hdr, 'TCRPX1' )
crpix1 = sxpar( hdr, 'TCRPX2' )
crpix = [crpix0,crpix1]
 
cd = [ [ cd11, cd21 ] , [ cd12, cd22] ]    ;Error in array order corrected
 

if (!debug gt 2) then print,cd,crval,crpix,noparams

return
end
