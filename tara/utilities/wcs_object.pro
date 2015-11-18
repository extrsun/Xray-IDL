;+
;========================================================================
;;;
;;; World Coordinate System object: $Id: wcs_object.pro 1774 2003-06-07 17:36:37Z patb $
;;;
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION: 
;;; This routine encapsulates code used to work with World Coordinate 
;;; Systems as defined in the FITS standard.
;;; The FITS keywords define a "value" coordinate system named CTYPEn
;;; in terms of the "pixel" coordinate system (1..N) of the array:
;;; 
;;; (value - CRVLn) = CDLTn * (pixel - CRPXn)
;;; 

;;; When working with coordinates involving spherical projections
;;; e.g. RA---TAN and DEC--TAN, then "wcs" should be a 2-vector of
;;; wcs structures and other parameters (e.g. ctyp) should be 2-vectors
;;; as well.

;;; To convert a "pixel" quantity to a world coordinate "value", define
;;; PIXEL and make sure VALUE is undefined -- VALUE and VALUE_STRING are
;;; then calculated.
;;; To convert a "value" quantity to a world coordinate "pixel", define
;;; VALUE and make sure PIXEL is undefined -- PIXEL and PIXEL_STRING are
;;; then calculated.

;-

PRO wcs_object, wcs, INIT=init, HEADER=header, COLNUM=colnum, $
		IS_NULL=is_null, $
		CTYP=ctyp, CRVL=crvl, CRPX=crpx, CDLT=cdlt, GET=get, $
		PIXEL=pixel, FORMATTED_PIXEL=fp, $
		VALUE=value, FORMATTED_VALUE=fv


if (n_elements(pixel) GT 0) then begin
  ;; We have a pixel-to-value computation to make.  What cardinality?
  if (n_elements(wcs) EQ 2) then begin
    ;; We have a pair of wcs structures.  What kind of projection?
    if ((wcs[0].ctyp EQ 'RA---TAN') AND (wcs[1].ctyp EQ 'DEC--TAN')) then begin
      ;; RA---TAN and DEC--TAN define a tangent (gnomonic) projection.
      ;; The following code was lifted from xy2ad.pro in the Astro Library.
      radeg = 180.0d/!DPI                  ;Double precision !RADEG
      crval = wcs.crvl / radeg

      xsi   = wcs[0].cdlt * (pixel[0] - wcs[0].crpx) / radeg
      eta   = wcs[1].cdlt * (pixel[1] - wcs[1].crpx) / radeg
      
      beta  = cos(crval[1]) - eta * sin(crval[1])
      a     = atan(xsi, beta) + crval[0]
      gamma = sqrt((xsi^2) +(beta^2))
      d     = atan(eta*cos(crval[1])+sin(crval[1]) , gamma)
      ra = a*RADEG   &  dec = d*RADEG
    
      value = [ra, dec]
      fv = (adstring(ra, dec, 2))[0]
      
    endif else begin
      ;; Just two uncoupled linear conversions.
      value = wcs.crvl + wcs.cdlt * (pixel - wcs.crpx)
      fv1=''  &  fv2=''
      if (wcs[0].ctyp NE '') then fv1=fv1 + wcs[0].ctyp + '='
      if (wcs[1].ctyp NE '') then fv2=fv2 + wcs[1].ctyp + '='
      fv1=fv1 + string(value[0], F='(G11.5)')
      fv2=fv2 + string(value[1], F='(G11.5)')
      fv=strcompress( fv1,/REM ) + "; " + strcompress( fv2,/REM )
    endelse
    
  endif else begin
    ;; We have only one wcs structure, so do linear conversion.
    value = wcs.crvl + wcs.cdlt * (pixel - wcs.crpx)
    fv=''
    if (wcs.ctyp NE '') then fv=fv + wcs.ctyp + '='
    fv=strcompress( fv + string(value[0], F='(G11.5)'), /REM )
  endelse    
  is_null = wcs.is_null 
  return
endif ;pixel-to-value conversion


if (n_elements(value) GT 0) then begin  
  ;; We have a value-to-pixel computation to make.  What cardinality?
  if (n_elements(wcs) EQ 2) then begin
    ;; We have a pair of wcs structures.  What kind of projection?
    if ((wcs[0].ctyp EQ 'RA---TAN') AND (wcs[1].ctyp EQ 'DEC--TAN')) then begin
      ;; RA---TAN and DEC--TAN define a tangent (gnomonic) projection.
      ;; The following code was lifted from ad2xy.pro in the Astro Library.
      radeg = 180.0d/!DPI   
      crval = wcs.crvl / radeg
      
      radif = value[0] / radeg - crval[0]
      dec   = value[1] / radeg
      h     = sin(dec)*sin(crval[1]) + cos(dec)*cos(crval[1])*cos(radif)

      xsi   = cos(dec)*sin(radif)/h
      eta   = (sin(dec)*cos(crval[1]) -  cos(dec)*sin(crval[1])*cos(radif))/h
 
      xsi   = xsi*RADEG/wcs[0].cdlt
      eta   = eta*RADEG/wcs[1].cdlt
      
      pixel = [xsi + wcs[0].crpx, eta + wcs[1].crpx]

      fp = strcompress( string( pixel, F='(G11.5,"; ",G11.5)' ) )
      
    endif else begin
      ;; Just two uncoupled linear conversions.
      pixel = wcs.crpx + (value - wcs.crvl) / wcs.cdlt
      fp = strcompress( string( pixel, F='(G11.5,"; ",G11.5)' ) )
    endelse
    
  endif else begin
    ;; We have only one wcs structure, so do linear conversion.
    pixel = wcs.crpx + (value - wcs.crvl) / wcs.cdlt
    fp = strcompress( string( pixel, F='(G11.5)' ) )
  endelse    
  is_null = wcs.is_null 
  return
endif ;value-to-pixel conversion


;; Create the structure if necessary.
if ((n_elements(wcs) EQ 0) OR keyword_set(init)) then $
  wcs = {WCS, is_null:1B, ctyp:'', crvl:0.0D, crpx:0.0D, cdlt:1.0D }


;; Parse a FITS header if desired.  
if (keyword_set(header) AND n_elements(colnum) GT 0) then begin
  cn  = string(colnum, f='(I0)')
  
  val = fxpar(header, 'TCTYP'+cn, COUNT=count)
  if (count GT 0) then begin
    wcs.ctyp    = val
    wcs.is_null = 0
  endif
  
  val = fxpar(header, 'TCRVL'+cn, COUNT=count)
  if (count GT 0) then begin
    wcs.crvl    = val
    wcs.is_null = 0
  endif
  
  val = fxpar(header, 'TCRPX'+cn, COUNT=count)
  if (count GT 0) then begin
    wcs.crpx    = val
    wcs.is_null = 0
  endif
  
  val = fxpar(header, 'TCDLT'+cn, COUNT=count)
  if (count GT 0) then begin
    wcs.cdlt    = val
    wcs.is_null = 0
  endif
endif

if keyword_set(get) then begin
  ctyp = wcs.ctyp
  crvl = wcs.crvl
  crpx = wcs.crpx
  cdlt = wcs.cdlt
endif else begin
  if (n_elements(ctyp) GT 0) then begin
    wcs.ctyp    = ctyp
    wcs.is_null = 0
  endif
  if (n_elements(crvl) GT 0) then begin
    wcs.crvl    = crvl
    wcs.is_null = 0
  endif
  if (n_elements(crpx) GT 0) then begin
    wcs.crpx    = crpx
    wcs.is_null = 0
  endif
  if (n_elements(cdlt) GT 0) then begin
    wcs.cdlt    = cdlt
    wcs.is_null = 0
  endif
endelse

is_null = wcs.is_null 
return
end
