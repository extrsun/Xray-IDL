;+
; NAME:
;  FITS_READ
; PURPOSE:
;  Procedure to determine the format of a FITS file (i.e., simple FITS,
;  FITS ASCII table, FITS binary table) and use the correct routine
;  to read it.
;  Also reads values of BSCALE and BZERO (for FITS images) to return
;  noninteger values
;
; CALLING SEQUENCE:
;  fits_read,name,h,tab,[unit],chatter=chatter
;
; INPUTS:
;  name - name of image or table header file (if qualifier not supplied
;         then .HHH is assummed)
; OUTPUTS:
;  hdr - fits header, string array
;  imtab - image or table array 
;          if ASCII table, then 2-D byte array of ascii characters
;          if binary table, then 2-D byte array
;
; OPTIONAL INPUTS:
;  unit - unit number (1 to 9) to use for I/O (default=1)
;
; PROCEDURES CALLED:
;  ZPARCHECK, SXREAD, SXPAR, SXOPEN
;  STRD, FTREAD, TBREAD
;  
; HISTORY:
;  Coded by G. Reichert 04 Feb 1992
;
;-
pro fits_read,name,hdr,imtab,unit,chatter=chatter
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'FITS_READ, name, HDR, IMTAB [,unit, chatter=chatter]'
  retall & endif
;
if (n_elements(chatter) eq 0) then chatter=1    ;default is 1
sxhread,name,hdr
if (npar lt 3) then begin        ;only read imtab if requested to do so
  if (chatter eq 1) then $
     print,' Reading header info only. Finished.'
  return
endif
;
try=sxpar(hdr,'XTENSION')        ;determine if this is an image or a table
stry = size(try)
ns = n_elements(stry)
trytyp = stry(ns-2)             ;type code for try
;
if (trytyp ne 7) then begin      ;try is not a string variable
;
  if (try eq 0) then begin          ;a simple image
    naxis = sxpar(hdr,'naxis')      ;check to make sure there's data
    if (naxis ne 0) then begin
      if (chatter eq 1) then $
        print,'Using STRD to read FITS image ',name
      strd,imtab,hdr,name
;
      bscale = sxpar(hdr,'bscale')
      bzero = sxpar(hdr,'bzero')
      if ((bscale ne 1.0) or (bzero ne 0.0)) then $
         imtab = bzero + bscale*imtab
    endif else print,'File ',name,' contains no data. Finished.'
;
  endif else begin
    print,'Sorry, use a different procedure to read this file: ',name
    !error = -1
  endelse
endif else begin                 ;try is a string variable    
  try = strtrim(try,2)
;
  case try of  
  'TABLE': begin                 ;an ASCII table
     if (chatter eq 1) then $
        print,'Using FTREAD to read ASCII table ',name
     ftread,name,hdr,imtab        
     end
  'BINTABLE': begin
     if (chatter eq 1) then $
        print,'Using TBREAD to read Binary table ',name
     tbread,name,hdr,imtab
     end
  'A3DTABLE': begin
     if (chatter eq 1) then $
        print,'Using TBREAD to read Binary table ',name
     tbread,name,hdr,imtab
     end
  else: begin
     print,'Sorry, use a different procedure to read this file: ',name
     !error = -1
     end
  endcase
endelse
;
return
end       ;pro fits_read     
