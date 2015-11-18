pro writefits_evt,fname,list,crval,refp,pixsize,rfname,galactic=galactic,h1=h1,gti=gti
;+
; create a rather comprehensive header for an image. Default information 
; may be transfered from the reference header to the output header
;
; image - the reference image. not used if dim is given.
; refhead - the reference header
; head - the output header
; del - the size of a pixel (in units of degrees)
; 	if del contains one element only, cdelt[1,2]=[-del,del]
;	else cdelt=del
; hist - history record
; cpx, cpy - the reference pixel in the image
; equi - the equinox of the coordinate
; glactic - if set, the coordinate type will be set as 
;		'GLOG---TAN' and 'GLAT--TAN' 
; 	instead if
;		'RA---TAN' and 'DEC--TAN'
; dimo - dimension of the array, if given, type should also be given and 
;	the image is not used.
;  TYPE - If more than 2 parameters are supplied, then the second parameter
;               is intepreted as an integer giving the IDL datatype e.g. 
;               1 - LOGICAL*1, 2 - INTEGER*2, 3 - INTEGER*4, 4 - REAL*4, 
;		5 - DOUBLE*8
; cdim - vector containing the one-side cut dimensions of the image 
; relatively to the reference image (e.g., cdim=[0,100]), used for
; changing the reference pixel in the new header.
; gti - GTI structure
; 
; writen by wqd, Sept 30, 1993
;
; add keywords, dim and type, so that image does not have to be supplied
; use the standard FORTRAN definition for reference pixel positions,
; instead of the idl coordinate, wqd, 7/27/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - writefits_evt,fname,list,crval,refp,pixsize,rfname,galactic=galactic,h1=h1,gti=gti'
RETURN
ENDIF
h0=headfits(rfname,ext=0)
FXWRITE,fname,h0
fxbhmake,h1,n_elements(list),'events',/init

if keyword_set(galactic) eq 0 then sxaddpar,h1,'tctyp1','RA---TAN' else $
  sxaddpar,h1,'tctyp1','GLOG---TAN'
sxaddpar,h1,'tcrvl1',crval(0)
sxaddpar,h1,'tcrpx1',refp(0)
sxaddpar,h1,'tcdlt1',-pixsize
if keyword_set(galactic) eq 0 then sxaddpar,h1,'tctyp2','DEC--TAN' else $
  sxaddpar,h1,'tctyp2','GLAT--TAN'
sxaddpar,h1,'tcrvl2',crval(1)
sxaddpar,h1,'tcrpx2',refp(1)
sxaddpar,h1,'tcdlt2',pixsize
mwrfits,list,fname,h1

if n_elements(gti) ne 0 then begin 
    s=sort(gti.(1)) 
    fxbhmake,h2,n_elements(gti),'GTI',/init
    mwrfits,gti(s),fname,h2
endif
return
end
