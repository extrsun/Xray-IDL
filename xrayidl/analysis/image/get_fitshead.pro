pro get_fitshead,image,head,refhead,del=del,hist=hist,cpx=cpx,cpy=cpy $
,equi=equi,crval=crval,galactic=galactic,dimo=dimo,type=type,cdim=cdim
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
; 
; writen by wqd, Sept 30, 1993
;
; add keywords, dim and type, so that image does not have to be supplied
; use the standard FORTRAN definition for reference pixel positions,
; instead of the idl coordinate, wqd, 7/27/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_fitshead,image,head,refhead,del=del,hist=hist'
print,',cpx=cpx,cpy=cpy,equi=equi,crval=crval,galactic=galactic,dimo=dimo,type=type,cdim=cdim'
RETURN
ENDIF
if n_elements(dimo) eq 0 then begin
	sz=size(image)
	dim=lonarr(2)
	dim(0)=sz(1)
	dim(1)=sz(2)
	mkhdr,head,image ; make a basic header first
endif else begin
	if n_elements(type) eq 0 then stop,'Image type is needed!'
	if n_elements(dimo) eq 1 then dim=[1,1]*dimo else dim=dimo
	mkhdr,head,type,dim ; make a basic header first
endelse
   sxaddpar,head,'naxis',2            ; Astron routine, adds parameters.
if n_elements(crval) eq 0 then 	begin
	crval=[sxpar(refhead,'crval1'),sxpar(refhead,'crval2')]
	if n_elements(crval) le 1 then begin
		crval=dblarr(2)
		crval(0)=sxpar(refhead,'RA_NOM') ;from RDF files
		crval(1)=sxpar(refhead,'DEC_NOM')
	endif
endif
sxaddpar,head,'crval1',crval(0)
sxaddpar,head,'crval2',crval(1)
if n_elements(del) eq 0 then begin
	cdelt=[sxpar(refhead,'cdelt1'),sxpar(refhead,'cdelt2',count=count)]
	if  count eq 0 then $
		cdelt=[sxpar(refhead,'cd1_1'),sxpar(refhead,'cd2_2')]
endif else begin
	if n_elements(del) eq 1 then begin
		cdelt=replicate(del,2)
		;assumed to be same for the two axises
		cdelt(0)=-cdelt(0)
	endif else cdelt=del
endelse
if N_elements(refhead) ne 0 then begin
	ctype=[sxpar(refhead,'ctype1'),sxpar(refhead,'ctype2')]
	if !err ge 0 then begin
	 sxaddpar,head,'ctype1',ctype(0)
	 sxaddpar,head,'ctype2',ctype(1)
	endif
endif else begin
	if keyword_set(galactic) then begin
		sxaddpar,head,'ctype1','GLOG---TAN'
		sxaddpar,head,'ctype2','GLAT--TAN' 
	endif else begin
		sxaddpar,head,'ctype1','RA---TAN'
		sxaddpar,head,'ctype2','DEC--TAN'
	endelse
endelse 
sxaddpar,head,'cdelt1',cdelt(0)
sxaddpar,head,'cdelt2',cdelt(1)

;if n_elements(cpx) eq 0 then cpx=dim(0)*0.5 
;if n_elements(cpy) eq 0 then cpy=dim(1)*0.5
if n_elements(cpx) eq 0 then begin
    if n_elements(cdim) ne 0 then cpx=sxpar(refhead,'crpix1')-cdim(0) $
    else cpx=(dim(0)+1.)*0.5 
endif 
if n_elements(cpy) eq 0 then begin
    if n_elements(cdim) ne 0 then cpy=sxpar(refhead,'crpix2')-cdim(1) $
    else cpy=(dim(1)+1.)*0.5
endif 
; use the standard Fortran definition Instead of the idl coordinate, wqd, 7/27/2001
sxaddpar,head,'crpix1',cpx
sxaddpar,head,'crpix2',cpy
   sxaddpar,head,'bscale',1.
   sxaddpar,head,'bzero',0.
   sxaddpar,head,'crota1',0.
   sxaddpar,head,'crota2',0.
   sxaddpar,head,'date',systime(0)
if n_elements(equi) eq 0  then begin
    if n_elements(refhead) ne 0  then begin
	equi=sxpar(refhead,'equinox',count=count) 
	if count eq 0 then equi=sxpar(refhead,'epoch',count=count)
	if count eq 0 then equi=2000
    endif else equi=2000
endif 
sxaddpar,head,'equinox',float(equi)
if n_elements(hist) ne 0 then sxaddpar,head,'history',hist
return
end
