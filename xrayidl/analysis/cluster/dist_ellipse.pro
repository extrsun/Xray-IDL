pro dist_ellipse ,im, dimx, xcen ,ycen,ee,al,dimy=dimy
;+
; NAME:	
;	DIST_ellipse
; PURPOSE:	
;	Form an array in which the value of each element is its
;	distance to a specified center in an elliptical coordinate system
;	specified by the ecentricity ee and angle al between the major axis
;	and x axis. 
;
; CALLING SEQUENCE:
;	DIST_CIRCLE, IM, N, XCEN, YCEN
;
; INPUTS:
;	dimx, dimy = size of result, scalar.  
;		Output array will have dimensions dimx X dimy
;	XCEN,YCEN = Scalars designating the X,Y pixel center.  These need
;		not be integers, and need not be located within the
;		output image
;
; OUTPUTS:
;	IM  - dimx by dimy floating array in which  the value of each pixel is 
;		equal to its distance to XCEN,YCEN
;
; EXAMPLE:
;	Total the flux in a circular aperture within 3' of a specified RA
;	and DEC on an 512 x 512 image IM, with a header H.
;
;	IDL> adxy, H, RA, DEC, x, y       ;Convert RA and DEC to X,Y
;	IDL> getrot, H, rot, cdelt        ;CDELT gives plate scale deg/pixel
;	IDL> cdelt = cdelt*3600.          ;Convert to arc sec/pixel
;	IDL> dist_circle, circle, 512, x, y  ;Create a distance circle image
;	IDL> circle = circle*abs(cdelt(0))   ;Distances now given in arcseconds
;	IDL> good = where(circle LT 180)  ;Within 3 arc minutes
;	IDL> print,total( IM(good) )      ;Total pixel values within 3'
;
; RESTRICTIONS:
;	The speed of DIST_CIRCLE decreases and the the demands on virtual
;	increase as the square of the output dimensions.   Users should
;	dimension the output array as small as possible, and re-use the
;	array rather than re-calling DIST_CIRCLE
;
; MODIFICATION HISTORY:
;	Adapted from DIST    W. Landsman            March 1991
;-
 On_error,2                ;Return to caller if an error occurs

 if N_params() LT 4  then begin
     print,'Calling Sequence - DIST_CIRCLE, IM, N, XCEN, YCEN,ee,al,dimy=dimy'
     print,'IM - output image array
     print,'N - size of the (square) output image array'
     print,'XCEN,YCEN - position from which to specify distances'
     return
 endif
if n_elements(al) eq 0 then al=0.
if n_elements(dimy) eq 0 then dimy=dimx
if  n_elements(xcen) eq 0 then xcen=(dimx-1.)*0.5
if  n_elements(ycen) eq 0 then ycen=(dimy-1.)*0.5
	qq=1.-ee
	loc=lindgen(dimx,dimy)
	xx=loc mod dimx
	yy=loc/dimx
	xe=cos(al)*(xx-xcen)+sin(al)*(yy-ycen)
	ye=-sin(al)*(xx-xcen)+cos(al)*(yy-ycen)
	im = fltarr(dimx,dimy, /NOZERO) 
	im(0,0)=sqrt(xe^2+ye^2/qq^2)
return
 end
