pro ir2xray,irfile,outfile,xfile=xfile,xsize=xsize,ysize=ysize, $
                           pixsize=pixsize,xcenter=xp,ycenter=yp, $
                           image=outarr,header=xhead
;
;+
;    Inputs:   irfile - input IRAS image.
;              outfile - output file containg IRAS pixels in the same grid as
;                          the ROSAT image. 
;
;    Keywords: xfile - name of the reference ROSAT file.  Only used for 
;                          coordinate information.
;              xsize,ysize - x and y size of output image
;              pixsize - size of a pixel in arcseconds
;              xcenter,ycenter - x and y coordinates of image center in
;                                 degrees, epoch = 2000.
;                            (output keywords)
;              image - output image array
;              header - FITS header for output image
;
; Either xfile or ALL the coordinate information ( (x,y,pix)size,(x,y)center )
;    must be specified.
;
;  All files are assumed to be FITS files.  The ASTRON routines are used to
;    access the files and their headers, and to write the output file.
;
;-
;
if n_params() eq 0 then begin
	print,'CALLING SEQUENCE - ir2xray,irfile,outfile'
	print,'	,xfile=xfile,xsize=xsize,ysize=ysize,pixsize=pixsize'
	print,',xcenter=xp,ycenter=yp,image=outarr,header=xhead'
	return
endif
common  irashdr,blank,crvalv,crpixv,cdeltv,crotav,naxisv
;
if keyword_set(xfile) then begin
   irashdr,xfile                    ; My routine, sets up the IRAS common block
   xhead  = headfits(xfile)         ; Astron routine, gets the FITS header
   xsize  = naxisv(0)               ; Get the coordinate information
   ysize  = naxisv(1)
   xc     = crpixv(0)-1             ; IDL pixels START at 0.
   yc     = crpixv(1)-1
   xp     = crvalv(0)
   yp     = crvalv(1)
   pixsize= abs(cdeltv(0)) * 3600.
endif else begin
   if not keyword_set(pixsize) then pixsize=15.
;   xc = fix(xsize/2.-0.5)             ; IDL pixels START at 0.
;   yc = fix(ysize/2.-0.5)
   xc = xsize/2.+0.5            ; IDL pixels START at 0. /corrected WQD 4/21/93
   yc = ysize/2.+0.5
   mkhdr,xhead,fltarr(xsize,ysize)     ; Astron routine, makes a basic header.
   sxaddpar,xhead,'naxis',2            ; Astron routine, adds parameters.
   sxaddpar,xhead,'naxis1',xsize
   sxaddpar,xhead,'naxis2',ysize
   sxaddpar,xhead,'bscale',1.
   sxaddpar,xhead,'bzero',0.
   sxaddpar,xhead,'crval1',xp
   sxaddpar,xhead,'crval2',yp
   sxaddpar,xhead,'cdelt1',pixsize/(-3600.)
   sxaddpar,xhead,'cdelt2',pixsize/3600.
   sxaddpar,xhead,'crpix1',xc
   sxaddpar,xhead,'crpix2',yc
   sxaddpar,xhead,'crota2',0.
   sxaddpar,xhead,'date',systime(0)
   sxaddpar,xhead,'equinox',2000.
endelse
outarr = fltarr(xsize,ysize)
mask   = outarr
;
;                ** calculate ra and dec arrays for the ROSAT image **
;
line   = fltarr(xsize,ysize)            ; Line and sample are the y and x 
sample = line                           ;    coordinates of each pixel.
count  = findgen(xsize)
for i=0,ysize-1 do sample(0,i) = count
count  = transpose( findgen(ysize))
for i=0,xsize-1 do line  (i,0) = count
count = 0
line = line - yc
sample = sample - xc
;
xcen = xp / !radeg
ycen = yp / !radeg
trans_loct,sample,line,xcen,ycen,ra,dec,pixsize=pixsize  ; Routine supplied by WQD.
line = 0                            ; Zero unneeded arrays to free up memory.
sample =0 
;
;                ** precess coordinates using rewritten Astron routine **
;
num = n_elements(ra)
rfoo=fltarr(num)
dfoo=rfoo
rfoo(0)=ra(lindgen(num)) * !radeg
dfoo(0)=dec(lindgen(num)) * !radeg
sprecess,rfoo,dfoo,2000.,1950.
ra(0) = rfoo / !radeg
dec(0) = dfoo / !radeg
rfoo = 0
dfoo = 0
;
;                ** calculate which IRAS pixels go into the image **
;
irashdr,irfile
sg    = sign(cdeltv(1))                     ; My routine
scale = 1./abs(cdeltv(0))
rac    = crvalv(0)
decc   = crvalv(1)
rac    = rac/!radeg
decc   = decc/!radeg
a      = cos(dec)*cos(ra-rac)
f      = scale*(!radeg)/(sin(decc)*sin(dec) + a*cos(decc))
yy    = sg*f*(cos(decc)*sin(dec)-a*sin(decc))
xx    = -f*cos(dec)*sin(ra-rac)
lmask  = (yy   ge 0)*1.0 - (yy   lt 0)*1.0
smask  = (xx   ge 0)*1.0 - (xx   lt 0)*1.0
yy    = crpixv(1) - 1 + fix(yy   + lmask*(0.5) + 0.5)
xx    = crpixv(0) - 1 + fix(xx   + smask*(0.5) + 0.5)
;
ra = 0
dec = 0
a = 0
f = 0
lmask = 0
smask = 0
;
;                ** Read in the IRAS image, fill in the pixels, and average **
;
map = readfits(irfile,irhead,silent=1) ; Astron routine, reads image
blank  = sxpar(xhead,'blank')    ; Astron routine, reads header values
irashdr,irfile
mm  = where( (xx lt naxisv(0)) and (xx ge 0) and $
             (yy lt naxisv(1)) and (yy ge 0) , count)
if count ne 0 then for i=0l,count-1 do begin
   if map( (xx(mm(i))) , (yy(mm(i))) ) ne blank then begin
      outarr(mm(i)) = outarr(mm(i)) + map( (xx(mm(i))) , (yy(mm(i))) )
      mask (mm(i)) = mask (mm(i)) + 1
   endif
endfor else begin
   print,'!!! No correspondence found between the images !!!'
   print,'           Returning...'
   retall
endelse
;
xx = 0
yy = 0
mm = 0
;
outarr = outarr / mask
index = where( finite(outarr) eq 0 , count)
if count ne 0 then outarr(index)=blank
mx = max(outarr(where(outarr ne blank)), min = mn)
;
;                     ** Modify the existing Rosat header and write out file **
;                         This uses Astron routines
;
sxaddpar,xhead,'telescop','IRAS - regridded'
sxaddpar,xhead,'origin','JPL-IPAC,  regrid done at CASA - Univ. of Colo.'
sxaddpar,xhead,'datamax',mx
sxaddpar,xhead,'datamin',mn
sxaddpar,xhead,'history',$
          'Regridded using software written in IDL at CASA - Univ. of Colo.'
;
if n_elements(outfile) ne 0 then writefits,outfile,outarr,xhead
;
return
end
;
;
;
pro trans_loct,xdis,ydis,xcen,ycen,xfar,yfar,pixsize=pixsize
;.......................................................................
;	Subroutine LOCT finds the location of a far point
;	which is XDIS and YDIS pixels away from center point
;	(XCEN,YCEN). It is the reverse subroutine of DIST.
;	The calling format of this subroutine is:
;	"call loct (xdis,ydis,xcen,ycen,xfar,yfar)"
;	XFAR and YFAR are the right ascension and declination of
;	the far point. They are the output of this subroutine.
;.......................................................................
if(n_params() eq 0) then begin
   print,'CALL SEQUENCE - trans_loct, xdis,ydis,xcen,ycen,XFAR,YFAR'
   print,' '
   print,'xdis,ydis ---- vectors (pixel)'
   print,'xcen,ycen ---- scalars (radian)'
   print,'XFAR,YFAR ---- vectors (radian) 
   return
endif
;......................................................................

;	First, Let's define a parameter for later use. This uses a
;	pre-defined variable pixsize (in arcseconds)
;
	if n_elements(pixsize) eq 0 then pixsize=!size_pixel
	radpix=648000./!pi/pixsize
;	
;	We set up a spherical surface with radius equal to one.
;	Center point and far point are on the surface. Then the
;	distance between far point and center point on the surface
;	is the square root of sum of square of the two distances.
;
	dispix=xdis*0.0
	c=where (XDIS NE 0. OR YDIS NE 0.,count)
	if count ne 0 then dispix(c)=sqrt(xdis(c)*xdis(c)+ydis(c)*ydis(c))
;
;	This distance can not exceed half circle which is 
;       648000./pixsize pixels.
;
	dispix = dispix <  648000./pixsize 
;
;	This distance is equivalent to the angle between the
;	center point and far point except the fact of RADPIX.
;
	angle=dispix/radpix
;
;	Now we calculate the sine of this angle for further use.
;
	sang=sin(angle)
;
;	Find the two normal distances from the far point to
;	the two great circle planes parallel or perpendicular
;	to the meridian and passing center point.
;
;	If the angle between the two points is very small, then we
;	can consider the spherical surface as a plane in small area.
;
	xnorm=xdis/radpix
	ynorm=ydis/radpix
;
;	Otherwise
;
	c=where(angle GE 0.001,count)
	if count NE 0 then begin
	xnorm(c)=xdis(c)*sang(c)/dispix(c)
	ynorm(c)=ydis(c)*sang(c)/dispix(c)
	endif
;
;	Now we can calculate the normal distance
;	from the far point to equator plane which
;	is the sine of the declination of far point.
;
	syfar=sin(ycen)*cos(angle) + ynorm*cos(ycen)
; ynorm is assumed to be positive in the direction of the north?
;
;	Then the YFAR is simply worked out.
;
	syfar=(((syfar < 1.0) +1.) > 0.) -1.
	yfar=asin(syfar)
	cyfar=cos(yfar)
	xfar=yfar*0.0
;
	c=where(cyfar EQ 0.0,count)
	if count ne 0 then begin
	xfar(c)=xcen
	if count EQ n_elements(xfar) then return
	endif
;
;	The difference of XFAR and XCEN is calculated.
;
	difx=xfar*0.0
	c=where (cyfar NE 0.0)
	difx(c)=(((xnorm(c)/cyfar(c) < 1.) +1.) > 0.) -1.
	difx(c)=asin(difx(c))
;
;	This diference could be great than
;	half pi if cosine of it is negative.
;
	sign=where( (ynorm(c)+syfar(c)*cos(ycen))*sin(ycen) LT 0.0,count)
	if count NE 0 then difx(sign)=!pi-difx(sign)
;
	xfar(c)=xcen-difx(c)
;
;	Right ascension is aranged from zero to two pi.
;
	c=where (xfar LT 0.0,count)
 	if count NE 0 then xfar(c)=xfar(c)+2.0*!pi
	c=where (xfar GT 2.0*!pi,count)
	if count NE 0 then xfar(c)=xfar(c)-2.0*!pi
;
;	Return to the calling routine.
;
	return
;
	end
;
;
;
function sign,number,option=option
;
;+
; Returns -1,0,1 for NUMBER<0, NUMBER=0, NUMBER>0 respectively if OPTION is
;   not set.
;
; Returns -1,1 for NUMBER<0, NUMBER>=0 respectively if OPTION is set.
;
; Will work on arrays and anything other than string or complex variables.
;-
;
if n_params(0) lt 1 then return,0
if keyword_set(option) then return, (number ge 0)*1 - (number lt 0)*1 $
   else return, (number gt 0)*1 - (number lt 0)*1
end
;
;
;
PRO IRAShdr,FILENAME
;+
;*****************************************************************************
;
;   to update the iras common block 
;
; input:
;
;     FILENAME:     name of the IRAS FIT file  ie. P075H3B3
;
;     WRITTEN:      4/17/92 - A modification of a verison 1 program, 
;                             by Jon Saken.
;
;                             Uses ASTRON procedures.
;*****************************************************************************
;-
   COMMON  IRASHDR,BLANK,CRVALV,CRPIXV,CDELTV,CROTAV,NAXISV
;
   IF N_PARAMS(0) LT 1 THEN BEGIN
     FILENAME = ''
     READ,'FIT File?  ',FILENAME
   ENDIF
;
header = headfits(filename)
NAXISV = sxpar(header,'naxis*')
BLANK  = sxpar(header,'blank')
CRVALV = float(sxpar(header,'crval*'))
CRPIXV = float(sxpar(header,'crpix*'))
CDELTV = float(sxpar(header,'cdelt*'))
CROTAV = sxpar(header,'crota*')
;
RETURN
END
;
;
;
pro sprecess, ra, dec, equinox1, equinox2, PRINT = print
;+
; NAME:
;    SPRECESS
; PURPOSE:
;    Precess coordinates from EQUINOX1 to EQUINOX2.  For interactive
;    display, one should use ASTRO which calls PRECESS.
; CALLING SEQUENCE:
;    PRECESS, ra, dec, [ equinox1, equinox2, PRINT = ]
; INPUT - OUTPUT:
;    RA - Input right ascension in DEGREES (scalar or vector)
;    DEC - Input declination in DEGREES (scalar or vector)
;          NOTE: The input RA and DEC are modified by PRECESS to give the 
;                 values after precession.
; OPTIONAL INPUTS:
;    EQUINOX1 - Original equinox of coordinates, numeric scalar.  If omitted, 
;             PRECESS will query for EQUINOX1 and EQUINOX2.
;    EQUINOX2 - Equinox of precessed coordinates.
; OPTIONAL INPUT KEYWORDS:
;    PRINT - If this keyword is set and non-zero, then the precessed
;            coordinates are displayed at the terminal.
; RESTRICTIONS:
;    Accuracy of precession decreases for declination values near 90 
;    degrees.  PRECESS should not be used more than 2.5 centures from
;    1900.    
; EXAMPLE:
;    Precess the 1950 coordinates of Eps Ind (RA = 21h 59m,33.053s,
;    DEC = (-56d, 59', 33.053") to equinox 1975.
;
;    IDL> precess, ten(21,59,33.053)*15., ten(-56,59,33.053),1950,1975,/print
;
; PROCEDURE:
;    Algorithm from Computational Spherical Astronomy by Taff (1983), 
;    p. 24. 
; REVISION HISTORY
;    Written, Wayne Landsman, STI Corporation  August 1986
;    Correct negative output RA values   February 1989
;    Added /PRINT keyword      W. Landsman   November, 1991
;    Exactly the same as the ASTRON precess routine, except single precesion.
;              JMS   Sept. 1992
;-    
On_error,2                                           ;Return to caller

NPAR = N_params()
CDR = 0.17453292519943e-1
;
if ( npar LT 2 ) then begin 
   print,'CALLING SEQUENCE - precess, ra, dec, [ equinox1, equinox2, PRINT =]
   print,'    NOTE: RA and DEC must be supplied in DEGREES'
   RETURN
endif else if (npar LT 4) then $
   read,'Enter original and new equinox of coordinates: ',equinox1,equinox2 

npts = min( [N_elements(ra), N_elements(dec)] )
if npts EQ 0 then $  
       message,'ERROR - Input RA and DEC must be vectors or scalars'

ra_rad = ra*cdr	  	;Convert to double precision if not already
dec_rad = dec*cdr 
;
a = cos(dec_rad)  

case NPTS of   	         ;Is RA a vector or scalar?
 1:    x = [a*cos(ra_rad), a*sin(ra_rad), sin(dec_rad)] ;input direction cosines
 else: begin  	
      x = fltarr(npts,3)
      x(0,0) = a*cos(ra_rad)
      x(0,1) = a*sin(ra_rad)
      x(0,2) = sin(dec_rad)
      x = transpose(x)
      end
endcase
;
csr = cdr/3600.

t = 0.001*( equinox2 - equinox1)

st = 0.001*( equinox1 - 1900.)
;                                Compute 3 rotation angles

A = CSR*T*(23042.53 + ST*(139.75 +0.06*ST) $
    +T*(30.23 - 0.27*ST+18.0*T))

B = CSR*T*T*(79.27 + 0.66*ST + 0.32*T) + A

C = CSR*T*(20046.85 - ST*(85.33 + 0.37*ST) $
    +T*(-42.67 - 0.37*ST -41.8*T))
;
sina = sin(a) &  sinb = sin(b)  & sinc = sin(c)
cosa = cos(a) &  cosb = cos(b)  & cosc = cos(c)
r = fltarr(3,3)
r(0,0) = [ cosa*cosb*cosc-sina*sinb, sina*cosb+cosa*sinb*cosc,  cosa*sinc]
r(0,1) = [-cosa*sinb-sina*cosb*cosc, cosa*cosb-sina*sinb*cosc, -sina*sinc]
r(0,2) = [-cosb*sinc, -sinb*sinc, cosc]
;
x2 = r#x 	;rotate to get output direction cosines

 if npts EQ 1 then begin	         ;Scalar

	ra_rad = atan(x2(1),x2(0))
	dec_rad = asin(x2(2))

 endif else begin	         ;Vector     

	ra_rad = fltarr(npts) + atan(x2(1,*),x2(0,*))
	dec_rad = fltarr(npts) + asin(x2(2,*))

 endelse

ra = ra_rad/cdr
ra = ra + (ra LT 0.)*360.            ;RA between 0 and 360 degrees
dec = dec_rad/cdr

if keyword_set( PRINT ) then $
   print, 'Equinox ('+strtrim(equinox2,2) + '): ',adstring(ra,dec,1)

return
end
                       

