pro tvcircle0, radius, xc, yc, color, COLOR = thecolor, $
          THICK = thick, DATA= data
;+
; NAME:
;	TVCIRCLE
; PURPOSE:
;	Draw a circle or circles of specified radius centered on the cursor, or 
;	at specified position
;
; CALLING SEQUENCE:
;	TVCIRCLE, rad, x, y, color, [ COLOR = , THICK =, /DATA ]         
;
; INPUTS:
;	RAD - radius of circle(s) to be drawn, scalar
;
; OPTIONAL INPUT:
;	X - x position for circle center, vector or scalar, device coordinates
;	Y - y position for circle center, vector or scalar
;		If X and Y are not specified then program will draw circle at
;		at the current cursor position
;	COLOR - intensity value (0-255) used to draw the circle, Default =
;		!P.COLOR.   User can specify color using either this parameter
;		 or the COLOR keyword.  
;
; OPTIONAL KEYWORD INPUTS:
;	THICK - thickness of the drawn circle,  default = !P.THICK (=1)
;	DATA - if this keyword is set and non-zero, then the box width and
;		X,Y position center are interpreted as being in DATA 
;		coordinates.   Note that data coordinates must be previously
;		defined (with a PLOT or CONTOUR call).
;	COLOR - Intensity value used to draw the circle.  Default = !P.COLOR
;
; OUTPUTS:
;	None
;
; RESTRICTIONS:
;	(1) TVCIRCLE does not check whether it writes off of the edge of the 
;		display
;	(2) Some round-off error may occur when non-integral values are 
;		supplied for both the radius and the center coordinates
;
; METHOD:
;	The method used is that of Michener's, modified to take into account
;	the fact that IDL plots arrays faster than single points.   See
;	"Fundamental of Interactive Computer Graphics" by Foley and Van Dam"
;	p. 445 for the algorithm.
;
; REVISON HISTORY:
;	Original version   written by B. Pfarr  STX   10-88 
;	Major rewrite adapted from CIRCLE by Allyn Saroyan   LNLL
;		Wayne Landsman   STX     Sep. 91
;	Added DATA keyword   Wayne Landsman  HSTX    June 1993
;-

;   on_error, 2   ; Return to caller

   if n_params() lt 1 then begin
         print, $
         'Syntax - tvcircle, rad, [ xc, yc, color, COLOR = ,THICK =, /DATA ]'
         return
   endif

   if n_elements(radius) ne 1 then message, $
          'ERROR - Circle radius (first parameter) must be a scalar'
   irad = fix(radius + 0.5)

   x = 0
   y = irad 
   d = 3 - 2 * irad

   if not keyword_set(thecolor) then begin
      if n_elements( color ) eq 0 then color = !P.COLOR
   endif else color = thecolor

   if not keyword_set(THICK) then thick = !P.THICK
   
  if n_params() lt 3 then begin
     cursor, xcen, ycen, /device, /nowait
     if (xcen lt 0) or (ycen lt 0) then begin
       message,'Position cursor in window ' + strtrim(!D.WINDOW,2) + $
              ' -- then hit mouse button',/INF
         cursor, xcen, ycen, /DEVICE, /WAIT
         message,'Circle is centered at (' + strtrim(xcen,2) + ',' + $
		strtrim(ycen,2) + ')',/INF
     endif
     n_circle = 1

  endif else begin

      n_circle = min( [ n_elements(xc), n_elements(yc) ] )
      xcen = fix(xc + 0.5)  & ycen = fix(yc+0.5)

  endelse

   ; Find the x and y coordinates for one eighth of a circle.
   ; The maximum number of these coordinates is the radius of the circle.

   xhalfquad = make_array( irad + 1, /int, /nozero )
   yhalfquad = xhalfquad

   path = 0

   while x lt y $
   do begin

      xhalfquad(path) = x
      yhalfquad(path) = y

      path = path + 1

      if d lt 0 $
      then d = d + (4*x) + 6 $
      else begin

           d = d + (4*(x-y)) + 10
           y = y - 1

           end

      x = x + 1

      end

   if x eq y $
   then begin ; Fill in last point

        xhalfquad(path) = x
        yhalfquad(path) = y

        path = path + 1

        end ; Filling in last point

   ; Shrink the arrays to their correct size

   xhalfquad = xhalfquad( 0:path-1 )
   yhalfquad = yhalfquad( 0:path-1 )

   ; Convert the eighth circle into a quadrant

   xquad = [ xhalfquad, rotate(yhalfquad, 5) ]
   yquad = [ yhalfquad, rotate(xhalfquad, 5) ]

   ; Prepare for converting the quadrants into a full circle

   xquadrev = rotate( xquad(0:2*path-2), 5 )
   yquadrev = rotate( yquad(0:2*path-2), 5 )

   ; Create full-circle coordinates

   x = [ xquad, xquadrev, -xquad(1:*), -xquadrev ]
   y = [ yquad, -yquadrev, -yquad(1:*), yquadrev ]

   ; Plot the coordinates about the given center

   for i = 0, n_circle-1 do begin

    if keyword_set(DATA) then begin
      plots, x + xcen(i), y+ ycen(i), COLOR = color, /DATA, THICK = thick
    endif else begin
      plots, (x + xcen(i))/float(!d.x_vsize), (y+ ycen(i))/float(!d.y_vsize), $
        COLOR = color, /norm, THICK = thick
    endelse

   endfor
   return

end; TVCIRCLE0
