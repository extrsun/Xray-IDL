;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   NINTREG
;
;*PURPOSE:
; A procedure to read in X & Y vectors which bound a 2D area and return
; integer vectors whose elements are equal to the integers closest to 
; the values of the elements in the original vectors 
; Similar to nint, except that new values are added such that
; the minimum and maximum values of the result include the entire range of 
; the original vectors
; i.e., min(new X, Y) < min(old X, Y)
;       max(new X, Y) > max(old X, Y)
; The resulting vectors may include up to 2 more elements than the original
; vector.
; Also, the number of elements in the new X & Y are forced to be the same.
;
;*CALLING SEQUENCE:
;   nintvec,x,y,newx,newy
;
;*INPUT:
;   X - An IDL variable (vectoror array), usually floating or double
;       Must be between -32767.5 and 32767.5 to avoid integer overflow
;   Y - similar to X
;
;*OUTPUT:
;   NEWX - Nearest integers to (with values added to include entire
;            range of X)
;   NEWY - same as above for Y
;          NEWX and NEWY are forced to have the same number of elements
;
;*EXAMPLE:
;   If X = [-0.9,-0.1,0.1,0.9] then NINT(X) = [-1,0,0,1]
;   If X = [0.9,1.1,2.4] then NINTVEC(X) = [0,1,2,3]
;
;*RESTRICTIONS:
;   Input variables must have the same numbers of elements
;
;*MODIFICATION HISTORY:
;   written 5 July 1991 by GAR
;   based on NINT from Astronomical Users' Library
;-
;-------------------------------------------------------------------------------
pro nintreg,x,y,newx,newy
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'NINTREG, x, y, NEWX, NEWY'
  retall
endif
;
nelx = n_elements(x)
if (nelx le 1) then begin
  print,' Number of elements of X must be > 1. Returning.'
  retall
endif
;
nely = n_elements(y)
if (nely le 1) then begin
  print,' Number of elements of Y must be > 1. Returning.'
  retall
endif
;
if (nelx ne nely) then begin
  print,' X and Y must have the same numbers of elements. Returning.)
  retall
endif
;
xmin = min(x)
xmax = max(x)
ymin = min(y)
ymax = max(y)
newx = fix( x + 0.5 - (x lt 0) )                ;same as NINT
newy = fix( y + 0.5 - (y lt 0) )
;
newxmin = min(newx)
newxmax = max(newx)
newymin = min(newy)
newymax = max(newy)
if (xmin lt newxmin) then begin
  newx = [newxmin-1,newx]            ;subtract 1 and append to include xmin
  if (ymin lt newymin) then newy = [newymin-1,newy] $
    else newy = [newymin,newy]       ;keep the same number of elements in y
endif
if (xmax gt newxmax) then begin
  newx = [newx,newxmax+1]            ;add 1 and append to include xmax
  if (ymax gt newymax) then newy = [newy,newymax+1] $
    else newy = [newy,newymax]       ;keep the same number of elements in y
endif
;
return
end          ;pro nintreg
