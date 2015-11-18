;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
; NAME:   
;      filldata
;
;*PURPOSE: 
;      Given 2 sets of (x,y) vectors, to fill the values in Y1 with the
;        values in Y2, when the X2 values are a subset of the X1 values
;
;*CALLING SEQUENCE:
;      filldata, x, y, xfill, YFILL
;
;*INPUT PARAMETERS: 
;      X -  Vector containing the current independent variable grid.
;      Y -  Vector containing the current dependent variable values at 
;           the X grid points.
;      XFILL -  Vector containing the new independent variable grid 
;               points, which contains the values of X
;
;*OUTPUT PARAMETERS:
;      YFILL  - Vector with the filled value(s) of the dependent variable
;               for X(I-1) lt XFILL(J) le X(I+1), YFILL(J) = Y(I+1)
;                           
;*EXAMPLE:
;    To make a vector of livetime factors FLIVCAS over the times of aspect 
;       measurement SCTCAS, given livetime factors FLIVEVR defined at the 
;       event rate times SCTEVR:
;
;       IDL> filldata,sctevr,flivevr,sctcas,flivcas
;   
;*RESTRICTIONS:
;    X and XFILL must be monotonically increasing or decreasing
;
;*PROCEDURE: 
;
;*MODIFICATION HISTORY:
;    Written 24 Oct 1992 (GAR
;-
;-------------------------------------------------------------------------------
pro filldata, x, y, xfill, yfill
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' FILLDATA, x, y, xfill, YFILL
  retall & endif
;
s = size(x) & ndimx = s(0) 
if (ndimx gt 1) then begin
  print,' Sorry. For now, X must be a 1 dimensional vector. Returning.'
  retall & endif
nx = n_elements(x)
;
s = size(xfill) & ndimxf=s(0) 
if (ndimxf gt 1) then begin
  print,' Sorry. For now, XFILL must be a 1 dimensional vector. Returning.'
  retall & endif
nxf = n_elements(xfill)
;
ny = n_elements(y)
if (ny ne nx) then begin
  print,' Sorry. X and Y must be the same size. Returning.'
  retall & endif
;
tabinv,x,xfill,ifill
ifill = fix(ifill + 1)
yfill = y(ifill)
;
return
end        ;pro filldata
