;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       reg_limits
;
;*PURPOSE:
; A procedure to determine X & Y limits, given a region type and arguments.
; Input X & Y limits may be specified. If specified and smaller than the
; region limits, then the input limits will override the region limits.
; If the region limits are smaller, then these will be used to reset the
; input limits.
;
;*CALLING SEQUENCE:
;       reg_limits,regtyp,args,XLIM,YLIM
;
;*PARAMETERS:
; INPUTS
;   regtyp   ASCII string specifying type of region, e.g., circle, box.
;   args     vector containing arguments which specify region. All optional
;            arguments which are not specified in region are set to 0.0.
;   xlim     Min & max values for X
;   ylim     Min & max values for Y
; 
; OUTPUTS
;   xlim     (new) X limits
;   ylim     (new) Y limits
;
;*RESTRICTIONS:
;
;*NOTES:
;   Use parse_simple to convert IRAF type region descriptor to region type
;   and arguments
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    written 02-09-92 by GAR
;    modified 15_Apr-1992 to allow rotation to be optional argument, & to
;      fix bug in specifying limits for box (GAR)
;-
;-------------------------------------------------------------------------------
pro reg_limits,regtyp,args,xlim,ylim
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' REG_LIMITS, regtyp, args, XLIM, YLIM'
  return
endif
if (n_elements(xlim) gt 1) then begin     ;xlim must be a 2 vector
  xmin = xlim(0)
  xmax = xlim(1)
endif else begin
  xmin = 0.
  xmax = 0.
endelse
if (n_elements(ylim) gt 1) then begin     ;ylim must be a 2 vector
  ymin = ylim(0)
  ymax = ylim(1)
endif else begin
  ymin = 0.
  ymax = 0.
endelse
;
types=['annulus','box','circle','ellipse','pie','point','polygon','rotbox']
ntypes = n_elements(types)
;
pinum = 3.14159
cf = pinum/180.
indtyp = where( strlowcase(regtyp) eq types )
indtyp = indtyp(0)
;
if ( (indtyp lt 0) or (indtyp ge ntypes) ) then begin    ;not allowed
  print,regtyp,' is not one of the supported region types.'
  print,' Supported types are: ',types,' Returning.'
  return
endif
;
if ( (indtyp eq 0) or (indtyp eq 2) ) then begin    ;annulus or circle
  rad = abs(args(2))
  if (indtyp eq 0) then rad = abs(args(3))
  nxmin = args(0) - rad
  nxmax = args(0) + rad
  nymin = args(1) - rad
  nymax = args(1) + rad
  if !debug gt 2 then print,indtyp,regtyp,nxmin,nxmax,nymin,nymax
endif
;
if ( indtyp eq 3) then begin                        ;ellipse
  nargs = n_elements(args)
  if (nargs gt 4) then rotang = args(4) else rotang = 0.   ;optional 5th arg
  if (rotang eq 0) then begin
    nxmin = args(0) - args(2)
    nxmax = args(0) + args(2)
    nymin = args(1) - args(3)
    nymax = args(1) + args(3)
  endif else begin
    ang = findgen(361.)
    xx = cos(cf*ang)
    yy = args(3)*sqrt(1.-xx*xx)
    xx = args(2)*xx
    yr = yy(181:360)
    yy(181) = -yr
;
    cosang = cos(cf*args(4))
    sinang = sin(cf*args(4))
    xr = xx*cosang - yy*sinang
    yr = xx*sinang + yy*cosang
    xx = args(0) + xr
    yy = args(1) + yr
    nxmin = min(xr)
    nxmax = max(xr)
    nymin = min(yr)
    nymax = max(yr)
  endelse
  if !debug gt 2 then print,indtyp,regtyp,nxmin,nxmax,nymin,nymax
endif
;
if ( (indtyp eq 1) or (indtyp eq 7) ) then begin    ;box or rotbox
  nargs = n_elements(args)
  if (nargs gt 4) then rotang = args(4) else rotang = 0.   ;optional 5th arg
  cosang = abs(cos(rotang*cf))               
  sinang = abs(sin(rotang*cf))
  xdel = args(2)*cosang + args(3)*sinang
  ydel = args(2)*sinang + args(3)*cosang
  nxmin = args(0) - xdel/2.     ;args(2,3) give whole xwidth, yheight
  nxmax = args(0) + xdel/2.
  nymin = args(1) - ydel/2.
  nymax = args(1) + ydel/2.
  if !debug gt 2 then print,indtyp,regtyp,nxmin,nxmax,nymin,nymax
endif
;
if ( (indtyp eq 5) or (indtyp eq 6) ) then begin    ;point or polygon
  nn = n_elements(args)/2
  ind=2*indgen(nn)
  nxmin = min(args(ind))
  nxmax = max(args(ind))
  ind = ind+1
  nymin = min(args(ind))
  nymax = max(args(ind))
  if !debug gt 2 then print,indtyp,regtyp,nxmin,nxmax,nymin,nymax
endif
;
if (indtyp eq 4) then begin        ;pie requires special treatment
  if (ang le 180.) then begin      ;args(0,1) give xmax & ymax
    nxmax = args(0)
    nymax = args(1)
    nxmin = xmin
    nymin = ymin
  endif else begin
    nxmin = args(0)
    nymin = args(1)
    nxmax = xmax
    nymax = ymax
  endelse     
  if !debug gt 2 then print,indtyp,regtyp,nxmin,nxmax,nymin,nymax
endif
;
; now compare to input limits (if these are nonzero)
;
if (xmin ne 0.) then nxmin = nxmin > xmin
if (xmax ne 0.) then nxmax = nxmax < xmax
if (ymin ne 0.) then nymin = nymin > ymin
if (ymax ne 0.) then nymax = nymax < ymax
;
xlim = [nxmin,nxmax]
ylim = [nymin,nymax]
;
return
end           ;pro reg_limits
