;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   feldcof
;
;*PURPOSE:
;   A procedure to determine coefficients and dipole moment from IGRF models
;
;*CALLING SEQUENCE:
;   feldcof,year,dte1,nmax1,gh1,dte2,nmax2,gh2,dimo,g,nmax
;
;*PARAMETERS:
; INPUTS:
;       year    - decimal year for which geomagnetic field is to be calculated
;                 (must be single-valued)
;       dte1    - Date of earlier model                                 
;       nmax1   - Maximum degree and order of earlier model             
;       gh1     - Schmidt quasi-normal internal spherical harmonic coefficients 
;                 of earlier model                
;       dte2    - Date of later model
;       nmax2   - Maximum degree and order of later model
;       gh2     - Schmidt quasi-normal internal spherical harmonic coefficients 
;                 of either later model or rate-of-change model
;
; OUTPUTS:
;       dimo    - Geomagnetic dipole moment in Gauss (normalized to Earth's 
;                 radius) at the time (year)
;       g       - normalized field coefficients 
;       nmax    - maximum order of spherical harmonics
;
;*RESTRICTIONS:
;  YEAR must be single-valued
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  GETSHC
;  INTERPSHC
;  EXTRAPSHC
;
;*MODIFICATION HISTORY:
;    adapted 23 Jan 1994 (GAR) from Fortran code FELDCOF.FOR from 
;    D. BILITZA, NSSDC, GSFC, CODE 633, GREENBELT, MD 20771, (301)286-9536
;    NOV 1987.
;    Modified so that the IGRF files are read outside the subroutine, so
;    that it can be run in a loop more efficiently  
;    modified 15 Feb 1994 (GAR) to fix bugs in defining L and DIMO
;-
;-------------------------------------------------------------------------------
pro feldcof,year,dte1,nmax1,gh1,dte2,nmax2,gh2,dimo,g,nmax
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' FELDCOF,year,dte1,nmax1,gh1,dte2,nmax2,gh2,DIMO,G,NMAX'
  retall
endif
;
;  Determine IGRF coefficients for year
;  numye is number of years represented by IGRF models
;
numye = 10
iyea = fix(year/5.)*5
L = (iyea - 1945)/5
L = (L > 0) < (numye - 1)
;if (!debug gt 6) then stop,' Stopping in FELDCOF after defining L'
;                                                                       
;  IS=0 for Schmidt normalization.   IS=1 for Gauss normalization.
;  IU  is input unit number for IGRF coefficient sets.
;
iu = 10
is = 0
;
if (L lt (numye-1)) then $
   interpshc,year,dte1,nmax1,gh1,dte2,nmax2,gh2,nmax,gha else $
   extrapshc,year,dte1,nmax1,gh1,nmax2,gh2,nmax,gha
;if (!debug gt 6) then stop,' Stopping in FELDCOF after interp/extrap(shc)'
;
;  Determine magnetic dipole moment and coeffiecients G
;
dimo = sqrt( 0.D0 + total((gha(0:2)*1.D-5)*(gha(0:2)*1.D-5)) )
;if (!debug gt 5) then stop,' Stopping in FELDCOF after dimo defined'
;
if (is eq 0) then f0 = -1.D-5 else f0 = 1.D-5
fact = 1./sqrt(2.D0)      
g = 0
g = fltarr(144)       ;This seems to be the default size in SHELLIG
;g = gha*0.            ;This will now be the output model - g(0) = 0.0
;
i = 1                                ;in SHELLIG: I = 2
for n=1,nmax do begin                
  x = double(n)                    
  if (is eq 0) then begin
    f0 = 0.5 * f0 * x 
    f = f0 * fact
  endif else begin
    f0 = f0 * x * x / (4.D0 * x - 2.D0)
    f = f0 * 0.5D0                                    
  endelse
  g(i) = gha(i-1) * f0
  i = i+1
;  nn = n < (nmax-1)                 
  for m=1,n do begin             
    if (is eq 0) then f = f * sqrt((x + m) / (x - m + 1.D0)) else $
                      f = f * (x + m) / (x - m + 1.D0)
    g(i) = gha(i-1:i) * f
    i = i+2
  endfor
;  if (!debug gt 6) then stop,' Stopping in FELDCOF at end of outer loop'
endfor
;if (is eq 0) then f = f * sqrt((x + m) / (x - m + 1.D0)) else $
;                  f = f * (x + m) / (x - m + 1.D0)
;g(i) = gha(i-1) * f
;if (!debug gt 5) then stop,' Stopping in FELDCOF at the end'
;
return
end          ;pro feldcof
