;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   get_bfield
;
;*PURPOSE:
;   A procedure to calculate Earth's magnetic field from the spherical
;     harmonics model of G. Kluge, European Space Operations Centre, 
;     Internal Note 61, 1970.
;
;*CALLING SEQUENCE:
;   GET_BFIELD, year, glat, glon, alt, DIMO, BNORTH, BEAST, BDOWN, BABS, $
;               LVAL, CODE, B0, igrfdir=igrfdir, chatter=chatter
;
;*PARAMETERS:
; INPUTS:
;       year    - decimal year for which geomagnetic field is to be calculated
;                 Can be a vector, but values must not cross the 5-year 
;                 boundary of the corresponding IGRF file
;       glat    - Geodetic latitude in decimal degrees (North)
;       glon    - Geodetic longitude in decimal degrees (East)
;       alt     - altitude in km above sea level
;
; OPTIONAL INPUTS:
;       igrfdir - Directory containing the magnetic field IGRF data files
;       chatter - Controls program feedback to user ("chattiness")
;                 (default = 1)
;
; OUTPUTS:
;       dimo    - Geomagnetic dipole moment in Gauss (normalized to Earth's 
;                 radius) at the time (year)
;       bfield  - data structure containing the geomagnetic field vectors
;                 has the structure of replicate(row,nval), where
;                 row = {magfvector,north:0.0,east:0.0,down:0.0,abs:0.0}
;           north = Component of the magnetic field which points North
;                   with respect to to the local Geodetic coordinate system
;                   (i.e., with axis pointing in the tangential plane to the 
;                   North)
;           east  = Eastward pointing component of the magnetic field
;           down  = Downward pointing component of the magnetic field
;           abs   = magnetic field strength in Gauss
;       and nval is the total number of measurements
;
;       lval    - L-value
;       code    - Code for program completion:
;                 = 1  Normal completion
;                 = 2  Unphysical conjugate point (fl meaningless)
;                 = 3  Shell parameter greater than limit up to which
;                      accurate calculation is required; approximation is used
;       b0      - Magnetic field strength in Gauss
;
;*RESTRICTIONS:
;  Values in YEAR must not cross the 5-year boundary of the corresponding
;  IGRF file
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  READ_IGRF
;  FELDCOF
;  GETSHC
;  INTERPSHC
;  EXTRAPSHC
;  FELDG
;  FELDI
;  SHELLG_GETL
;
;*MODIFICATION HISTORY:
;    adapted  23 Jan 1994 (GAR) from Fortran code FELDG.FOR
;    Fortran code modified by D. BILITZA (NSSDC), NOV 87 to read the magnetic
;    field coefficients from binary data files instead of from block data
;    and to calculate the dipole moment
;    modified 02 Feb 1994 (GAR) to also calculate MacIlwain L-values
;    modified 04 Feb 1994 (GAR) to print '% done' in loop when CHATTER ge 2
;    modified 15 Feb 1994 (GAR) to fix bug in defining L
;-
;-------------------------------------------------------------------------------
pro get_bfield,year,glat,glon,alt,dimo,bfield,lval,code,b0,igrfdir=igrfdir,$
    chatter=chatter
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'GET_BFIELD,year,glat,glon,alt,DIMO,BFIELD,LVAL,CODE,B0,' + $
        'igrfdir=igrfdir,chatter=chatter'
  retall
endif
if (n_elements(chatter) eq 0) then chatter = 1
if (n_elements(igrfdir) eq 0) then igrfdir = ''
;
;  Determine IGRF-years for input-year
;  numye is number of years represented by IGRF models
;
numye = 10
iyea = fix(year/5.)*5
L = (iyea - 1945)/5 
L = (L > 0) < (numye - 1)
if (min(L) ne max(L)) then begin
  print,' Error in GET_BFIELD: Values in YEAR cross a 5-year IGRF boundary.'
  print,' Please check your inputs. Returning.'
  retall
endif 
;
; Read the data for the spherical harmonics models from the IGRF files
;
read_igrf,year(0),dte1,nmax1,gh1,dte2,nmax2,gh2,igrfdir=igrfdir
;
era = 6371.2           ;Earth radius for normalization of Cartesian coord.s
erequ = 6378.16        ;Major half axis for Earth ellipsoid (in km)
erpol = 6356.775       ;Minor half axis for Earth ellipsoid (in km)
aquad = erequ*erequ    ;Square of major half axis
bquad = erpol*erpol    ;Square of minor half axis
umr = 1./!radeg        ;conversion from degrees to radians
;
rlat = umr * glat
ct = sin(rlat)
st = cos(rlat)
rlat = 0                           ;free up some memory
d = sqrt(aquad - (aquad - bquad)*ct*ct)
rlon = umr * glon
cp = cos(rlon)
sp = sin(rlon)
rlon = 0                           ;free up some memory
zzz = (1./era)*(alt + bquad/d)*ct  ;same as x(2) in SHELLG.FOR
rho = (1./era)*(alt + aquad/d)*st  
d = 0                              ;free up some memory
xxx = rho*cp                       ;same as x(0) in SHELLG.FOR
yyy = rho*sp                       ;same as x(1) in SHELLG.FOR
rho = 0                            ;free up some memory
;
rq = 1./(xxx*xxx + yyy*yyy + zzz*zzz)
;
;  Now do the magnetic field calculations, using FELDCOF, FELDG, and
;    SHELLG.
;  Since the number of elements in both YEAR and the spherical harmonics
;    vectors can be large (>1000 and > 100), do this in a loop.
;
nval = n_elements(year)
;bxxx = year*0.
bxxx = fltarr(nval)
byyy = bxxx
bzzz = bxxx
dimo = bxxx
;
lval = fltarr(nval)
code = intarr(nval)
b0 = fltarr(nval)
;
checkval = nint( nval*0.1*(1+indgen(10)) )      ;in case chatter ge 2
for nn=0,nval-1 do begin
;
;  Get the magnetic dipole and coefficients
;
  feldcof,year(nn),dte1,nmax1,gh1,dte2,nmax2,gh2,dm,g,nmax
  dimo(nn) = dm
  if (!debug gt 4) then print,' GET_BFIELD: nn,dm,dimo ',nn,dm,dimo(nn)
;
;  Compute the Geomagnetic field quantities if requested
;
  if (npar gt 5) then begin
;
;  Compute the quantity h (whatever that is) 
;
    x = [xxx(nn),yyy(nn),zzz(nn)]
    xi = rq(nn)*x
    feldi,xi,g,nmax,h           
    if (!debug gt 5) then stop,'Stopping in GET_BFIELD nn loop after FELDI'
;
;  Now do the rest of the calculation from FELDG
;
    s = .5*h(0) + 2.*( h(1)*xi(2) + h(2)*xi(0) + h(3)*xi(1) )
    t = 2.*rq(nn)*sqrt(rq(nn))
    bxxx(nn) = t * ( h(2) - s*xxx(nn) )
    byyy(nn) = t * ( h(3) - s*yyy(nn) )
    bzzz(nn) = t * ( h(1) - s*zzz(nn) )
    if (!debug gt 3) then print,' GET_BFIELD: ',nn,xi,s,t,bxxx(nn),$
        byyy(nn),bzzz(nn)
    if (!debug gt 4) then stop,'Stopping in GET_BFIELD after bxxx, etc. def'
  endif
;
;  Now do the rest of the calculation from SHELLG if requested
;
  if (npar gt 6) then begin
    shellg_getl,x,dm,g,nmax,fl,icode,bzero
;
    lval(nn) = fl
    code(nn) = icode
    b0(nn) = bzero
    if (!debug gt 3) then print,' GET_BFIELD: ',nn,x,dm,lval(nn),code(nn),$
        b0(nn)
    if (!debug gt 4) then stop,'Stopping in GET_BFIELD after SHELLG'
  endif
;
;  Let the user know how things are progressing (if chatter ge 2)
;
  if (chatter ge 2) then begin
    icheck = where(nn eq checkval,ncheck)
    if (ncheck gt 0) then $
       print,systime(0),'   ',nint(float(nn)/nval*100.),'  % done'
  endif
endfor
if (!debug gt 3) then stop,'Stopping in GET_BFIELD after nn loop'
;
;  Define the output magnetic field data structure (if requested)
;
if (npar gt 5) then begin
  row = {magfvector,north:0.0,east:0.0,down:0.0,abs:0.0}
  bfield = replicate(row,nval)
;
  bfield.abs = sqrt( bxxx*bxxx + byyy*byyy + bzzz*bzzz )
  bfield.east = byyy*cp - bxxx*sp
  brho = byyy*sp + bxxx*cp
  bfield.north = bzzz*st - brho*ct
  bfield.down = -bzzz*ct - brho*st
endif
if (!debug gt 0) then stop,'Stopping in GET_BFIELD at end'
;
return
end           ;pro get_bfield
