;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   feldc
;
;*PURPOSE:
;   A procedure to calculate Earth's magnetic field from the spherical
;     harmonics model of G. Kluge, European Space Operations Centre, 
;     Internal Note 61, 1970.
;
;*CALLING SEQUENCE:
;   FELDC, year, v, DIMO, BNORTH, BEAST, BDOWN, BABS, XI, G, NMAX, H
;
;*PARAMETERS:
; INPUTS:
;       year   - decimal year for which geomagnetic field is to be calculated
;       v      - 3-element vector of Cartesian coordinates in Earth radii 
;                (6371.2 km)
;                X-axis pointing to equator at 0 longitude
;                Y-axis pointing to equator at 90 long.
;                Z-axis pointing to north pole
;
; OUTPUTS:
;       dimo   - Geomagnetic dipole moment in Gauss (normalized to Earth's 
;                radius) at the time (year)
;	bnorth - Component of the magnetic field which points North
;                with respect to to the local Geodetic coordinate system
;                (i.e., with axis pointing in the tangential plane to the 
;                North)
;       beast  - Eastward pointing component of the magnetic field
;       bdown  - Downward pointing component of the magnetic field
;       babs   - magnetic field strength in Gauss
;       xi     - ?? 
;       g      - normalized field coefficients (from FELDCOF)
;       nmax   - maximum order of spherical harmonics (from FELDCOF)
;       h      - ?? (quantity from FELDI)
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  FELDCOF
;  GETSHC
;  INTERPSHC
;  EXTRAPSHC
;  FELDI
;
;*MODIFICATION HISTORY:
;    adapted 23 Jan 1994 (GAR) from Fortran code FELDG.FOR
;    Fortran code modified by D. BILITZA (NSSDC), NOV 87 to read the magnetic
;    field coefficients from binary data files instead of from block data
;    and to calculate the dipole moment
;-
;-------------------------------------------------------------------------------
pro feldc,year,glat,glon,alt,dimo,bnorth,beast,bdown,babs,xi,g,nmax,h
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' FELDC, year, v, DIMO, BNORTH, BEAST, BDOWN, BABS, XI, G, NMAX, H'
  retall
endif
;
xxx = v(1)
yyy = v(2)
zzz = v(3)
;
rq = 1./(xxx*xxx + yyy*yyy + zzz*zzz)
nq = n_elements(rq)
xi = fltarr(nq,3)
xi(0,0) = xxx*rq
xi(0,1) = yyy*rq
xi(0,2) = zzz*rq
;
feldcof,year,dimo,g,nmax    ;Get the magnetic dipole and coefficients
;
feldi,xi,g,nmax,h           ;Compute the quantity h (whatever that is)
;
s = .5*h(0) + 2.*( h(1)*xi(*,2) + h(2)*xi(*,0) + h(3)*xi(*,1) )
t = (rq + rq)*sqrt(rq)
bxxx = t * ( h(2) - s*xxx )
byyy = t * ( h(3) - s*yyy )
bzzz = t * ( h(1) - s*zzz )
babs = sqrt( bxxx*bxxx + byyy*byyy + bzzz*bzzz )
brho = byyy*sp + bxxx*cp
bnorth = bzzz*st - brho*ct
bdown = -bzzz*ct - brho*st
;
return
end           ;pro feldc
