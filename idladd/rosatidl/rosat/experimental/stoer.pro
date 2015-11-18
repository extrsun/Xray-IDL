;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   stoer
;
;*PURPOSE:
;   A procedure used for field line tracing in SHELLG
;
;*CALLING SEQUENCE:
;   stoer,bq,r,g,nmax,pvals
;
;*PARAMETERS:
; INPUTS:
;       bq    - 
;       r     - 
;       g     - normalized field coefficients 
;       nmax  - maximum order of spherical harmonics
;       pvals -
;
; OUTPUTS:
;       pvals -
;
;*RESTRICTIONS:
;
;*NOTES:
;  PVALS will be updated
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    adapted  03 Feb 1994 (GAR) from Fortran code STOER.FOR from NSSDC
;-
;-------------------------------------------------------------------------------
pro stoer,bq,r,g,nmax,pvals
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' STOER, bq, r, g, nmax, PVALS'
  retall
endif
;
;  XM, YM, ZM  are geomagnetic cartesian inverse co-ordinates
;
zm = pvals(2)
fli = pvals(0)*pvals(0) + pvals(1)*pvals(1) + 1e-15
r = 0.5*( fli + sqrt(fli*fli + 4.*zm*zm) )
rq = r*r
wr = sqrt(r)
xm = pvals(0)*wr
ym = pvals(1)*wr
;
;  Transform to geographic co-ordinate system
;
u = fltarr(3,3)
u(0,0) = [+0.3511737,-0.9148385,-0.1993679]
u(0,1) = [+0.9335804,+0.3583680,+0.0000000]
u(0,2) = [+0.0714471,-0.1861260,+0.9799247]
xi = fltarr(3)
xi(0) = xm*u(0,0) + ym*u(0,1) + zm*u(0,2)
xi(1) = xm*u(1,0) + ym*u(1,1) + zm*u(1,2)
xi(2) = xm*u(2,0)             + zm*u(2,2)
;
;  Compute derivatives
;
feldi,xi,g,nmax,h                ;do not pass h back to thr calling procedure
q = h(0)/rq
dx = h(2) + h(2) + q*xi(0)
dy = h(3) + h(3) + q*xi(1)
dz = h(1) + h(1) + q*xi(2)
;
;  Transform back to geomagnetic co-ordinate system
;
dxm = u(0,0)*dx + u(1,0)*dy + u(2,0)*dz
dym = u(0,1)*dx + u(1,1)*dy
dzm = u(0,2)*dx + u(1,2)*dy + u(2,2)*dz
dr = (xm*dxm + ym*dym + zm*dzm)/r
;
;  Form slowly varying expressions
;
pvals(3) = (wr*dxm - 0.5*pvals(0)*dr)/(r*dzm)
pvals(4) = (wr*dym - 0.5*pvals(1)*dr)/(r*dzm)
dsq = rq*(dxm*dxm + dym*dym + dzm*dzm)
bq = dsq*rq*rq
pvals(5) = sqrt(dsq/(rq + 3.*zm*zm))
pvals(6) = pvals(5)*(rq + zm*zm)/(rq*dzm)
;
return
end          ;pro stoer
