;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   shellg_getl
;
;*PURPOSE:
;   A procedure to calculate the MacIlwain L-value for specified 
;     Geodetic coordinates, altitude, and geomagnetic field model.
;     (Meant to be called by SHELLG or similar routine.)
;     Ref. G. Kluge, European Space Operations Centre, Internal Note 67 1970.
;
;*CALLING SEQUENCE:
;   SHELLG_GETL, x, dimo, g, nmax, FL, ICODE, B0
;
;*PARAMETERS:
; INPUTS:
;       x      - 
;       dimo   - Geomagnetic dipole moment in Gauss (normalized to Earth's 
;                radius) 
;
; OUTPUTS:
;       fl     - L-value
;       icode  - Code for program completion:
;                = 1  Normal completion
;                = 2  Unphysical conjugate point (fl meaningless)
;                = 3  Shell parameter greater than limit up to which
;                     accurate calculation is required; approximation is used
;       b0     - Magnetic field strength in Gauss
;
;*RESTRICTIONS:
;   X must be a 3-element vector, defined for one Geodetic position. DIMO 
;     must be single valued.
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    adapted  03 Feb 1994 (GAR) from Fortran code SHELLG.FOR
;    Fortran code modified by D. BILITZA (NSSDC), NOV 87 to use the correct
;    dipole moment (a change in the common block) and to use the IGRF
;    Earth magnetic field models from 1945 to 1990
;    modified 16 Feb 1994 (GAR) to fix logic bugs in translation from Fortran
;-
;-------------------------------------------------------------------------------
pro shellg_getl,x,dimo,g,nmax,fl,icode,b0
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' SHELLG_GETL, x, dimo, g, nmax, FL, ICODE, B0'
  retall
endif
;
u = fltarr(3,3)
u(0,0) = [+0.3511737,-0.9148385,-0.1993679]
u(0,1) = [+0.9335804,+0.3583680,+0.0000000]
u(0,2) = [+0.0714471,-0.1861260,+0.9799247]
;
;  RMIN, RMAX are boundaries for identification of icode=2 and 3
;  STEP is step size for field line tracing
;  STEQ is step size for integration
; 
rmin = 0.05
rmax = 1.01
step = 0.20
steq = 0.03
bequ = 1.E10
;
rq = 1./(total(x*x))
r3h = sqrt(rq*sqrt(rq)) 
p = fltarr(8,100)
p(0,1) = total( x*u(0:2,0)      )*r3h
p(1,1) = total( x(0:1)*u(0:1,1) )*r3h
p(2,1) = total( x*u(0:2,2)      )*rq 
;
;  First three points of field line 
;
step = -abs(p(2,1))/p(2,1)*step      ;In Fortran, step = -sign(step,p(2,1)) 
if (!debug eq 5) then print,' SHELLG_GETL: ',x,rq,r3h,p(0:2,1),step
;
pvals = p(0:6,1)
stoer,bq2,r2,g,nmax,pvals
if (!debug gt 5) then print,' SHELLG_GETL R2: bq2,r2,pvals ',bq2,r2,pvals
if (!debug gt 6) then stop,' Stopping in SHELLG_GETL after STOER for R2'
p(0,1) = pvals
;
b0 = sqrt(bq2) 
p(0,2) = p(0,1) + 0.5*step*p(3,1)
p(1,2) = p(1,1) + 0.5*step*p(4,1)
p(2,2) = p(2,1) + 0.5*step
pvals = p(0:6,2)
stoer,bq3,r3,g,nmax,pvals
if (!debug gt 5) then print,' SHELLG_GETL R3: bq3,r3,pvals ',bq3,r3,pvals
if (!debug gt 6) then stop,' Stopping in SHELLG_GETL after STOER for R3'
p(0,2) = pvals
;
p(0,0) = p(0,1) - step*(2.*p(3,1) - p(3,2))
p(1,0) = p(1,1) - step*(2.*p(4,1) - p(4,2))
p(2,0) = p(2,1) - step
pvals = p(0:6,0)
stoer,bq1,r1,g,nmax,pvals
if (!debug gt 5) then print,' SHELLG_GETL R1: bq1,r1,pvals ',bq1,r1,pvals
if (!debug gt 6) then stop,' Stopping in SHELLG_GETL after STOER for R1'
p(0,0) = pvals
;
p(0,2) = p(0,1) + step*(20.*p(3,2) - 3.*p(3,1) + p(3,0))/18.
p(1,2) = p(1,1) + step*(20.*p(4,2) - 3.*p(4,1) + p(4,0))/18.
p(2,2) = p(2,1) + step
pvals = p(0:6,2)
stoer,bq3,r3,g,nmax,pvals
if (!debug gt 5) then print,' SHELLG_GETL R3: bq3,r3,pvals ',bq3,r3,pvals
if (!debug gt 6) then stop,' Stopping in SHELLG_GETL after STOER for R3'
p(0,2) = pvals
;
;  Invert sense if required 
;
if (bq3 gt bq1) then begin
  step = -step
  r3 = r1
  bq3 = bq1
  for I=0,6 do begin
    zz = p(I,0)
    p(I,0) = p(I,2)
    p(I,2) = zz
  endfor
  if (!debug eq 5) then print,' Inverted: ',step,r1,r3,p(0:6,0),p(0:6,2)
endif
;
;  Search for lowest magnetic field strength
;
bequ = bq1 < bq2 < bq3
case 1 of
  (bequ eq bq1): iequ = 1
  (bequ eq bq2): iequ = 2
  (bequ eq bq3): iequ = 3
endcase
if (!debug gt 5) then print,' SHELLG_GETL: ',bq1,bq2,bq3,bequ,iequ
if (!debug gt 6) then stop,' Stopping in SHELLG_GETL after bequ def'
;
;  Initialization of integration loops
;
step12 = step/12.
step2 = 2.*step
steq = abs(step)/step*steq           ;In Fortran, steq = sign(steq,step)
fi = 0.
icode = 1
oradik = 0.
radik = 99999.                       ;just to start off with
oterm = 0.
stp = r2*steq
z = p(2,1) + stp
stp = stp/0.75
p(7,0) = step2*(p(0,0)*p(3,0) + p(1,0)*p(4,0))
p(7,1) = step2*(p(0,1)*p(3,1) + p(1,1)*p(4,1))
if (!debug gt 5) then print,' Loop init: ',step,step12,step2,steq,fi,icode,$
   radik,oradik,oterm,stp,p(2,1),z,p(0:4,0:1),p(7,0:1)
if (!debug gt 6) then stop,' Stopping in SHELLG_GETL after loops initialized'
;
;  Main loop (field line tracing) - but loop should really be done only
;  while 2.*radik > oradik
;
N = 2
while ( ((2.*radik) gt oradik) and (N le 3332) ) do begin
;
;  Corrector (field line tracing)
;
  p(0,N) = p(0,N-1) + step12*( 5.*p(3,N) + 8.*p(3,N-1) - p(3,N-2) )
  p(1,N) = p(1,N-1) + step12*( 5.*p(4,N) + 8.*p(4,N-1) - p(4,N-2) )
;
;  Prepare expansion coefficients for interpolation
;    of slowly varying quantities
;
  p(7,N) = step2*( p(0,N)*p(3,N) + p(1,N)*p(4,N) )
  if (!debug gt 5) then begin
    print,' P vals: ',N
    print,p(0:4,N-1:N),p(5:7,N-2:N)
  endif
  c0 = p(0,N-1)*p(0,N-1) + p(1,N-1)*p(1,N-1)
  c1 = p(7,N-1)
  c2 = 0.25*( p(7,N) - p(7,N-2) )
  c3 = ( p(7,N) + p(7,N-2) - 2.*c1 )/6.0
  d0 = p(5,N-1)
  d1 = 0.5*( p(5,N) -p(5,N-2) )
  d2 = 0.5*( p(5,N) + p(5,N-2) - 2.*d0 )
  e0 = p(6,N-1)
  e1 = 0.5*( p(6,N) - p(6,N-2) )
  e2 = 0.5*( p(6,N) + p(6,N-2) - 2.*e0 )
  if (!debug gt 5) then print,' Exp coef: ',N,c0,c1,c2,c3,d0,d1,d2,e0,e1,e2,$
     radik,2.*radik,oradik
;
;  Inner loop (for quadrature)
;
  t = ( z - p(2,N-1) )/step         ;Need to do this here to start the loop
  if (!debug gt 5) then print,' T etc.: ',N,p(2,N-1),step,z,t
  tcheck = 1                        ;Need this to make the logic work
  if (t le 1) then begin
    tcheck = 0                      ;T started as le 1, then increased
    while ( (t le 1) and (2.*radik gt oradik) ) do begin
      t = ( z - p(2,N-1) )/step     ;and here as well so t will get updated
      if (!debug gt 5) then print,' T LE 1: ',N,p(2,N-1),step,z,t
;
      hli = 0.5*( ( (c3*t + c2)*t + c1)*t + c0 )
      zq = z*z
      r = hli+sqrt(hli*hli+zq)
      if (!debug gt 5) then print,'   t,hli,z,zq,r,rmin,rmax: ',$
         t,hli,z,zq,r,rmin,rmax
;
      if (r le rmin) then begin          ;Approximation for high values of L.
        icode = 3
        t = -p(2,N-1)/step
        fl = 1./(abs(((c3*t + c2)*t + c1)*t + c0) + 1E-15)
        return
      endif
;
      rq = r*r
      ff = sqrt(1. + 3.*zq/rq)
      radik = b0 - ( (d2*t+d1)*t + d0 )*r*rq*ff
      if (!debug gt 5) then print,'   r,rq,zq,ff,d1,d2,radik: ',$
         r,rq,zq,ff,d1,d2,radik
;
      if (r gt rmax) then begin
        icode = 2
        radik = radik - 12.*(r-rmax)*(r-rmax)
      endif
      if (!debug gt 5) then print,'   r,rmax,radik,2.*radik,oradik: ',$
         r,rmax,radik,2.*radik,oradik
;
      if (2.*radik gt oradik) then begin
        term = sqrt(radik) * ff*( (e2*t+e1)*t + e0 )/(rq + zq)
        fi = fi+stp*(oterm+term)
        oradik = radik
        oterm = term
        stp = r*steq
        z = z+stp
        if (!debug gt 5) then print,'   rq,zq,e0,e1,e2,t,ff,radik,term,'+ $
           'oterm,stp,fi: ',rq,zq,e0,e1,e2,t,ff,radik,term,oterm,stp,fi
      endif
      if (!debug gt 6) then stop,' Stopping in SHELLG_GETL where T LE 1'
    endwhile
;
  endif 
;
;  Predictor (field line tracing) - Do this part only if T started as gt 1
;  (no matter how 2.*radik compares to oradik) or if the WHILE loop on T
;  was exited because 2.*radik gt oradik
;
  if ( (tcheck) or (2.*radik gt oradik) ) then begin
    if (!debug gt 5) then print,' T GT 1: ',N,p(2,N-1),step,z,t
    p(0,N+1) = p(0,N) + step12*(23.*p(3,N) - 16.*p(3,N-1) + 5.*p(3,N-2) )
    p(1,N+1) = p(1,N) + step12*(23.*p(4,N) - 16.*p(4,N-1) + 5.*p(4,N-2) )
    p(2,N+1) = p(2,N) + step
    pvals = p(0:6,N+1)
    stoer,bq3,r3,g,nmax,pvals
    if (!debug gt 5) then print,' SHELLG_GETL R3: N,bq3,r3,pvals ',$
       N,bq3,r3,pvals
    if (!debug gt 7) then stop,' Stopping in SHELLG_GETL after STOER for R3'
    p(0,N+1) = pvals
    if (!debug gt 7) then stop,' Stopping in SHELLG_GETL where T GT 1'
;
;  Search for lowest magnetic field strength
;
    if (bq3 lt bequ) then begin
      iequ = N + 2
      bequ = bq3
    endif
    if (!debug gt 7) then stop,' Stopping in SHELLG_GETL where T GT 1'
  endif
;
  N = N + 1        ;increment the loop
endwhile
;
iequ = iequ > 2 
sp = fltarr(3)
sp(0) = p(0,iequ-2)
sp(1) = p(1,iequ-2)
sp(2) = p(2,iequ-2)
if (oradik ge 1e-15) then fi = fi + stp/0.75*oterm*oradik/(oradik-radik)
;
;-- The minimal allowable value of FI was changed from 1E-15 to 1E-12,
;-- because 1E-38 is the minimal allowable arg. for ALOG in our envir.
;-- D. Bilitza, Nov 87.
;
fi = 0.5*abs(fi)/sqrt(b0) + 1e-12
;
;  Compute L from b and i.  Same as CARMEL in invar.
;
;-- Correct dipole moment is used here. D. Bilitza, Nov 87.
;
dimob0 = dimo/b0
xx = alog(fi*fi*fi/dimob0)
case 1 of
  (xx le -22.): gg = 3.33338E-1*xx + 3.0062102E-1
  ( (xx gt -22.) and (xx le -3.) ): begin
                                    gg = (-8.1537735E-14*xx + 8.3232531E-13)*xx
                                    gg = (gg + 1.0066362E-9)*xx
                                    gg = (gg + 8.1048663E-8)*xx
                                    gg = (gg + 3.2916354E-6)*xx
                                    gg = (gg + 8.2711096E-5)*xx
                                    gg = (gg + 1.3714667E-3)*xx
                                    gg = (gg + 1.5017245E-2)*xx
                                    gg = (gg + 4.3432642E-1)*xx + 6.2337691E-1
                                    end
  ( (xx gt -3.) and (xx le +3.) ):  begin
                                    gg = (2.6047023E-10*xx + 2.3028767E-9)*xx
                                    gg = (gg - 2.1997983E-8)*xx
                                    gg = (gg - 5.3977642E-7)*xx
                                    gg = (gg - 3.3408822E-6)*xx
                                    gg = (gg + 3.8379917E-5)*xx
                                    gg = (gg + 1.1784234E-3)*xx
                                    gg = (gg + 1.4492441E-2)*xx
                                    gg = (gg + 4.3352788E-1)*xx + 6.228644E-1
                                    end
  ( (xx gt +3.) and (xx le 11.7) ): begin
                                    gg = (6.3271665E-10*xx - 3.958306E-8)*xx
                                    gg = (gg + 9.9766148E-7)*xx
                                    gg = (gg - 1.2531932E-5)*xx
                                    gg = (gg + 7.9451313E-5)*xx
                                    gg = (gg - 3.2077032E-4)*xx
                                    gg = (gg + 2.1680398E-3)*xx
                                    gg = (gg + 1.2817956E-2)*xx
                                    gg = (gg + 4.3510529E-1)*xx + 6.222355E-1
                                    end
  ( (xx gt 11.7) and (xx le 23.) ): begin
                                    gg = (2.8212095E-8*xx - 3.8049276E-6)*xx
                                    gg = (gg + 2.1702240E-4)*xx
                                    gg = (gg - 6.7310339E-3)*xx
                                    gg = (gg + 1.2038224E-1)*xx
                                    gg = (gg - 1.8461796E-1)*xx + 2.0007187E0
                                    end
  (xx gt 23.0): gg = xx - 3.0460681E0
endcase
fl = exp( alog( (1.+exp(gg))*dimob0 )/3.0 )
;
return
end               ;pro shellg_getl
