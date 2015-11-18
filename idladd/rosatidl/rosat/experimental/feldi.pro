;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   feldi
;
;*PURPOSE:
;   A procedure to calculate Earth's magnetic field from the spherical
;     harmonics model of G. Kluge, European Space Operations Centre, 
;     Internal Note 61, 1970.
;   Used in connection with L-calculation program SHELLG.
;
;*CALLING SEQUENCE:
;   FELDI, xi, g, nmax, H
;
;*PARAMETERS:
; INPUTS:
;       xi    -
;       g     - normalized field coefficients 
;       nmax  - maximum order of spherical harmonics
;
; OUTPUTS:
;       h     -
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    adapted 23 Jan 1994 (GAR) from Fortran code FELDG.FOR
;    Fortran code modified by D. BILITZA (NSSDC), NOV 87 to read the magnetic
;    field coefficients from binary data files instead of from block data
;    and to calculate the dipole moment
;    modified 15 Feb 1994 (GAR) to get rid of FOR loop on M
;-
;-------------------------------------------------------------------------------
pro feldi,xi,g,nmax,h
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' FELDI, xi, g, nmax, H'
  retall
endif
;
era = 6371.2           ;Earth radius for normalization of Cartesian coord.s
erequ = 6378.16        ;Major half axis for Earth ellipsoid (in km)
erpol = 6356.775       ;Minor half axis for Earth ellipsoid (in km)
aquad = erequ*erequ    ;Square of major half axis
bquad = erpol*erpol    ;Square of minor half axis
umr = 1./!radeg        ;conversion from degrees to radians
;
ihmax = nmax*nmax + 1
last = ihmax + 2*nmax
imax = 2*nmax - 1
h = g*0.
h(ihmax-1) = g(ihmax-1:last-1)      ;IDL indices begin with zero
;
for K=1,3,2 do begin
  i = imax
  ih = ihmax - 1                    ;in SHELLIG: IH = IHMAX
  while (K lt (I+2)) do begin
    il = ih - i
    f = 2./float(I-K+2)                                         
    x = xi(0)*f
    y = xi(1)*f
    z = xi(2)*2.*f
    I = I - 2  
    if (!debug gt 7) then print,' FELDI: K,imax,ihmax,i,ih,il,f,xi,x,y,z =',$
       K,imax,ihmax,i,ih,il,f,xi,x,y,z
;    if (!debug gt 7) then stop,' Stopping in FELDI'
    if (I gt 1) then begin
      ilm = il + 3 + 2*indgen((i-3)/2+1)
      ihm = ih + 3 + 2*indgen((i-3)/2+1)
      h(ilM+1) = g(ilM+1) + z*h(ihM+1) + x*( h(ihM+3) - h(ihM-1) ) $
                    - y*( h(ihM+2) + h(ihM-2) )   
      h(ilM) = g(ilM) + z*h(ihM) + x*( h(ihM+2) - h(ihM-2) ) $
                  + y*( h(ihM+3) + h(ihM-1) )
    endif
;
    if (I ge 1) then begin
      h(il+2) = g(il+2) + z*h(ih+2) + x*h(ih+4) - y*( h(ih+3) + h(ih) )
      h(il+1) = g(il+1) + z*h(ih+1) + y*h(ih+4) + x*( h(ih+3) - h(ih) )
    endif
;
    h(il) = g(il) + z*h(ih) + 2.*( x*h(ih+1) + y*h(ih+2) ) 
;
    ih = il 
    if (!debug gt 8) then stop,' Stopping in FELDI at end of inner while loop'
  endwhile
;  if (!debug gt 6) then stop,' Stopping in FELDI at end of outer for loop'
endfor
;
return
end            ;pro feldi
