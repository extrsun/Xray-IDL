;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   extrapshc
;
;*PURPOSE:
;   A procedure to linearly extrapolate a spherical harmonic model with a 
;   rate-of-change model.                                          
;
;*CALLING SEQUENCE:
;   extrapshc,date,dte1,nmax1,gh1,nmax2,gh2,NMAX,GH
;
;*PARAMETERS:
; INPUTS:
;       date   - Date of resulting model (in decimal year)          
;       dte1   - Date of base model                                 
;       nmax1  - Maximum degree and order of base model             
;       gh1    - Schmidt quasi-normal internal spherical harmonic coefficients 
;                of base model                
;       nmax2  - Maximum degree and order of rate-of-change model
;       gh2    - Schmidt quasi-normal internal spherical harmonic coefficients 
;                of rate-of-change model      
;
; OUTPUTS:
;       gh     - Coefficients of resulting model                    
;       nmax   - Maximum degree and order of resulting model        
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    adapted 22 Jan 1994 (GAR) from Fortran code FELDCOF.FOR from NSSDC
;    written by A. Zunde                                                       
;    USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225      
;-
;-------------------------------------------------------------------------------
pro extrapshc,date,dte1,nmax1,gh1,nmax2,gh2,nmax,gh                     
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' EXTRAPSHC, date, dte1, nmax1, gh1, nmax2, gh2, NMAX, GH'
  retall
endif
;
;  The coefficients (GH) of the resulting model, at date DATE, are computed by 
;  linearly extrapolating the coefficients of the base model (GH1), at date 
;  DTE1, using those of the rate-of-change model (GH2), at date DTE2. If one 
;  model is smaller than the other, the extrapolation is performed with the 
;  missing coefficients assumed to be 0.       
;
factor = (date - dte1)
nmin = nmax1 < nmax2
k = nmin * (nmin + 2)
nmax = nmax1 > nmax2
L = nmax * (nmax + 2)
gh = fltarr(L)
gh(0) = gh1(0:k-1) + factor * gh2(0:k-1)
case 1 of
  (nmax1 eq nmax2): continue
  (nmax1 gt nmax2): gh(k) = gh1(k:*)
  (nmax1 lt nmax2): gh(k) = factor * gh2(k:*)
endcase
;
return
end       ;pro extrapshc
