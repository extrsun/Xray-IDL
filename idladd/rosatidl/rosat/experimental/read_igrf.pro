;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   read_igrf
;
;*PURPOSE:
;   A procedure to read the IGRF model data from the IGRF data files
;
;*CALLING SEQUENCE:
;   read_igrf,year,dte1,nmax1,gh1,dte2,nmax2,gh2,igrfdir=igrfdir
;
;*PARAMETERS:
; INPUTS:
;       year    - decimal year for which geomagnetic field is to be calculated
;                 (must be single-valued)
;
; OPTIONAL INPUTS:
;       igrfdir - directory containing the magnetic field data files
;
; OUTPUTS:
;       dte1    - Date of earlier model                                 
;       nmax1   - Maximum degree and order of earlier model             
;       gh1     - Schmidt quasi-normal internal spherical harmonic coefficients 
;                 of earlier model                
;       dte2    - Date of later model
;       nmax2   - Maximum degree and order of later model
;       gh2     - Schmidt quasi-normal internal spherical harmonic coefficients 
;                 of either later model or rate-of-change model
;
;*RESTRICTIONS:
;  YEAR must be single-valued
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    adapted 23 Jan 1994 (GAR) from code for FELDCOF in SHELLIG.FOR 
;    Fortran code from D. BILITZA, NSSDC, GSFC, CODE 633, GREENBELT, 
;    MD 20771, (301)286-9536, NOV 1987.
;    modified 15 Feb 1994 (GAR) to fix bug in defining L
;-
;-------------------------------------------------------------------------------
pro read_igrf,year,dte1,nmax1,gh1,dte2,nmax2,gh2,igrfdir=igrfdir
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' READ_IGRF,year,DTE1,NMAX1,GH1,DTE2,NMAX2,GH2,igrfdir=igrfdir'
  retall
endif
if (n_elements(igrfdir) eq 0) then igrfdir = ''
;
filmod = ['dgrf45.dat','dgrf50.dat','dgrf55.dat','dgrf60.dat','dgrf65.dat',$
          'dgrf70.dat', 'dgrf75.dat', 'dgrf80.dat','dgrf85.dat',$
          'igrf90.dat','igrf90s.dat']
filmod = igrfdir+filmod
dtemod = [1945.,1950.,1955.,1960.,1965.,1970.,1975.,1980.,1985.,1990.,1995.]
;
;  numye is number of years represented by IGRF models
;
numye = 10
;                                                                       
;  IS=0 for Schmidt normalization.   IS=1 for Gauss normalization.
;  IU  is input unit number for IGRF coefficient sets.
;
iu = 10
is = 0
;
;  Determine IGRF-years for input-year
;
iyea = fix(year/5.)*5
L = (iyea - 1945)/5
L = (L > 0) < (numye - 1)
dte1 = dtemod(L)   
fil1 = filmod(L)   
dte2 = dtemod(L+1) 
fil2 = filmod(L+1) 
;
;  Get IGRF coefficients for the boundary years
;
getshc,fil1,nmax1,erad,gh1,ier  
if (ier ne 0) then begin
  print,' Error in getting IGRF coefficients for the 1st year. Returning.'
  retall
endif
getshc,fil2,nmax2,erad,gh2,ier
if (ier ne 0) then begin
  print,' Error in getting IGRF coefficients for the 2nd year. Returning.'
  retall
endif
;
return
end            ;pro read_igrf
