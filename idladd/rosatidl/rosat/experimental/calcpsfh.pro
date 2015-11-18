;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       calcpsfp
;
;*PURPOSE:
; Calculate Rosat HRI (on axis) point spread function
;
;*CALLING SEQUENCE:
;       calcpsfh,nbins,binsize,offcen,offang,prof,psfimg $
;               ,binrat=binrat,chatter=chatter
;
;*PARAMETERS:
; INPUTS:
;       nbins   - Number of bins for output image of psf 
;       binsize - Size of bin (in arcsec)
;       offcen  - Off axis angle of (center of) source (in arcmin)
;       binrat  - ratio between number of output bins and number of pixels
;                 for calculation (default = 11)
;       chatter - controls program feedback to user
;
; OUTPUTS:
;       offang  - vector containing binned off axis angles (in arcsec) for 
;                 psf profile
;       prof    - binned (1 dimensional) psf radial profile 
;       psfimg  - Output 2 dimensional array of (nbins,nbins) elements
;                 containing psf binned to input pixel size (in counts)
;                 (normalized so that integral from 0 to infinity = 1)
;
;*PROCEDURE:
;   Calculates the 2 dimensional image of the psf over a binrat (11) times 
;   finer grid of pixels than specified by binsize & nbins. The unique values
;   of off axis angle over this finer grid are found. These values are
;   calculated using the functional from from 27 Mar 1992. The calculated
;   psf is the energy averaged response.
;   The psf is binned by constructing a second array (the same
;   size as the calculated psf) which specifies the mapping from small
;   pixels to final bins.
;
;*RESTRICTIONS:
;   For now, program only works on axis
;   Uses functional form from 27 Mar 1992; this is an energy averaged
;     response
;
;*NOTES:
;   Be careful - large images (> 50 or so bins) will slow the calculation down
;   considerably!
;   Nbins and binrat will be set to the nearest odd integers, so that the
;   centering will come out right
;
;*SUBROUTINES CALLED:
;   NINT
;   DIST_CIRCLE
;   RSPSFH
;   TABINV
;
;*MODIFICATION HISTORY:
;    written  16 Aug 1992 (GAR) (adapted from calcpsfp)
;    modified 31 Jul 1993 (GAR) to clean up documentation prologue
;    modified 03 Mar 1994 (GAR) to be compatible with new version of RSPSFH
;       which includes off-axis parameterization.
;-
;-------------------------------------------------------------------------------
pro calcpsfh,nbins,binsize,offcen,offang,prof,psfimg,binrat=binrat,$
    chatter=chatter
;
npar = n_params(0)
if (npar eq 0) then begin
   print,' CALCPSFH, nbins, binsize, offcen (0), OFFANG, PROF, PSFIMG,'
   print,'           binrat=binrat (def=11), chatter=chatter'
   retall
end
if (n_elements(chatter) eq 0) then chatter = 1      ;default = 1
;
if (n_elements(binrat) eq 0) then binrat = 11.      ;default is 11
;
; Make sure that nbins and binrat are both odd integers
;
check = 2.*fix(nbins/2.)
if (check eq nbins) then begin
  print,'Number of bins',nbins,' is not an odd integer. Resetting to '$
       ,nbins+1
  nbins = nbins + 1
endif
check = 2.*fix(binrat/2.)
if (check eq binrat) then begin
  print,' Bin ratio',binrat,' is not an odd integer. Resetting to '$
       ,binrat+1.
  binrat = binrat + 1.
endif
;
; First, create a fine grid of off axis angles for calculating the psf
;
npix = nbins*binrat
pixsize = binsize/binrat
cen = fix(npix/2)
if (chatter eq 1) then $
   print,' Constructing fine grid of ',npix,' by ',npix,' for calculation'
dist_circle,gridang,npix,cen,cen 
gridang = gridang*pixsize              ;the fine grid for the calculation
;
; Now find all the unique values of off axis angle in this grid
; (to within an accuracy of 0.1 arcsec)
;
if (chatter eq 1) then $
   print,' Finding unique radii within fine grid'
Loc = long(gridang*10.)                ;find the unique values of gridang
indsrt = sort(Loc)                     ;(within an accuracy of 0.1 arcsec)
Loc = [ Loc(indsrt), -1 ]
Ldup = where ( Loc(1:*)-Loc )
if N_elements( Ldup ) EQ 1 then $
   Kdup = Ldup(0)+1         $
   else Kdup = [ Ldup(0)+1 , Ldup(1:*)-Ldup ]       ; # duplicates at Loci.
Loc = gridang(indsrt(Ldup))             ;now Loc = unique values of gridang
nloc = n_elements(Loc)
;
psfcalc = gridang*0.
gridang = 0                     ;gridang not needed any more - save memory
;
; Calculate the psf at each of the unique values.
;
if (chatter eq 1) then $
   print,' Calculating psf'
rspsfh,offcen,loc,psfloc,ierr=ierr     ;psf calculated at each angle
;
offang = Loc
Loc = 0
prof = psfloc
;
; Does user want the image as well? If not, can skip next part
;
if (npar gt 4) then begin
  if (chatter eq 1) then $
     print,' Now filling fine grid and rebinning to ',nbins,' by ',nbins
;
  ii = 0                                 ;now fill fine psf grid with proper
  n1 = ii                                ;values (given by gridang) of psfloc
  n2 = n1+kdup(ii)-1 
  psfcalc(indsrt(n1:n2)) = psfloc(ii)    ;elements n1 to n2 have same value
  for ii=1,nloc-1 do begin               ;gridang = loc(ii)
    n1 = Ldup(ii-1)+1 
    n2 = Ldup(ii-1)+kdup(ii)
    psfcalc(indsrt(n1:n2)) = psfloc(ii)  
  endfor
;
  psfimg = rebin(psfcalc,nbins,nbins)        ;use REBIN to bin to nbins X nbins
  psfimg = psfimg*binrat*binrat              ;REBIN *averages*
  psfimg = psfimg*(pixsize*pixsize)          ;normalize to area of pixels
endif
if (!debug gt 1) then stop,'Stopping at end of program'
;
return
end            ;pro calcpsfh
