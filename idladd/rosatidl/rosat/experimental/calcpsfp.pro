;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       calcpsfp
;
;*PURPOSE:
; Calculate Rosat PSPC on axis point spread function for an assumed input
;   counts spectrum
;
;*CALLING SEQUENCE:
;       calcpsfp,nbins,binsize,offcen,rate,offang,prof,psfimg,group=group $
;               ,ecen=ecen,binrat=binrat,chatter=chatter
;
;*PARAMETERS:
; INPUTS:
;       nbins   - Number of bins for output image of psf 
;       binsize - Size of bin (in arcsec)
;       offcen  - Off axis angle of (center of) source (in arcmin)
;       rate    - vector containing observed (or assumed) counting rate for 
;                 each of the channels specified by group
;       group   - pha channel boundaries (e.g., as specified by !group)
;       ecen    - central energies
;                 Note: if group and ecen are both defined, then ecen will
;                       be redefined according to group
;       binrat  - ratio between number of output bins and number of pixels
;                 for calculation (default = 11)
;       chatter - controls program feedback to user
;
; OUTPUTS:
;       ecen    - central energies for each element of rate
;                 will be calculated (or overwritten) if group is defined
;       offang  - vector containing binned off axis angles (in arcsec) for 
;                 psf profile
;                 The center of offang will be the off-axis angle of the 
;                 center of the PSF
;       prof    - binned (1 dimensional) PSF radial profile 
;                 Will be centered on the center of the computed PSF
;       psfimg  - Output 2 dimensional array of (nbins,nbins) elements
;                 containing PSF binned to input pixel size (in counts)
;                 (normalized so that integral from 0 to infinity = 1)
;
;*PROCEDURE:
;   Calculates the 2 dimensional image of the psf over a binrat (11) times 
;   finer grid of pixels than specified by binsize & nbins. The unique values
;   of off axis angle over this finer grid are found, and the psf at these
;   angles is calculated for each pha channel energy. The psf is summed
;   over energy. The psf is binned by constructing a second array (the same
;   size as the calculated psf) which specifies the mapping from small
;   pixels to final bins.
;
;   The following is taken from the comments written by G. R. Hasinger:
;
;   The PSPC on-axis PSF is a combination of three, physically well 
;   understood terms:
;
;   1. a Gaussian for the intrinsic PSPC resolution due to the inherent
;      statistics of the primary electron generation. Theoretically
;      the Gaussian Sigma is propotrional to 1/SQRT(Energy)
;
;   2. an exponential function due to the finite penetration depth of
;      the X-rays in the counter gas combined with the 8.5 degree cone
;      angle. The PSPC is focussed for 1 keV; the 'chromatic aberration'
;      is largest for large energies
;
;   3. A Lorentz function for the mirror scattering; theoretically, the
;      behavior increases as the square of the energy if the grazing 
;      angle is constant, so diffraction forces the shape parameters to
;      vary as inverse energy.
;
;   Technically, these 3 components should be folded together, but their
;   angular domains are sufficiently separated that simply adding them
;   together is reasonably accurate.  Detailed comparison with calibration
;   data at monochromatic energies of 0.28, 0.93, 1.49, and 1.70keV provide
;   the parameter values.  No fit is possible below 0.15keV (channel 15) 
;   due to the PSPC electronics which give rise to additional 'ghost
;   images'. These events should be avoided as far as possible in PSF 
;   modelling.
;
;   The off-axis blur of the telescope, although highly structured and
;   asymmetric, can be modeled by a simple Gaussiane in its radially
;   integrated profile.  This Gaussian is added in quadrature to the
;   Gaussian of the detector.  Since the PSF is not convolved, but a 
;   simple addition of terms, the contribution of the exponential term
;   must be diminished while the Gaussian is "eating up" the exponential.
;   This is modelled as a Gaussian decay of the exponential term as a 
;   function of the off-axis angle.
;
;*RESTRICTIONS:
;   For now, program must be able to access file pscpb_mar11.ebds (so
;   CHAN2ENERGY will work)
;
;*NOTES:
;   The central energy of each pha channel "group" is used.
;   Be careful - large images (> 50 or so bins) will slow the calculation down
;   considerably!
;   Nbins and binrat will be set to the nearest odd integers, so that the
;   centering will come out right
;
;*SUBROUTINES CALLED:
;   NINT
;   DIST_CIRCLE
;   CHAN2ENERGY
;   RSPSFP
;   TABINV
;   NINT
;
;*MODIFICATION HISTORY:
;    written 23 June 1992 (GAR)
;    modifed 13 July 1992 (GAR) to be compatible with MAKE_PSF, and to use
;       REBIN
;    modified 08 Aug 1992 (GAR) keyword chatter added
;    modified 10 Jul 1993 (GAR) to delete lines that were commented out
;    modified 12 July 1993 (GAR) to include description in prologue and
;       to incorporate changes for calculating off-axis psf
; 	add keywords: sigmaw -- weighted sigma of the total Gaussian 
; 	component of the PSPC psf, and prof1 - the Gaussian component of the
;	psf. These two parameters are used by psf_frac.pro. wqd, 4/14/96
;-
;-------------------------------------------------------------------------------
;pro calcpsfp,nbins,binsize,offcen,rate,offang,prof,psfimg,group=group,ecen=ecen,binrat=binrat,chatter=chatter
pro calcpsfp,nbins,binsize,offcen,rate,offang,prof,psfimg,group=group,$
ecen=ecen,binrat=binrat,chatter=chatter,sigmaw=sigmaw,prof1=prof1
;modified by wqd, April 14, 1996 
;
npar = n_params(0)
if (npar eq 0) then begin
   print,' CALCPSFP, nbins, binsize, offcen (0), rate, OFFANG, PROF, PSFIMG,'
   print,'           group=group (def = !group), ecen=ecen, binrat=binrat' $
        ,' (def=11), chatter=chatter'
   retall
end
if (n_elements(chatter) eq 0) then chatter = 1      ;default = 1
;
if (n_elements(ecen) eq 0) then begin               ;use channel groupings
  ecalc = 1              ;will need to calculate central energies
  if (n_elements(group) eq 0) then group = !group   ;use !group as default
  s = size(group) & nchan = s(1)
endif else begin
  if (n_elements(group) eq 0) then begin            ;use central energies
    ecalc = 0 
    nchan = n_elements(ecen)
  endif else begin                                  ;ecen will be redefined
    ecalc = 1
    s = size(group) & nchan = s(1)
  endelse
endelse
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
; Calculate the psf at each of the unique values, for each energy.
; For each channel group, weight by the number of counts. Sum over channels
; and divide by the total number of counts to keep the right normalization.
;
if (chatter eq 1) then $
   print,' Calculating psf at various energies'
psfloc = fltarr(nloc)                  ;psf calculated at each angle
psf1loc = fltarr(nloc)  ;added by wqd
sigmaw=0.
if (ecalc) then $
   chan2energy,ecen,edel,group=group   ;calculate channel central energies
for ii=0,nchan-1 do begin
;	rspsfp,offcen,ecen(ii),loc,temp,ierr=ierr ;ecen must be in keV
  rspsfp,offcen,ecen(ii),loc,temp,ierr=ierr,sigma=sigma,term1=term1 
	;modified by wqd, April 14, 1996   
;print,'sigma=',sigma
  sigmaw=sigmaw+rate(ii)*sigma
  psfloc = psfloc + rate(ii)*temp      ;weight by counts and add
  psf1loc = psf1loc + rate(ii)*term1
endfor  
psfloc = psfloc/total(rate)            ;renormalize profile so integral = 1
psf1loc = psf1loc/total(rate)
sigmaw=sigmaw/total(rate)
;
offang = Loc
Loc = 0
prof = psfloc
prof1 = psf1loc
;
; Does user want the image as well? If not, can skip next part
;
if (npar gt 6) then begin ;change from 5 to 6 by wqd, april 17, 1996
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
if (!debug gt 1) then stop,'Stopping at end of CALCPSFP'
;  
return
end            ;pro calcpsfp
