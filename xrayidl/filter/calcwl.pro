;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       calcwl
;
;*PURPOSE:
; Calculate wavelet function
;
;*CALLING SEQUENCE:
;       calcwl,nbins,binsize,wlimg,offang,prof $
;               ,binrat=binrat,chatter=chatter
;
;*PARAMETERS:
; INPUTS:
;       nbins   - Number of bins for output image of wavelet function
;       binsize - Size of bin (in Hat scale)
;       binrat  - ratio between number of output bins and number of pixels
;                 for calculation (default = 11)
;       chatter - controls program feedback to user
;
; OUTPUTS:
;       offang  - vector containing binned off axis angles (in Hat scale) for 
;                 wavelet function profile
;       prof    - binned (1 dimensional) wavelet function radial profile 
;       wlimg  - Output 2 dimensional array of (nbins,nbins) elements
;                 containing wavelet function binned to input pixel size 
;		(energy SURFACE density)
;
;*PROCEDURE:
;   Calculates the 2 dimensional image of the wavelet function over a binrat (11) times 
;   finer grid of pixels than specified by binsize & nbins. The unique values
;   of off axis angle over this finer grid are found. These values are
;   calculated using the Mexican Hat function. 
;   The wavelet function is binned by constructing a second array (the same
;   size as the calculated wavelet function) which specifies the mapping from small
;   pixels to final bins.
;
;*RESTRICTIONS:
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
;   TABINV
;
;*MODIFICATION HISTORY:
;    written  by QDW 1/27/95
;-
;-------------------------------------------------------------------------------
pro calcwl,nbins,binsize,wlimg,offang,prof,binrat=binrat,chatter=chatter
;
npar = n_params(0)
if (npar eq 0) then begin
   print,' CALWL,nbins,binsize,wlimg,offang,prof,binrat=binrat'
   print,',chatter=chatter'
return
end
if (n_elements(chatter) eq 0) then chatter = 1      ;default = 1
;
if (n_elements(binrat) eq 0) then binrat = 1.      ;default is 11
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
; First, create a fine grid of off axis angles for calculating the wavelet function
;
npix = nbins*binrat
pixsize = binsize/binrat
cen = fix(npix/2)
if (chatter eq 1) then $
   print,' Constructing fine grid of ',npix,' by ',npix,' for calculation'
dist_circle,gridang,npix,cen,cen 
;gridang = gridang*pixsize             ;the fine grid for the calculation
;				
; Now find all the unique values of off axis angle in this grid
;
if (chatter eq 1) then $
   print,' Finding unique radii within fine grid'
Loc = long(gridang*10.)              ; accurate to 10% of the pixel size 
indsrt = sort(Loc)                     
Loc = [ Loc(indsrt), -1 ]
Ldup = where ( Loc(1:*)-Loc )
if N_elements( Ldup ) EQ 1 then $
   Kdup = Ldup(0)+1         $
   else Kdup = [ Ldup(0)+1 , Ldup(1:*)-Ldup ]       ; # duplicates at Loci.
Loc = gridang(indsrt(Ldup))*pixsize      ;now Loc = unique values of gridang
nloc = n_elements(Loc)
;
wlcalc = gridang*0.
;gridang = 0                     ;gridang not needed any more - save memory
;
; Calculate the wavelet function at each of the unique values.
;
if (chatter eq 1) then $
   print,' Calculating wavelet function'
wlloc=(2.-loc^2)*exp(-loc^2*0.5)
;
offang = Loc
Loc = 0
prof = wlloc
;
; Does user want the image as well? If not, can skip next part
;
;if (npar gt 4) then begin
  if (chatter eq 1) then $
     print,' Now filling fine grid and rebinning to ',nbins,' by ',nbins
;
  ii = 0                                 ;now fill fine wavelet function grid with proper
  n1 = ii                                ;values (given by gridang) of wlloc
  n2 = n1+kdup(ii)-1 
  wlcalc(indsrt(n1:n2)) = wlloc(ii)    ;elements n1 to n2 have same value
  for ii=1,nloc-1 do begin               ;gridang = loc(ii)
    n1 = Ldup(ii-1)+1 
    n2 = Ldup(ii-1)+kdup(ii)
    wlcalc(indsrt(n1:n2)) = wlloc(ii)  
  endfor
;
  wlimg = rebin(wlcalc,nbins,nbins)        ;use REBIN to bin to nbins X nbins
  wlimg = wlimg*binsize^2          ;normalize to area of pixels
				
;endif
if (!debug gt 1) then stop,'Stopping at end of program'
;
return
end            ;pro calcwl
