;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       calcl
;
;*PURPOSE:
; Calculate a cluter image
;
;*CALLING SEQUENCE:
;    calcl,nbins,binsize,climg,offang,prof,binrat=binrat,chatter=chatter
;    ,modelp=modelp   
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
;       climg  - Output 2 dimensional array of (nbins,nbins) elements
;                 containing the cluster image binned to input pixel size 
;			(in a probability distribution)
;
;*PROCEDURE:
;   Calculates the 2 dimensional image of a cluster, which could be
; on a fine grid of pixels than specified by binsize & nbins. The unique values
;   of off axis angle over this finer grid are found. 
;
;*MODIFICATION HISTORY:
;    written  by QDW 7/13/96
;-
;-------------------------------------------------------------------------------
pro calcl,nbins,binsize,climg,offang,prof,binrat=binrat,chatter=chatter $
	,modelp=modelp
;
npar = n_params(0)
if (npar eq 0) then begin
   print,' CALCL,nbins,binsize,climg,offang,prof,binrat=binrat'
   print,',chatter=chatter,modelp=modelp'
return
end
if (n_elements(chatter) eq 0) then chatter = 1      ;default = 1
if (n_elements(binrat) eq 0) then binrat = 1.      ;default is 11
;
; First, create a fine grid of off axis angles 
;
npix = nbins*binrat
cen =(npix-1.)*0.5
if (chatter eq 1) then $
   print,' Constructing fine grid of ',npix,' by ',npix,' for calculation'
dist_circle,gridang,npix,cen,cen 
;
; Now find all the unique values of off axis angle in this grid
;
if (chatter eq 1) then $
   print,' Finding unique radii within fine grid'
Loc = long(gridang*10.) ;accuate to 1/10 of the fine pixel size   
get_unique,loc,indsrt,Ldup,Kdup            
Loc = gridang(indsrt(Ldup))*(binsize/binrat)
	;now Loc = unique values of gridang
;
; Calculate the wavelet function at each of the unique values.
;
if (chatter eq 1) then $
   print,' Calculating wavelet function'
cal_model,loc,clloc,modelp=modelp
;
  if (chatter eq 1) then $
     print,' Now filling fine grid and rebinning to ',nbins,' by ',nbins
;
clcalc=gridang*0.
unique_image,clloc,indsrt,Ldup,Kdup,clcalc
climg = rebin(clcalc,nbins,nbins)        ;use REBIN to bin to nbins X nbins
;
if (npar ge 4) then begin
	offang = Loc
	prof = clloc
endif

;if (!debug gt 1) then stop,'Stopping at end of program'
;
return
end           
