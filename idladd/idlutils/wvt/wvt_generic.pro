; Interface for WVT_BINNING to work a user defined signal-to-noise criterion.
; Version 2.0, updated 12/05/2005
;
;######################################################################
;+
; NAME:
;     WVT_GENERIC
;
; AUTHORS:
;       Steven Diehl & Thomas S. Statler, Ohio University, US
;       diehl@phy.ohiou.edu, statler@ohio.edu
;
; PURPOSE:
;       Adaptive spatial binning of any type of 2-dimensional data with
;       the aim of obtaining a uniform signal-to-noise per bin distribution.
;
; NOTE:
;       This wrapper provides the external ADD_SIGNAL and ADD_NOISE functions
;       and gives a little hands-on tutorial on how to modify them according 
;       to your own needs. 
;
; EXPLANATION:
;       For additional information about WVT binning,
;       please refer to Diehl, S. & Statler, T.S. (2005).
;       Further information on the original VORONOI_2D_BINNING algorithm can 
;       be found in Cappellari M., Copin Y., 2003, MNRAS, 342, 345.
;
;----------------------------------------------------------------------------
;
; THE GENERAL IDEA (SHORT):
;   The structure of the main binning algorithm WVT_BINNING was held in 
;   a very general way, in order to make the algorithm more flexible and 
;   applicable to a broader variety of problems. 
;   All information that you need in order to calculate the combined
;   signal of the bin, has to be contained in a global variable named "P".
;   P is a structure generally containing vectors of length n_pixels. 
;   The functions ADD_SIGNAL and ADD_NOISE determine how to compute 
;   the combined signal and noise for the bin. The only parameter given to
;   these function are the indices of the bin members, locating the 
;   correct pixel properties in the arrays of P.
;
;
; STEP-BY-STEP GUIDE:
;
;   STEP 1: In the main wrapper (here WVT_GENERIC), define a global variable:
;
;           >COMMON DATAVALUES, P
;           >P={ pixprop1:  dblarr(npix),   $
;           >    pixprop2:  fltarr(npix),   $
;           >    pixprop3:  lonarr(npix),   $
;           >    constant1: 3.142d0     ,   $
;           >    constant2: 9L          } 
;
;           The number of tags (here: 5), as well as their names 
;           (here: 'pixprop1', 'pixprop2', 'pixprop3', 'constant1' and 
;           'constant2' are irrelevant and only used in the functions 
;           ADD_SIGNAL and ADD_NOISE, which are defined by the user. The 
;           structure should hold ALL the information that you need to add 
;           signal and noise correctly. There is no limit on how much 
;           information P can contain, and which nature it should be.
;           In this example, the 'pixprop*' tags could describe pixel 
;           properties (since they are vectors of length npix), such as  
;           the flux, counts, noise or the exposure in each pixel.
;
;   STEP 2: Define the function ADD_SIGNAL 
;
;           >FUNCTION ADD_SIGNAL, index
;           >COMMON DATAVALUES, P
;           >RETURN, total(P.pixprop1[index])
;           >END
;           
;           This is the simplest example on how your ADD_SIGNAL function 
;           could look like. This function takes, the global variable P
;           and adds up the pixel property 'pixprop1' of all bin members
;           (-> INDEX). The recipe on how compute the signal can be as 
;           easy or complicated as is required by the specific problem.
;           This would be a real example if 'pixprop1' was something like a 
;           flux per pixel.
;
;   STEP 3: Define the function ADD_NOISE 
;
;           >FUNCTION ADD_NOISE, index
;           >COMMON DATAVALUES, P
;           >RETURN, sqrt(total(P.pixprop2[index]^2))
;           >END
;           
;           The usage of ADD_NOISE is completely analogous to ADD_SIGNAL, 
;           with the only difference that the combined noise of the bin
;           should be returned. In the example case, the square root of 
;           the sum of all squares of 'pixprop2', similar to a real example
;           of adding gaussian errors.
;
;   STEP 4: Call the main binning program WVT_BINNING
;        
;
; MODIFICATION HISTORY:
;       V1.0: Provided the functions ADD_SIGNAL and ADD_NOISE in this wrapper.
;           Usage is now similar to the original code by Cappellari & Copin.
;           (07/12/2005)
;       V2.0: First published version (12/05/2005)
;
;----------------------------------------------------------------------------
FUNCTION ADD_SIGNAL, index
  ; Define how to add the signal for the bin defined by 'index'
  COMMON DATAVALUES, P
  ; Put your own prescription here:
  RETURN, total(P.pixprop1[index])
END

;----------------------------------------------------------------------------
FUNCTION ADD_NOISE, index
  ; Define how to add the noise for the bin defined by 'index'
  COMMON DATAVALUES, P
  ; Put your own prescription here:
  RETURN, sqrt(total(P.pixprop2[index]^2))
END

;----------------------------------------------------------------------------
PRO WVT_GENERIC, x, y, pixelSize, targetSN, $
    class, xNode, yNode, area, QUIET=quiet, $
    dens=dens, binvalue=binvalue, snbin=snbin, plotit=plotit, $
    resume=resume, neighborlist=neighborlist, max_area=max_area, $
    gersho=gersho, keepfixed=keepfixed
  ; Generic wrapper for the WVT_BINNING procedure. 

  ; Define the global variable P, rename the tags to whatever you want,
  ; but be consistent with ADD_SIGNAL and ADD_NOISE
  COMMON DATAVALUES, P
  P={ pixprop1:  dblarr(npix),   $
      pixprop2:  fltarr(npix),   $
      pixprop3:  lonarr(npix),   $
      constant1: 3.142d0     ,   $
      constant2: 9L          } 
 
  ; Now that ADD_SIGNAL and ADD_NOISE, as well as the structure P are 
  ; defined, you can call the main binning program. Simply delete all 
  ; the keywords which are not necessary for your specific cause.  
  WVT_BINNING, x, y, pixelSize, targetSN, $
    class, xNode, yNode, area, QUIET=quiet, $
    dens=dens, binvalue=binvalue, snbin=snbin, plotit=plotit, $
    resume=resume, neighborlist=neighborlist, max_area=max_area, $
    gersho=gersho, keepfixed=keepfixed

END

