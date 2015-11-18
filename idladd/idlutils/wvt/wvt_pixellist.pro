; Interface for WVT_BINNING to work with pixel lists. 
; Version 2.0, updated 12/05/2005
;
;######################################################################
;+
; NAME:
;     WVT_PIXELLIST
;
; AUTHORS:
;       Steven Diehl & Thomas S. Statler, Ohio University, US
;       diehl@phy.ohiou.edu, statler@ohio.edu
;
; PURPOSE:
;       Adaptive spatial binning of any type of 2-dimensional data with
;       the aim of obtaining a uniform signal-to-noise per bin distribution.
;       WVT_PIXELLIST provides an interface to directly use pixel lists, 
;       similar to the code VORONOI_2D_BINNING (Cappellari & Copin, 2003), 
;       which this algorithm was inspired by.
;
; NOTE:
;       This procedure is identical in use to the more flexible WVT_BINNING
;       but doesn't require the external ADD_SIGNAL and ADD_NOISE functions.
;       Signal is supposed to simply add, whereas noise is expected to add
;       in quadrature. Look at WVT_GENERIC for a template on how to define
;       your own signal-to-noise criterion.
;
; EXPLANATION:
;       For additional information about WVT binning,
;       please refer to Diehl, S. & Statler, T.S. (2005).
;       Further information on the original VORONOI_2D_BINNING algorithm can 
;       be found in Cappellari M., Copin Y., 2003, MNRAS, 342, 345.
;
; CALLING SEQUENCE:
;       WVT_LIST, x, y, pixelSize, targetSN, class, xNode, yNode, area 
;         [, /QUIET=quiet, dens=dens, binvalue=binvalue, snbin=snbin, $
;         plotit=plotit, resume=resume, neighborlist=neighborlist, 
;         max_area=max_area, gersho=gersho, xray=xray, keepfixed=keepfixed ]
;
; INPUTS (REQUIRED):
;            X: Vector containing the X coordinate of the pixels to bin.
;               It is assumed here that pixels are arranged in a regular
;               grid, so that the PIXELSIZE is a well defined quantity.
;               The pixel grid however can contain holes (some pixels can be
;               excluded from the binning, e.g point sources in X-ray data) 
;               and can have an irregular boundary. However, the data should be
;               continuous, as the uniformity of the S/N distribution can not 
;               be guaranteed otherwise. Arbitrary units can be used 
;               (e.g. arcsec or pixels).
;            Y: Vector (same size as X) containing the Y coordinate
;               of the pixels to bin.
;    PIXELSIZE: Side length of one pixel. Pixels are assumed to be square.
;     TARGETSN: The desired signal-to-noise ratio in the final
;               2D-binned data. A TARGETSN between ~4-10 is standard for X-ray 
;               images, a temperature map would require higher TARGETSN 
;               (~30-100), depending on the spectral model used. For integral 
;               field spectroscopy, a TARGETSN of ~50 per pixel may be a
;               reasonable value to extract stellar kinematics
;               information from galaxy spectra.
;       SIGNAL: Vector (same size as X) containing the signal
;               associated with each pixel, having coordinates (X,Y).
;               If pixels are the actual pixels of the CCD in a galaxy
;               image, the signal will be simply the counts in each pixel.
;               The signal list can be background subtracted or exposure map 
;               corrected, as long as the NOISE vector reflects this. 
;               If the `pixels' are actually the apertures of an
;               integral-field spectrograph, then the signal can be
;               defined as the average flux in the spectral range under
;               study, for each aperture.
;        NOISE: Vector (same size as X) containing the corresponding
;               noise associated with each pixel.
;
; OUTPUTS:
;        CLASS: Vector (same size as X) containing the bin number assigned
;               to each input pixel. The index goes from zero to Nbins-1.
;               This vector alone is enough for making *any* subsequent
;               computation on the binned data. Everything else is optional.
;        XNODE: Vector (size Nbins) of the X coordinates of bin generators, 
;               i.e the geometric centers of the bins.
;        YNODE: Vector (size Nbins) of Y coordinates of the bin generators.
;        SNBIN: Vector (size Nbins) with the final SN of each bin.
;         AREA: Vector (size Nbins) with the number of pixels for each bin.
;     BINVALUE: Vector (size Nbins) with the final signal of each bin
;
; KEYWORDS (OPTIONAL):
;       PLOTIT: Set this keyword to one to produce a plot of the 2-D
;               bins and of the corresponding S/N at the end of the
;               computation. A value of two will produce a similar plot 
;               after the bin accretion step. Setting PLOTIT higher than 
;        QUIET: by default the program shows the progress while accreting
;               pixels and then while iterating the CVT. Set this keyword
;               to avoid printing progess results. 
;       GERSHO: Use Gersho's conjecture with normal Voronoi tesselations, 
;               instead of the WVT algorithm. Except for some changes in the 
;               bin accretion step, this procedures is identical to the one 
;               implemented in Cappellari & Copin's code VORONOI_2D_BINNING.
;               Be aware, that Gersho's conjecture is only valid for strictly
;               positive data, where the S/N adds in quadrature!
;       RESUME: Set this keyword, if you want to start from an existing WVT, 
;               which is uniquely defined by the vectors XNODE, YNODE, and 
;               WEIGHT. The WVT iteration scheme will be applied to the 
;               supplied WVT, and the values overwritten with the final output.
;
; EXTERNAL INTERACTIVE CONTROLS
;       PLOTIT: If you decide to plot the iteration steps, create 
;               file in the current directory with the name 'plotit'. A simple
;               way to do this is to issue the command 'touch plotit' in a 
;               shell. Remove the file if you want to stop plotting.
;    STOPMENOW: If you want to terminate the iteration at the next iteration
;               create a file named 'stopmenow' in the current directory (e.g 
;               'touch stopmenow').
;
; PROCEDURES PROVIDED:
;       The following procedures are contained in the main WVT_BINNING program.
;       Refer to the main code for further explanations.
;            WVT_PIXELLIST                 -- main interface
;
; FUNCTIONS PROVIDED:
;       The following functions are contained in the main WVT_PIXELLIST 
;       program. Refer to the main code for further explanations.
;            ADD_SIGNAL                    -- adds the signal of a bin
;            ADD_NOISE                     -- adds the noise of a bin
;
; NOTE:
;       WVT_BINNING uses the function MATCH from the IDL astro library. We
;       included this function in the main program release for completeness.
;       Feel free to remove the file match.pro if you already have the IDL 
;       astro library (see idlastro.gsfc.nasa.gov/homepage.html) installed.
;
; MODIFICATION HISTORY:
;       V1.0: Provided the functions ADD_SIGNAL and ADD_NOISE in this wrapper.
;           Usage is now similar to the original code by Cappellari & Copin.
;           (07/12/2005)
;       V1.1: Fixed bug when incompatible ADD_SIGNAL or ADD_NOISE functions
;           are precompiled. (09/08/2005)
;       V2.0: First published version (12/05/2005)
;
;
;----------------------------------------------------------------------------

FUNCTION ADD_SIGNAL, index
  ; returns the signal inside the bin, specified by the indeces of the bin members
  COMMON DATAVALUES, P
  return, total(P.signal[index])/n_elements(index)
END

;----------------------------------------------------------------------------
FUNCTION ADD_NOISE, index
  ; returns the noise inside the bin, specified by the indeces of the bin members
  COMMON DATAVALUES, P
  return, sqrt(total(P.noise[index]^2))/n_elements(index)
END

;----------------------------------------------------------------------------
PRO WVT_PIXELLIST, x, y, signal, noise, targetSN, $
    xNode, yNode, weight, binnumber=binnumber, area=area, QUIET=quiet, $
    dens=dens, binvalue=binvalue, snbin=snbin, plotit=plotit, $
    resume=resume, neighborlist=neighborlist, max_area=max_area, $
    gersho=gersho, keepfixed=keepfixed, center=center, pixelsize=pixelsize
  ; Simple wrapper for the WVT_BINNING procedure. 
  COMMON DATAVALUES, P
  P={ signal:signal, noise:noise } 
  IF NOT(keyword_set(pixelsize)) THEN pixelsize=1.  

  ; Check if the ADD_SIGNAL and ADD_NOISE functions originate from this file
  ; If not, stop the procedure before you do something stupid...
  faddsignal = routine_info('add_signal',/source,/functions)
  faddnoise  = routine_info('add_noise' ,/source,/functions)
  fthisfile = routine_info('wvt_pixellist',/source)
  IF strcmp(faddsignal.path, fthisfile.path) EQ 0 $
    OR strcmp(faddnoise.path, fthisfile.path) EQ 0 THEN $
    message, 'PRECOMPILED ADD_SIGNAL OR ADD_NOISE FUNCTION'+ $
             ' NOT COMPATIBLE WITH WVT_PIXELLIST!' + $
             ' To avoid this, type ".compile wvt_pixellist" before starting.'

  WVT_BINNING, x, y, pixelSize, targetSN, $
    binnumber, xNode, yNode, weight, area=area, QUIET=quiet, $
    dens=dens, binvalue=binvalue, snbin=snbin, plotit=plotit, $
    resume=resume, neighborlist=neighborlist, max_area=max_area, $
    gersho=gersho, keepfixed=keepfixed, center=center

END

