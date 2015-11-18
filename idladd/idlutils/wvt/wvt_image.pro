; Interface for WVT_BINNING to work with two-dimensional images. 
; Version 2.0, updated 12/05/2005
;
;######################################################################
;+
; NAME:
;     WVT_IMAGE
;
; AUTHORS:
;       Steven Diehl & Thomas S. Statler, Ohio University, US
;       diehl@phy.ohiou.edu, statler@ohio.edu
;
; PURPOSE:
;       Adaptive spatial binning of any type of 2-dimensional data with
;       the aim of obtaining a uniform signal-to-noise per bin distribution.
;       WVT_IMAGE provides an interface to directly use 2d images as opposed
;       to pixel lists which are compatible with WVT_BINNING.
;
; EXPLANATION:
;       For additional information about WVT binning,
;       please refer to Diehl, S. & Statler, T.S. (2006).
;       Further information on the original VORONOI_2D_BINNING algorithm can 
;       be found in Cappellari M., Copin Y., 2003, MNRAS, 342, 345.
;
; CALLING SEQUENCE:
;     WVT_IMAGE, signal, noise, targetSN, binnedimage, xnode, ynode, weight $
;         [, snbin=snbin, mask=mask, ctsimage=ctsimage, 
;            binnumber=binnumber, binvalue=binvalue, center=center, 
;            plotit=plotit, resume=resume, save_all=save_all, 
;            max_area=max_area, gersho=gersho, keepfixed=keepfixed, 
;            quiet=quiet ]
;
; EXAMPLE
;       See wvt_image.example
;
; INPUTS (REQUIRED):
;       SIGNAL: Two-dimensional image (n_x,n_y), containing the signal per 
;               pixel. If pixels are the actual pixels of the CCD in a galaxy
;               image, the signal will be simply the counts in each pixel.
;               The image can be background subtracted or exposure map 
;               corrected, as long as the NOISE image reflects this. 
;               If the `pixels' are actually the apertures of an
;               integral-field spectrograph, then the signal can be
;               defined as the average flux in the spectral range under
;               study, for each aperture.
;        NOISE: Two-dimensional image (n_x,n_y), containing the noise 
;               associated with each pixel (sqrt(variance))
;     TARGETSN: The desired signal-to-noise ratio in the final
;               2D-binned data. A TARGETSN between ~4-10 is standard for 
;               X-ray images, a temperature map would require higher 
;               TARGETSN (~30-100), depending on the spectral model used. 
;               For integral field spectroscopy, a TARGETSN of ~50 per pixel 
;               may be a reasonable value to extract stellar kinematics
;               information from galaxy spectra.
;            
; OUTPUTS:
;  BINNEDIMAGE: Two-dimensional image (n_x,n_y) containing the binned signal.
;        XNODE: Vector (size Nbins) of the X coordinates of the bin 
;               generators, i.e the geometric centers of the bins.
;        YNODE: Vector (size Nbins) of Y coordinates of the bin generators.
;        SNBIN: Vector (size Nbins) with the final SN of each bin.
;    BINNUMBER: Two-dimensional image (n_x,n_y) containing the bin number 
;               assigned to each input pixel. The index goes from 1 to 
;               Nbins. Pixels that were excluded during the binning process 
;               are marked "0" (see also the keyword MASK).
;               This vector alone is enough for making *any* subsequent
;               computation on the binned data. Everything else is optional.
;         AREA: Vector (size Nbins) with the number of pixels for each bin.
;     BINVALUE: Vector (size Nbins) with the final signal of each bin
;
; KEYWORDS (OPTIONAL):
;       CENTER: [Input] Vector (size 2) containing x and y values for the 
;               center. If this keyword is NOT supplied, [0,0] will be 
;               assumed and the algorithm will start at the highest S/N 
;               pixel with the bin accretion step. If the center is given, 
;               bin accretion will start there. 
;     MAX_AREA: [Input] Scalar specifying a maximum bin size (in pixel). 
;               Useful in cases where there is essentially no signal in a 
;               certain region. ATTENTION: In area where the bin size hits
;               MAX_AREA, the algorithm does NOT enforce a uniform S/N 
;               anymore. Use with care. 
;         MASK: [Input] Two-dimensional image (n_x,n_y) specifying which 
;               pixels should be included in the WVT binning algorithm. 
;               Valid pixels are designated as "1", excluded pixels as "0" 
;               (integer or byte). 
;     CTSIMAGE: [Input] Two-dimensional image (n_x,n_y) containing the counts 
;               per pixel. Necessary for very sparse X-ray data, where some 
;               pixels can contain no counts. Nevertheless, in certain cases,
;               you can "artificially" have a high S/N value for this pixel, 
;               e.g. when you subtract two background corrected components 
;               (example: remove the point source contribution in 
;               ellipticals). Supply the counts image in order to avoid that 
;               the binning algorithm produces bins without any counts.
;    KEEPFIXED: [Input] Vector (size 2 x n_fixed) containing x and y 
;               coordinates of bin generators that you want to keep fixed 
;               in their position. The binning algorithm will move all other 
;               bins around as usual. Example use: Keep one bin fixed on 
;               the center of a galaxy.
;       PLOTIT: Set this keyword to one to produce a plot of the 
;               two-dimensional bin distribution and of the corresponding 
;               S/N at the end of the computation. A PLOTIT value of 2 will 
;               produce a similar plot after the bin accretion step. Setting 
;               PLOTIT to 3 results in graphical output for each iteration.
;               One should keep in mind that plotting slows down the 
;               algorithm significantly.
;        QUIET: by default the program shows the progress while accreting
;               pixels and then while iterating the CVT. Set this keyword
;               to avoid printing progess results. 
;       GERSHO: Use Gersho's conjecture with normal Voronoi tesselations, 
;               instead of the WVT algorithm. Except for some changes in the 
;               bin accretion step, this procedures is identical to the one 
;               implemented in Cappellari & Copin's code VORONOI_2D_BINNING.
;               Be aware, that Gersho's conjecture is only valid for strictly
;               positive data, where the S/N adds in quadrature!
;       RESUME: Set this keyword, if you want to start from an existing WVT. 
;               which is uniquely defined in the structure SAVE_ALL
;     SAVE_ALL: [Output/Input] Structure that saves all of the necessary 
;               information to restart WVT_IMAGE from any given point. If the
;               keyword RESUME is set, the WVT iteration scheme is applied 
;               to the supplied WVT.
;
; EXTERNAL INTERACTIVE CONTROLS
;       PLOTIT: If you decide to plot the iteration steps, create 
;               file in the current directory with the name 'plotit'. A simple
;               way to do this is to issue the command 'touch plotit' in a 
;               shell. Remove the file if you want to stop plotting.
;    STOPMENOW: If you want to terminate the iteration at the next iteration,
;               create a file named 'stopmenow' in the current directory (e.g 
;               'touch stopmenow').
;
; PROCEDURES PROVIDED:
;       The following procedures are contained in the main WVT_BINNING program.
;       Refer to the main code for further explanations.
;            WVT_IMAGE                     -- main interface
;
; FUNCTIONS PROVIDED:
;       The following functions are contained in the main WVT_BINNING program.
;       Refer to the main code for further explanations.
;            ADD_SIGNAL                    -- adds the signal of a bin
;            ADD_NOISE                     -- adds the noise of a bin
;      
; NOTE:
;       WVT_BINNING uses the function MATCH from the IDL astro library. We
;       included this function in the main program release for completeness.
;       Feel free to remove the file match.pro if you already have the IDL 
;       astro library (see idlastro.gsfc.nasa.gov/homepage.html) installed.
;
; MODIFICATION HISTORY (ORIGINAL VORONOI_2D_BINNING):
;       V0.1: First interface (10/2004)
;       V0.1: Exported the functions ADDSIGNAL and ADD_NOISE to make 
;           WVT_BINNING more flexible (11/2004)
;       V1.0: Major changes to work with the new WVT_BINNING calling sequence 
;           (07/12/2005)
;       V1.1: Fixed bug for keepfixed: the variable is no longer 
;           overwritten and also saved in SAVE_ALL now. (09/06/2005) 
;       V1.2: Fixed bug when incompatible ADD_SIGNAL or ADD_NOISE functions
;           are precompiled. (09/08/2005)
;       V2.0: First published version (12/05/2005)
;
;
;----------------------------------------------------------------------------
FUNCTION ADD_SIGNAL, index
  ; Adds the signal inside the bin, specified by the indeces of the bin members
  COMMON DATAVALUES, P
  IF total(P.cts[index]) GT 0 THEN $
    RETURN, total(P.signal[index])/n_elements(index) $
    ELSE RETURN, 0d0
END

;----------------------------------------------------------------------------
FUNCTION ADD_NOISE, index 
 ; Adds the noise inside the bin, specified by the indeces of the bin members
  COMMON DATAVALUES, P
  RETURN, (sqrt(total(P.noise[index]^2))/n_elements(index))>1d-200 
END

;----------------------------------------------------------------------------
PRO WVT_IMAGE, signal, noise, targetSN, binnedimage, xnode, ynode, weight, $
    snbin=snbin, mask=mask, ctsimage=ctsimage, binnumber=binnumber, $
    binvalue=binvalue, center=center, plotit=plotit, resume=resume, $
    save_all=save_all, max_area=max_area, gersho=gersho, $
    keepfixed=keepfixed, quiet=quiet
  ; Interface for using WVT_BINNING with images instead of pixel lists
  ; Takes the images, converts them into pixel lists, stores all of their
  ; properties (signal, noise, cts, ...) in the global variable P (structure) 
  ; and passes everything on to WVT_BINNING. This was designed particularly 
  ; with X-ray images in mind. The structure P will hold all information 
  ; about the individual pixels (signal, noise, cts, ...)
  COMMON DATAVALUES, P

  ; Check if the ADD_SIGNAL and ADD_NOISE functions originate from this file
  ; If not, stop the procedure before you do something stupid...
  faddsignal = routine_info('add_signal',/source,/functions)
  faddnoise  = routine_info('add_noise' ,/source,/functions)
  fthisfile = routine_info('wvt_image',/source)
  IF strcmp(faddsignal.path, fthisfile.path) EQ 0 $
    OR strcmp(faddnoise.path, fthisfile.path) EQ 0 THEN $
    message, 'PRECOMPILED ADD_SIGNAL OR ADD_NOISE FUNCTION'+ $
             ' NOT COMPATIBLE WITH WVT_IMAGE!' + $
             ' To avoid this, type ".compile wvt_image" before starting.'

  ; Turn images into 1D pixel lists if you don't resume an old session
  IF NOT(keyword_set(resume)) THEN BEGIN
    si=size(signal,/dimension)
    m=long(si(0))
    n=long(si(1))
    binnedimage=fltarr(m,n)
    IF NOT(keyword_set(mask)) THEN mask=fix(signal-signal+1)
    IF NOT(keyword_set(ctsimage)) THEN ctsimage=mask
    IF NOT(keyword_set(center)) THEN center=0

    temp=where(mask NE 0, ngood)
    x=fltarr(ngood)
    y=fltarr(ngood)
    
    p={ signal:dblarr(ngood), $
        noise:dblarr(ngood), $
        cts:lonarr(ngood) } 
    dens=dblarr(ngood)

    ii=0L
    FOR j=0L,n-1L DO BEGIN
      FOR i=0L,m-1L DO BEGIN
        ; Check if the pixel should be included or not
        IF mask(i,j) NE 0 THEN BEGIN
          x[ii]=float(i)
          y[ii]=float(j)
          p.signal[ii]=signal[i,j]
          p.noise[ii]=noise[i,j]
          dens[ii]=p.signal[ii]/(p.noise[ii]>1d-200)
          p.cts[ii]=ctsimage[i,j]      
          ii=ii+1L
        ENDIF
      ENDFOR
    ENDFOR
    pixelsize=1.0
  ENDIF ELSE BEGIN
    ; In case you are resuming an old session, read all the information out of 
    ; the save_all structure
    x=save_all.x
    y=save_all.y
    p={ signal:save_all.signal, $
        noise:save_all.noise, $
        cts:save_all.cts }
    dens=save_all.dens
    class=save_all.class
    neighborlist=save_all.neighborlist
    pixelsize=save_all.pixelsize
    targetSN=save_all.targetSN 
    xnode=save_all.xnode
    ynode=save_all.ynode
    snbin=save_all.snbin
    center=save_all.center
    keepfixed=save_all.keepfixed
  ENDELSE

  nfixed=fix(n_elements(keepfixed)/2.)
  IF nfixed GT 0 THEN keepfixed_in=keepfixed ELSE keepfixed=0
  FOR i=0,nfixed-1 DO keepfixed_in[*,i]=round(keepfixed[*,i])  

  ; Start the main binning algorithm
  WVT_BINNING, x, y, pixelsize, targetSN, class, $
      xnode, ynode, weight, binvalue=binvalue, $
      plotit=plotit, quiet=quiet, snbin=snbin, resume=resume, $
      neighborlist=neighborlist, dens=dens, max_area=max_area, $
      gersho=gersho, keepfixed=keepfixed_in, center=center

  ; Reconvert the output of WVT_BINNING back to images
  wvt_list_to_image, binnedimage, binvalue[class], x, y, dim=si
  wvt_list_to_image, binnumber, class+1, x, y, dim=si

  ; Create the structure save_all, which can be used to resume the procedure 
  ; at the point it was stopped. The structure can also be used to start with 
  ; a given tesselation
  save_all={ x:x, $
             y:y, $
             class:class, $
             neighborlist:neighborlist, $
             dens:dens, $
             signal:p.signal, $
             noise:p.noise, $
             xnode:xnode, $
             ynode:ynode, $
             weight:weight, $
             binvalue:binvalue, $
             pixelsize:pixelsize, $
             targetSN:targetSN, $
             snbin:snbin, $
             cts:p.cts, $
             center:center, $
             max_area:max_area, $
             keepfixed:keepfixed }

END




