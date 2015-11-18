; Interface for WVT_BINNING to produce an X-ray color image from 2 
; two-dimensional images. 
; Version 2.0, updated 12/05/2005
;
;######################################################################
;+
; NAME:
;     WVT_XRAYCOLOR
;
; AUTHORS:
;       Steven Diehl & Thomas S. Statler, Ohio University, U.S.
;       diehl@phy.ohiou.edu, statler@ohio.edu
;
; PURPOSE:
;       Adaptive spatial binning of 2 two-dimensional images to produce
;       a hardness ratio map (also called color map), while keeping a
;       uniform signal-to-noise per bin distribution.
;
; EXPLANATION:
;       For additional information about WVT binning,
;       please refer to Diehl, S. & Statler, T.S. (2005).
;       Further information on the original VORONOI_2D_BINNING algorithm can 
;       be found in Cappellari M., Copin Y., 2003, MNRAS, 342, 345.
;
; CALLING SEQUENCE:
;    WVT_XRAYCOLOR, signal, signal2, noise, noise2, targetSN, binnedimage, $
;      xnode, ynode, weight, [, snbin=snbin, mask=mask, ctsimage=ctsimage, $
;      binnumber=binnumber, binvalue=binvalue, center=center, plotit=plotit, $
;      resume=resume, save_all=save_all, max_area=max_area, $
;      keepfixed=keepfixed ]
;
; EXAMPLE
;       See wvt_xraycolor.example
;
; INPUTS (REQUIRED):
;        SIGNAL: Two-dimensional image (n_x,n_y), contains the signal per pixel
;               in the first bandpass. You can use exposure and background 
;               corrected images, as well as simple counts images.
;       SIGNAL2: Analogous to SIGNAL (n_x,n_y), but for bandpass 2.
;        NOISE: Two-dimensional image (n_x,n_y), containing the noise 
;               associated with each pixel (sqrt(variance))
;        NOISE2: Analogous to NOISE (n_x,n_y), but for bandpass 2.
;     TARGETSN: The desired signal-to-noise ratio in the final
;               2D-binned data. Choice depends if the emphasis is on spatial 
;               resolution or accuracy.
;            
; OUTPUTS:
;  BINNEDIMAGE: Two-dimensional image (n_x,n_y) containing the hardness 
;               ratios of each bin.
;        XNODE: Vector (size Nbins) of the X coordinates of bin generators, 
;               i.e the geometric centers of the bins.
;        YNODE: Vector (size Nbins) of Y coordinates of the bin generators.
;        SNBIN: Vector (size Nbins) with the final SN of each bin.
;    BINNUMBER: Two-dimensional image (n_x,n_y) containing the bin number 
;               assigned to each input pixel. The index goes from 1 to Nbins. 
;               Pixels that were excluded during the binning process are 
;               marked "0" (see also the keyword MASK).
;               This vector alone is enough for making *any* subsequent
;               computation on the binned data. Everything else is optional.
;         AREA: Vector (size Nbins) with the number of pixels for each bin.
;     BINVALUE: Vector (size Nbins) with the final signal of each bin
;
; KEYWORDS (OPTIONAL):
;       CENTER: [Input] Vector (size 2) contains x and y values of the center. 
;               If this keyword is NOT supplied, [0,0] will be assumed and the 
;               algorithm will start at the highest S/N pixel with the bin 
;               accretion step. If the center is given, bin accretion will 
;               start there. 
;     MAX_AREA: [Input] Scalar specifying a maximum bin size (in pixel). 
;               Useful in cases where there is essentially no signal in a 
;               certain region.
;               ATTENTION: In area where the bin size hits MAX_AREA, the 
;                 algorithm does NOT enforce a uniform S/N anymore. 
;                 Use with care. 
;         MASK: [Input] Two-dimensional image (n_x,n_y) specifies which pixels 
;               should be included in the WVT binning algorithm. Valid pixels 
;               are designated as "1", excluded pixels as "0" (int or byte). 
;     CTSIMAGE: [Input] Two-dimensional image (n_x,n_y) containing the counts 
;               per pixel of the *added* images IMAGE and IMAGE2. Necessary 
;               for very sparse X-ray data, where some pixels can contain no 
;               counts. In these empty pixels, you might "artificially" have 
;               a high S/N value, if both images are background subtracted.
;               In this case, the so-called signal would simply be the
;               hardness ratio of the two background values, instead of the 
;               desired hardness of the object. If the counts image is 
;               supplied, the binning algorithm doesn't allow bins without 
;               any counts in them (recommended). However, note that this is 
;               only a very rare case. Most hardness ratio maps will run fine 
;               without this.
;    KEEPFIXED: [Input] Vector (size 2 x n_fixed) contains x and y coordinates 
;               of bin generators that you want to keep fixed in position.
;               The binning algorithm will move all other bins around as usual.
;               Example use: Keep one bin fixed on the center of a galaxy.
;       PLOTIT: Set this keyword to one to produce a plot of the 2-D
;               bins and of the corresponding S/N at the end of the
;               computation. A value of two will produce a similar plot 
;               after the bin accretion step. Setting PLOTIT higher than 
;        QUIET: by default the program shows the progress while accreting
;               pixels and then while iterating the CVT. Set this keyword
;               to avoid printing progess results. 
;       RESUME: Set this keyword, if you want to start from an existing WVT. 
;               which is uniquely defined in the structure SAVE_ALL
;     SAVE_ALL: [Output/Input] Structure that saves all of the necessary 
;               information to restart WVT_XRAYCOLOR from any given point. If
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
;            WVT_IMAGE           -- main interface
;
; FUNCTIONS PROVIDED:
;       The following functions are contained in the main WVT_BINNING program.
;       Refer to the main code for further explanations.
;            ADD_SIGNAL          -- adds the signal of a bin (hardness ratios)
;            ADD_NOISE           -- adds the noise of a bin 
;      
; NOTE:
;       WVT_BINNING uses the function MATCH from the IDL astro library. We
;       included this function in the main program release for completeness.
;       Feel free to remove the file match.pro if you already have the IDL 
;       astro library (see idlastro.gsfc.nasa.gov/homepage.html) installed.
;
; MODIFICATION HISTORY:
;       V1.0: First version, built on the old SD_VORONOI_COLOR and WVT_IMAGE
;           (07/12/2005)
;       V1.1: Bugfixes for keepfixed and max_area keywords (09/08/05)
;       V1.2: Fixed bug when incompatible ADD_SIGNAL or ADD_NOISE functions
;           are precompiled. (08/09/2005)
;       V2.0: First published version (12/05/2005)
;     
;----------------------------------------------------------------------------
FUNCTION ADD_SIGNAL, index
COMMON DATAVALUES, P
  IF total(P.signal2[index]) EQ 0 or total(P.cts[index]) EQ 0 $
    THEN RETURN, 0d0 $
    ELSE RETURN, total(P.signal[index])/total(P.signal2[index])
END

;----------------------------------------------------------------------------
FUNCTION ADD_NOISE, index
 ; Adds the noise inside the bin, specified by the indeces of the bin members
  COMMON DATAVALUES, P
  H=total(P.signal2[index])
  S=total(P.signal[index])
  IF H EQ 0 THEN RETURN, 1d0 $
    ELSE RETURN, 1d0/H*sqrt(total(P.noise[index]^2) + $
                 (S/H)^2*total(P.noise2[index]^2))
END

;----------------------------------------------------------------------------
PRO WVT_XRAYCOLOR, signal, signal2, noise, noise2, targetSN, binnedimage, $
    xnode, ynode, weight, snbin=snbin, mask=mask, ctsimage=ctsimage, $
    binnumber=binnumber, binvalue=binvalue, center=center, plotit=plotit, $
    resume=resume, save_all=save_all, max_area=max_area, keepfixed=keepfixed
  COMMON DATAVALUES, P

  ; Check if the ADD_SIGNAL and ADD_NOISE functions originate from this file
  ; If not, stop the procedure before you do something stupid...
  faddsignal = routine_info('add_signal',/source,/functions)
  faddnoise  = routine_info('add_noise' ,/source,/functions)
  fthisfile = routine_info('wvt_xraycolor',/source)
  IF strcmp(faddsignal.path, fthisfile.path) EQ 0 $
    OR strcmp(faddnoise.path, fthisfile.path) EQ 0 THEN $
    message, 'PRECOMPILED ADD_SIGNAL OR ADD_NOISE FUNCTION'+ $
             ' NOT COMPATIBLE WITH WVT_XRAYCOLOR!' + $
             ' To avoid this, type ".compile wvt_xraycolor" before starting.'


  ; Turn both images into 1D pixel lists
  ; Check if you want to resume a previous session
  IF NOT(keyword_set(resume)) THEN BEGIN
    si=size(signal,/dimension)
    m=long(si(0))
    n=long(si(1))
    binnedimage=fltarr(m,n)
    IF NOT(keyword_set(mask)) THEN mask=signal-signal+1
    IF NOT(keyword_set(ctsimage)) THEN ctsimage=mask
    IF keyword_set(center) THEN center=round(center) ELSE center=0

    temp=where(mask NE 0, ngood)
    x=fltarr(ngood)
    y=fltarr(ngood)
    p={ signal:dblarr(ngood), $
        noise:dblarr(ngood), $
        signal2:dblarr(ngood), $
        noise2:dblarr(ngood), $
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
          p.signal2[ii]=signal2[i,j]
          p.noise2[ii]=noise2[i,j]
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
    p={ signal:save_all.signal,   $
        noise:save_all.noise,     $
        signal2:save_all.signal2, $
        noise2:save_all.noise2,   $
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

  ; Convert the keepfixed parameters into the proper xy notation
  nfixed=n_elements(keepfixed)/2
  IF nfixed GT 0 THEN BEGIN
    keepfixed_in=dblarr(2,nfixed)
    FOR i=0,nfixed-1 DO BEGIN
      keepfixed_in[*,i]=keepfixed[*,i]
    ENDFOR
  ENDIF ELSE keepfixed=0

  ; Start the main binning algorithm
  WVT_BINNING, x, y, pixelsize, targetSN, class, $
      xnode, ynode, weight, binvalue=binvalue, keepfixed=keepfixed_in, $
      plotit=plotit, quiet=quiet, snbin=snbin, resume=resume, $
      neighborlist=neighborlist, dens=dens, max_area=max_area, center=center


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
             signal2:p.signal2, $
             noise2:p.noise2, $
             xnode:xnode, $
             ynode:ynode, $
             weight:weight, $
             binvalue:binvalue, $
             pixelsize:pixelsize, $
             targetSN:targetSN, $
             snbin:snbin, $
             cts:p.cts, $
             center:center,$
             keepfixed:keepfixed }



END





