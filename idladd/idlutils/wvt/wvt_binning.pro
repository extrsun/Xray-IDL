; Adaptive binning with a modified Lloyd algorithm using 
; Weighted Voronoi Tesselations (WVT)
; Version 2.0 updated 12/05/05
;
;######################################################################
; copyright notice for original code VORNOI_2D_BINNING,
; on which WVT_BINNING is based 
;
; Copyright (C) 2001-2003, Michele Cappellari
; E-mail: cappellari@strw.leidenuniv.nl
;
; For details on the method see:
;   Cappellari M., Copin Y., 2003, MNRAS, 342, 345
;
; Updated versions of the software are available from my web page
; http://www.strw.leidenuniv.nl/~mcappell/idl
;
; If you have found this software useful for your
; research, we would appreciate an acknowledgment to use of
; `the Voronoi 2D-binning method by Cappellari & Copin (2003)'.
;
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes is granted. Permission to modify for
; personal or internal use is granted, provided this copyright
; and disclaimer are included unchanged at the beginning of
; the file. All other rights are reserved.
;
;######################################################################
;+
; NAME:
;     WVT_BINNING
;
; AUTHORS:
;       MODIFIED CODE (WVT_BINNING)      
;       Steven Diehl & Thomas S. Statler, Ohio University, US
;       diehl@helios.phy.ohiou.edu, tss@coma.phy.ohiou.edu
;
;       ORIGINAL CODE (VORONOI_2D_BINNING)
;       Michele Cappellari, Leiden Observatory, The Netherlands
;       cappellari@strw.leidenuniv.nl
;
; CREDIT:
;       In case you are using this method for you data analysis, please
;       refer to it as the "WVT adaptive binning algorithm by Diehl &
;       Statler (2006), based on the Voronoi 2D-binning method by 
;       Cappellari & Copin (2003)" 
;
; PURPOSE:
;       Adaptive spatial binning of any type of 2-dimensional data with
;       the aim of obtaining a uniform signal-to-noise per bin distribution.
;       The code uses weighted Voronoi tesselations (WVT) and is
;       designed in particular to work with X-ray intensity image, color images,
;       or integral-field spectroscopic data.
;       WVT_BINNING operates only on pixel lists. WVT_IMAGE provides an 
;       interface to directly use 2d images instead.
;
; EXPLANATION:
;       For additional information about WVT binning
;       please refer to Diehl, S. & Statler, T.S. (2006).
;       Further information on the original VORONOI_2D_BINNING algorithm can 
;       be found in Cappellari M., Copin Y., 2003, MNRAS, 342, 345.
;
; CALLING SEQUENCE:
;       WVT_BINNING, x, y, pixelSize, targetSN, binnumber, xNode, yNode, area 
;         [, /QUIET=quiet, dens=dens, binvalue=binvalue, snbin=snbin, $
;         plotit=plotit, resume=resume, neighborlist=neighborlist, 
;         max_area=max_area, gersho=gersho, keepfixed=keepfixed ]
;
; REQUIREMENTS:
;       There have to be two external, compiled functions ADD_SIGNAL and 
;       ADD_NOISE, which define how the signal and noise inside a bin are 
;       added. The wrappers WVT_IMAGE, WVT_XRAYCOLOR or WVT_PIXELLIST contain
;       these functions already. See WVT_GENERIC for an example on how
;       to build your own S/N calculation.
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
;               The image can be background subtracted or exposure map 
;               corrected,as long as the NOISE image reflects this. 
;               If the `pixels' are actually the apertures of an
;               integral-field spectrograph, then the signal can be
;               defined as the average flux in the spectral range under
;               study, for each aperture.
;        NOISE: Vector (same size as X) containing the corresponding
;               noise associated with each pixel.
;
; OUTPUTS:
;    BINNUMBER: Vector (same size as X) containing the bin number assigned
;               to each input pixel. The index goes from zero to Nbins-1.
;               This vector alone is enough for making *any* subsequent
;               computation on the binned data. Everything else is optional.
;        XNODE: Vector (size Nbins) of the X coordinates of the bin generators, 
;               i.e the geometric centers of the bins.
;        YNODE: Vector (size Nbins) of Y coordinates of the bin generators.
;        SNBIN: Vector (size Nbins) with the final SN of each bin.
;         AREA: Vector (size Nbins) with the number of pixels for each bin.
;     BINVALUE: Vector (size Nbins) with the final signal of each bin
;
; KEYWORDS (OPTIONAL):
;       PLOTIT: Set this keyword to one to produce a plot of the two-dimensional
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
;               which is uniquely defined by the vectors XNODE, YNODE, and SNBIN
;               The WVT iteration scheme will be applied to the supplied WVT, 
;               and the values overwritten with the final output.
;    KEEPFIXED: [Input] Vector (size 2 x n_fixed) containing x and y coordinates 
;               of bin generators that you want to keep fixed in their position.
;               The binning algorithm will move all other bins around as usual.
;               Example use: Keep one bin fixed on the center of a galaxy.
;
; EXTERNAL INTERACTIVE CONTROLS
;       PLOTIT: If you decide that you want to plot the iteration steps, create 
;               file in the current directory with the name 'plotit'. A simple
;               way to do this is to issue the command 'touch plotit' in a 
;               shell. Remove the file if you want to stop plotting.
;    STOPMENOW: If you want to terminate the iteration at the next possible time,
;               create a file named 'stopmenow' in the current directory (e.g 
;               'touch stopmenow').
;
; PROCEDURES PROVIDED:
;       The following procedures are contained in the main WVT_BINNING program.
;       Refer to the main code for further explanations.
;            WVT_FIND_BINNEIGHBORS         -- find neighboring bins
;            WVT_CHECK_ALL_BINS            -- check for zero sized bins
;            WVT_CHECK_BINISLANDS          -- check if one bin encloses another
;            WVT_RENUMBER_BINNUMBER        -- kick out zero size bins, renumber
;            WVT_UNWEIGHTED_CENTROID       -- calculate geometric center
;            WVT_ADDTO_UNWEIGHTED_CENTROID -- update center when pixels added
;            WVT_WEIGHTED_CENTROID         -- calculated weighted centroid
;            WVT_BIN_ACCRETION             -- modified bin accretion algorithm
;            WVT_REASSIGN_BAD_BINS         -- unallocated pixels to closest bin
;            WVT_EQUAL_MASS                -- main iteration scheme
;            WVT_DISPLAY_PIXELS            -- display pixel list
;            WVT_MAKENEIGHBORLIST          -- create list of pixel neighbors
;            WVT_FINDNEIGHBORS             -- find all neighbors of a bin
;            WVT_BINNING                   -- MAIN PROGRAM
;
; FUNCTIONS PROVIDED:
;       The following functions are contained in the main WVT_BINNING program.
;       Refer to the main code for further explanations.
;            WVT_ASSIGN_TO_BIN             -- create the WVT
;            WVT_ASSIGN_NEIGHBORS          -- find bin numbers of neighbors
;            WVT_RECURSIVE_NEIGHBORS       -- recursive list of nearby bins
;            WVT_BIN_ROUNDNESS             -- computes roundness of a bin
;
; NOTE:
;       This program uses the function MATCH from the IDL astro library. We
;       included this function in the main program release for completeness.
;       Feel free to remove the file match.pro if you already have the IDL 
;       astro library (see idlastro.gsfc.nasa.gov/homepage.html) installed.
;
; MODIFICATION HISTORY (ORIGINAL VORONOI_2D_BINNING):
;       V1.0: First implementation. Michele Cappellari, Leiden, June 2001
;       V2.0: Major revisions. Stable version. MC, Leiden, 11 September 2001
;       V2.1: First released version. Written documentation.
;           MC, Vicenza, 13 February 2003
;       V2.2: Added computation of bin quantities in output. Deleted some
;           safety checks for zero size bins in CVT. Minor polishing of code.
;           MC, Leiden, 11 March 2003
;       V2.3: Unified the three tests to stop the accretion of one bin.
;           This can improve some bins at the border. MC, Leiden, 9 April 2003
;       V2.31: Do *not* assume the first bin is made of one single pixel.
;           Added computation of S/N scatter and plotting of 1-pixel bins.
;           MC, Leiden, 13 April 2003
;       V2.4: Added basic error checking of input S/N. Reintroduced the
;           treatment for zero-size bins in CVT, which was deleted in V2.2.
;           Thanks to Robert Sharp and Kambiz Fathi for reporting problems.
;           MC, Leiden, 10 December 2003.
;       V2.41: Added /QUIET keyword and verbose output during the computation.
;           After suggestion by Richard McDermid. MC, Leiden, 14 December 2003
;
; MODIFICATION HISTORY (WVT_BINNING):
;       V0.0: Made loop indices long integers to work with big images.
;           Changed binnumber to a long integer array. (10/2004)
;       V0.1: Added code to relax the requirement that S/N increase with every
;           added pixel in bin accretion. This includes a test for good enough
;           S/N at head of pixel addition loop. (These options currently
;           commented out.) (10/2004)
;       V0.2 User specifies pixelsize when calling voronoi_2d_binning, to avoid
;           lengthy search through all pairs of pixels. (10/2004)
;       V0.3 Added procedures makeneighborlist and findneighbors. bin_accretion
;           calls makeneighborlist and findneighbors in order
;           to expedite search for candidate pixels (10/2004)
;       V0.4 Modified cvt_equal_mass to use nearest-neighbor list of voronoi
;           nodes. (10/2004)
;       V1.0: Complete haulover. Removal of Gersho's conjecture, use of S/N 
;           density instead, together with weighted Voronoi tesselations, 
;           really the first WVT binning algorithm (11/2004)
;       V1.1: Added stability checks, redistribute "bin islands" (12/2004) 
;       V1.2: Added possibility to resume an old session (11/2004)
;       V1.3: Added flexibility for a general signal and noise calculation
;           Exported the functions add_signal and add_noise to a separate 
;           file  (11/2004)
;       V1.4: Introduced the optional keyword max_area to limit the absolute 
;           size of bins (01/05/2005)
;       V1.5: Added possibility to keep a number of supplied centers fixed
;           (06/14/2005)       
;       V1.6: Code polishing, renaming of functions to wvt_* (07/14/2005)
;       V1.7: Replaced "class" with "binnumber", for consistency with
;           interfaces (07/22/2005)
;       V1.8: Replaced the external function "histo" by the IDL equivalent
;           histogram to resolve portability issues. Minor bug fix for 
;           choosing the center in bin accretion (07/29/2005)
;     V1.9.0: Checked new convergence criteria (09/06/2005)
;     V1.9.1: Added explicit CENTER keyword to avoid problems with bin
;           accretion (09/08/2005)
;     V1.9.2: Reduced possibility of limit cycle by 'slowing down' the 
;           movement of nodes for only half of all nodes (randomly chosen) 
;     V1.9.3: Increased speed for WVT_FINDNEIGHBORS in the bin accretion 
;           step. Necessary for cases with large individual bins. (09/15/2005)
;       V2.0: First published version (12/05/2005)
;
;
;
;----------------------------------------------------------------------------
FUNCTION WVT_ASSIGN_TO_BIN, x, y, xnode, ynode, SNnode
  ; Assigns each pixel to the S/N weighted closest pixel 
  ; i.e. this constructs the weighted voronoi tesselation
  tmp = min(((x-xnode)^2+(y-ynode)^2)*SNnode, index)
  RETURN, index[0]
END

;----------------------------------------------------------------------------
FUNCTION WVT_ASSIGN_NEIGHBORS, neighborlist, binnumber
  ; Finds the binnumber of all neighbors of a bin and returns a list of adjacent
  ; bins. You have to take into account that there might be no neighbors (-1)
  tempbinnumber=[-1,binnumber]  
  RETURN, tempbinnumber[neighborlist+1]
END

;----------------------------------------------------------------------------
PRO WVT_FIND_BINNEIGHBORS, binneighbors, neighborlist, neighborbinnumber, binnumber, $
    nbins,area=area, reverse_indices=r
  ; Produces the final list of *unique* bin neighbors.
  ; binneighbors will have the same format as the the REVERSE_INDICES
  ; output of the HISTOGRAM function

  IF NOT(keyword_set(area)) THEN $
  area = HISTOGRAM(binnumber, REVERSE_INDICES=r ,min=0, max=nbins-1)
  npix=n_elements(binnumber)
  binneighbors=lonarr(nbins+1L+nbins*50) ; Just make the array way too big!
  binneighbors[0]=nbins+1 

  FOR i=0L,nbins-1 DO BEGIN
    w = r[r[i]:r[i+1]-1]
    tmpbinnumber=neighborbinnumber[w,*]
    tmplist=tmpbinnumber(uniq(tmpbinnumber,sort(tmpbinnumber))) 
    tmplist=tmplist[where(tmplist NE -1)]
    binneighbors[i+1]=binneighbors[i]+n_elements(tmplist)
    binneighbors[binneighbors[i]:binneighbors[i+1]-1]=tmplist
  ENDFOR
  binneighbors=binneighbors[0:binneighbors[nbins]-1]
END

;----------------------------------------------------------------------------
FUNCTION WVT_RECURSIVE_NEIGHBORS, binneighbors, current, nlevels
  ; Function to increase the speed of constructing the WVT in the next 
  ; iteration. Takes the list of bin neighbors for each single bin and 
  ; recursively finds the next closest neighbors by stepping down "nlevels" 
  ; in the hierarchy. "Current" is the list of bins you would find neighbors
  ; for.
  tmplist=current
  FOR i=0,n_elements(current)-1 DO BEGIN
    k=current[i]
    tmplist=[tmplist,k, binneighbors[binneighbors[k]:binneighbors[k+1]-1L]]
  ENDFOR
  tmplist=tmplist(uniq(tmplist, sort(tmplist)))

  IF nlevels EQ 0 THEN RETURN, tmplist $
   ELSE RETURN, wvt_recursive_neighbors( binneighbors, tmplist, nlevels-1)
END

;----------------------------------------------------------------------------
PRO WVT_CHECK_ALL_BINS, binnumber, x, y, xnode, ynode, nbad=nbad, area=area, $
    reverse_indices=r
  ; Sanity checks: make sure you don't have coinciding bin centers (possible
  ; in WVTs, IF one bin is completely enclosed by another bin of larger scale
  ; length. In cases with holes in the data, it is generally possible to have
  ; the bin center outside the valid data and the bin containing zero pixels.
  nbins=n_elements(xnode)
  IF NOT(keyword_set(area)) THEN $ 
    area = HISTOGRAM(binnumber, REVERSE_INDICES=r ,min=0, max=nbins-1)
  w = where(area EQ 0, nbad) ; Check for zero-size Voronoi bins

  WHILE nbad GT 0 DO BEGIN
    w2 = where(area GE 2, m2) ; You have to make sure that you don't assign the
                              ; the center of another 1 pixel bin
    FOR n=0L, nbad-1 DO BEGIN
      tmp=sort((x-xnode[w[n]])^2+(y-ynode[w[n]])^2)
      loop=0L
      WHILE 1 do BEGIN
        IF total(where(w2 EQ binnumber[tmp[loop]])) NE -1 THEN BEGIN
          index=tmp[loop]
          break
        ENDIF
        loop=loop+1L
      ENDWHILE
    ; Set the centroid to the center of the pixel
    binnumber[index]=w[n]
    xnode[binnumber[index]]=x[index]
    ynode[binnumber[index]]=y[index]
    print, 'Bin with zero pixels found: ', w[n]
  ENDFOR
  IF nbad NE 0 THEN BEGIN
    area = HISTOGRAM(binnumber, REVERSE_INDICES=r ,min=0, max=nbins-1)
  ENDIF
  w = where(area EQ 0, nbad) ; Check for zero-size Voronoi bins
ENDWHILE

END

;----------------------------------------------------------------------------
PRO WVT_CHECK_BINISLANDS, x, y, binnumber, neighborbinnumber, xnode, ynode, $
    area=area, reverse_indices=r
  ; Checks that every bin is not enclosed by another bin, i.e. has more than 
  ; one neighboring bin. Note that this check should be avoided if you have 
  ; lots of gaps in your data since then you could have an isolated bin due 
  ; to the fact that the gaps are "cornering" the bin.

  IF NOT(keyword_set(area)) THEN $
    area = HISTOGRAM(binnumber, REVERSE_INDICES=r ,min=0, max=nbins-1)
  nbins=n_elements(xnode)
  recalc_area=0

  FOR k=0L,nbins-1 DO BEGIN
    index = r[r[k]:r[k+1]-1] ; Find subscripts of pixels in bin k.
    binnumbertemp=neighborbinnumber[index,*]
    neighbornodes=binnumbertemp[uniq(binnumbertemp, sort(binnumbertemp))]
    w=where(neighbornodes NE k AND neighbornodes NE -1, nneighbors)
    IF nneighbors NE 0 THEN neighbornodes=neighbornodes[w]
    IF nneighbors EQ 1 THEN BEGIN
      k2=neighbornodes[0]
      print, 'Bin ', k,' is enclosed by bin ', k2, '. Redistributing bins.'
      index=[index,r[r[k2]:r[k2+1]-1]]   ; add the other bin pixels
      temp=[k,k2]  ; only search among 2 nodes
      FOR i=0L,n_elements(index)-1L DO $
          binnumber[index[i]]=$
            temp[wvt_assign_to_bin(x[index[i]], y[index[i]], $
            xnode[temp], ynode[temp], 1.)]
      recalc_area=1
    ENDIF
  ENDFOR

  IF recalc_area THEN $
    area = HISTOGRAM(binnumber, REVERSE_INDICES=r ,min=0, max=nbins-1)
END

;----------------------------------------------------------------------------
PRO WVT_CALC_BIN_SN, binnumber, SNbin, binvalue=binvalue, binarea=binarea, $
    area=area, reverse_indices=r
  ; Calculates the S/N values for all bins, as well as the bin values
  ; New version, should be considerably faster for large nbin
  ; Note: binnumber has to start at 0 and end at nbins-1!
  COMMON DATAVALUES, P

  nbins=max(binnumber)+1
  SNbin=dblarr(nbins)
  binvalue=dblarr(nbins)
  binarea=lonarr(nbins)

  IF NOT(keyword_set(area)) THEN $
    area = HISTOGRAM(binnumber, REVERSE_INDICES=r,min=0, max=nbins-1)
  w = where(area GT 0, m) ; Check for zero-size Voronoi bins

  FOR j=0L,m-1 DO BEGIN
    k = w[j]                 ; Only loop over nonzero bins
    index = r[r[k]:r[k+1]-1] ; Find subscripts of pixels in bin k.
    npix=n_elements(index)
    binarea[k]=npix
    binvalue[k]=add_signal(index)
    SNbin[k]=binvalue[k]/add_noise(index)
    ;binvalue[k]=binvalue[k]/double(npix)
  ENDFOR

END

;----------------------------------------------------------------------------
PRO WVT_RENUMBER_BINNUMBER, binnumber, minbinnumber=minbinnumber, area=area
  ; Kicks out zero pixel bins and renumbers the rest continuously, starting at
  ; the value minbinnumber

  nbins=max(binnumber)
  IF NOT(keyword_set(minbinnumber)) THEN minbinnumber=0L
  IF NOT(keyword_set(area)) THEN $
    area = HISTOGRAM(binnumber, REVERSE_INDICES=r,min=minbinnumber, max=nbins)
  w = where(area GT 0, nbinnumber) ; Check for zero-size Voronoi bins
  
  FOR n=0L,nbinnumber-1L DO BEGIN
    k = w[n]                 ; Only loop over nonzero bins
    binnumber(r[r[k]:r[k+1]-1])=n+minbinnumber
  ENDFOR
END

;----------------------------------------------------------------------------
PRO WVT_UNWEIGHTED_CENTROID, x, y, xBar, yBar
  ; Computes the geometric center of one bin

  Mass=double(n_elements(x))
  xBar=total(x)/mass
  yBar=total(y)/mass
END

;----------------------------------------------------------------------------
PRO WVT_ADDTO_UNWEIGHTED_CENTROID, x, y, xBar, yBar, xbold, ybold, massold
  ; For speed, this procedure computes the geometric center of a bin by adding 
  ; a new list of xy values for an existing bin. Useful in bin accretion step. 

  Mass=double(n_elements(x)+massold)
  xBar=(total(x)+massold*xbold)/mass
  yBar=(total(y)+massold*ybold)/mass
END

;----------------------------------------------------------------------------
PRO WVT_ADDTO_WEIGHTED_CENTROID, x, y, xymass, xBar, yBar, $
    xbold, ybold, massold
  ; For speed, this procedure computes the geometric center of a bin by adding 
  ; a new list of xy values for an existing bin. Useful in bin accretion step. 

  Mass=double(total(xymass>1d-200)+massold)
  IF mass GT 0 THEN BEGIN 
    xBar=(total((xymass>1d-200)*x)+massold*xbold)/mass
    yBar=(total((xymass>1d-200)*y)+massold*ybold)/mass
  ENDIF 
  massold=mass
END

;----------------------------------------------------------------------------
PRO WVT_WEIGHTED_CENTROID, x, y, density, xBar, yBar
  ; Computes the weighted centroid of one bin, only implemented, if 
  ; Gersho's conjecture is used. CAREFUL! Gersho's conjecture is invalid for 
  ; negative data. If the total signal is negative, the weighted centroid   
  ; will be set to the geometric center instead.

  mass = total(density)
  ; Check if the bin has negative signal
  minmass=1d-30
  IF mass LE minmass THEN BEGIN
    ; For negative overall signal take this approximation:
    IF total(density>0.) NE 0 THEN BEGIN
      ; Only use positive signal and get the weighted center from there:
      mass=total(density>0)
      xBar=total(x*density>0)/mass
      yBar=total(y*density>0)/mass
    ENDIF ELSE BEGIN
      ; Take the geometrical mean of the bin IF there is NO positive pixel 
      Mass=double(n_elements(x))
      xBar=total(x)/mass
      yBar=total(y)/mass
    ENDELSE
  ENDIF ELSE BEGIN
    xBar = total(x*density)/mass
    yBar = total(y*density)/mass
  ENDELSE
END

;----------------------------------------------------------------------
FUNCTION WVT_BIN_ROUNDNESS, x, y, pixelSize
  ; Returns the "roundness" of a bin, as defined by equation (5) of 
  ; Cappellari & Copin (2003)

  n = n_elements(x)
  equivalentRadius = sqrt(n/!Pi)*pixelSize
  xBar = total(x)/n ; unweighted centroid here!
  yBar = total(y)/n
  maxDistance = sqrt(max((x-xBar)^2 + (y-yBar)^2))
  roundness = maxDistance/equivalentRadius - 1.0

  RETURN, roundness
END

;----------------------------------------------------------------------
PRO WVT_BIN_ACCRETION, x, y, dens, targetSN, binnumber, pixelSize, $
  QUIET=quiet, SNBin=SNBin, neighborlist=neighborlist, max_area=max_area, $ 
  keepfixed=keepfixed, center=center
  ; Implementation of the bin accretion algorithm. Optimized for speed when 
  ; working with large images. Uses a list of neighboring pixels to search 
  ; only pixels adjacent to the already binned pixels when trying to add 
  ; the next pixel, instead of searching the whole list.
  ; Based on steps (i)-(v) in section 5.1 of Cappellari & Copin (2003), 
  ; but severely modified
  COMMON DATAVALUES, P

  ; Create neighbor list for each pixel
  print,"...making neighbor list..."
  wvt_makeneighborlist,x,y,pixelsize,neighborlist

  n = n_elements(x)
  binnumber = lonarr(n) ; will contain the bin number of each given pixel
  good = bytarr(n)  ; will contain 1 IF the bin has been accepted as good
  SNbin= dblarr(n)  ; will contain the S/N of the corresponding bin

  IF n_elements(center) EQ 2 THEN BEGIN
    ; Start at the specified center, if this keyword is set 
    temp=min((x-center[0])^2+(y-center[1])^2,currentbin) 
    print, 'Bin accretion started at the specified center:'
  ENDIF ELSE BEGIN
    ; For X-ray images, you might have a special cases, where you can have 
    ; an artificially high S/N for a pixel that actually has no signal in 
    ; it, namely when one knows the background very accurately. Thus, start
    ; with the highest S/N pixel that has at least one count in it.
    IF TAG_EXIST(p,'cts') THEN SN = max(dens*(p.cts<1), currentBin) ELSE $
    SN = max(dens, currentBin)
    print, 'Bin accretion started at highest S/N value:'
  ENDELSE
 
  currentbin=currentbin[0]
  SN = dens[currentBin] 

  startingx=x[currentbin]
  startingy=y[currentbin]
  print, 'First bin starts at ', startingx, startingy

  ; The first bin will be assigned BINNUMBER = 1
  ; With N pixels there will be at most N bins
  totpix=double(n_elements(x))
  SNold=add_signal(dindgen(totpix))/add_noise(dindgen(totpix))
  goodold=0
  areaold=totpix
  area=1L
  mass=1d-200
  totarea=0L
  xBar_binned=startingx
  yBar_binned=startingy

  nfixed=size(keepfixed, /dimensions)
  IF nfixed[0] NE 0 THEN $
    IF n_elements(nfixed) EQ 1 AND nfixed[0] EQ 2 THEN $
      nfixed=1 ELSE nfixed=nfixed[1]
  IF nfixed GT 0 THEN $
    temp=min((x-keepfixed[0,0])^2+(y-keepfixed[1,0])^2,currentbin) 

  ; Check if the max_area is set. If not, leave the area unrestricted.
  IF NOT(keyword_set(max_area)) THEN max_area=totpix

  totdens=0d0
  totgoodbins=0L

  FOR ind=1L,n do BEGIN
    totgoodbins=totgoodbins+goodold
    IF not keyword_set(quiet) AND goodold EQ 1 THEN $
       print, 'bin:  ', string(totgoodbins,format='(I7)'), $
        ' | S/N:', string(SNold, format='(F8.2)'),' | n_pixels: ', $
        string(areaold,format='(I7)'), ' | ', $
        string(100*(1-n_elements(unBinned)/totpix), format='(F6.2)'), ' % done'

    SNold=0d0
    areaold=0L

    binnumber[currentBin] = ind ; Here currentBin is still made of one pixel
    xbar = x[currentBin]
    ybar = y[currentBin]    ; Centroid of one pixels

    WHILE 1 DO BEGIN

      ; If nfixed is set, then leave the first nfixed pixels what they are.
      IF ind LE nfixed THEN good[currentbin]=1

      ; Test if the bin is good enough already
      ; Better way to decrease the scatter around the target S/N later on:
      ; Use the average bin members' S/N to estimate the S/N when another 
      ; pixel is added to see if adding another pixels would actually 
      ; increase the scatter around the target S/N due to "overshooting". 
      ; Also stop accreting if the bin size reached "max_area"
      modtargetSN=(targetSN-SN*(sqrt(1d0+1d0/area)-1d0)/2d0)
      IF (SN ge modtargetSN OR area GE max_area) THEN BEGIN
        good[currentBin]=1
        SNOld = SN
        SNbin[currentBin]=SN 
        BREAK
      ENDIF

      ; Find nearest neighbors of pixels in the current bin, i.e. 
      ; pixels contiguous with the bin, that have a chance of being accreted
      ; For speed, remember the neighbors from the last accretion step and 
      ; simply add the new ones.
      wvt_findneighbors,currentBin,neighborlist,neighbors,exneighbors,newmember

      ; Accretable neighbors are those that aren't already binned
      IF neighbors[0] NE -1 THEN wh=where(binnumber[neighbors] eq 0, nwh) $
        ELSE nwh=0
      ; Stop if there aren't any accretable neighbors
      IF (nwh eq 0) THEN BEGIN
        IF (SN gt 0.8*targetSN) THEN BEGIN
          good[currentBin] = 1
          SNbin[currentBin]=SN 
        ENDIF
        BREAK
      ENDIF
      ; Otherwise keep only the accretable ones
      neighbors=neighbors[wh]

      ; Search only the neighbors to get the next pixel
      minDist = min((x[neighbors]-xBar)^2 + (y[neighbors]-yBar)^2, k)

      ; Remember the old verified neighbors and the new members of the bin
      ;exneighbors=neighbors
      newmember=neighbors[k]
      nextBin = [currentBin,neighbors[k]]
      roundness = wvt_bin_roundness(x[nextBin], y[nextBin], pixelSize)

      ; Compute the S/N one would obtain by adding
      ; the candidate pixel to the current bin
      SNOld = SN
      SN = add_signal(nextBin)/add_noise(nextBin)
    

      ; Test whether the candidate pixel is connected to the
      ; current bin, whether the possible new bin is round enough
      ; Relaxed constraint on roundness to accept more bins
      IF (roundness gt 1.0 ) THEN BEGIN
        IF (SN gt 0.8*targetSN) THEN BEGIN
          good[currentBin] = 1
          SNbin[currentBin]=SN 
        ENDIF
        BREAK                
      ENDIF

      ; If all the above tests are negative then accept the candidate pixel,
      ; add it to the current bin, and continue accreting pixels
      binnumber[neighbors[k]] = ind
      currentBin = nextBin

      ; Update the centroid of the current bin
      IF ind GT nfixed THEN BEGIN 
;        wvt_addto_unweighted_centroid, x[newmember], y[newmember], $
;          xBarnew, yBarnew, xbar, ybar, area 
        wvt_addto_weighted_centroid, x[newmember], y[newmember], $
          dens[newmember]^2, xBarnew, yBarnew, xbar, ybar, mass
      ENDIF ELSE BEGIN ; keep the fixed centers unaltered!
        xbarnew=keepfixed[0,ind-1]
        ybarnew=keepfixed[1,ind-1]
      ENDELSE
      xbar=xbarnew
      ybar=ybarnew
      ; Update the area of the bin
      area=area+1L
  
    ENDWHILE
  
    goodold=good[currentbin[0]]
    areaold=area 
    area=1L 
    mass=0d0

    unBinned = where(binnumber eq 0, COMPLEMENT=binned, m)
    IF (m eq 0) THEN BREAK ; Stop if all pixels are binned
    totarea=totarea+areaold

    ; Find the closest unbinned pixel to the centroid of all
    ; the binned pixels, and start a new bin from that pixel.
    IF ind GE nfixed THEN $
      minDist = min((x[unBinned]-xBar_binned)^2 + $
        (y[unBinned]-yBar_binned)^2,k)$
    ELSE $; keep the fixed centers unaltered!
      minDist = min((x[unBinned]-keepfixed[0,ind])^2 + $
        (y[unBinned]-keepfixed[1,ind])^2, k)

    currentBin = unBinned[k[0]]    ; The bin is initially made of one pixel
    SN = add_signal(currentBin)/add_noise(currentBin) 

  ENDFOR

  ; Set all bins to zero that did not reach the target S/N
  binnumber = binnumber * good 

END

;----------------------------------------------------------------------------
PRO WVT_REASSIGN_BAD_BINS, x, y, dens, targetSN, binnumber, xnode, ynode, $
    SNBin=SNBin
  ; Find pixels that the bin accretion step wasn't able to assign to a bin and
  ; reassign them to the next closest bin
  ; Implements steps (vi)-(vii) in section 5.1 of Cappellari & Copin (2003)
  COMMON DATAVALUES, P

  ; Find the centroid of all succesful bins.
  ; BINNUMBER = 0 are unbinned pixels which are excluded.
  wvt_renumber_binnumber, binnumber, minbinnumber=1L

  area = histogram(binnumber, REVERSE_INDICES=r, MIN=1)
  good = where(area gt 0, nnodes) ; Obtain the index of the good bins
  xnode = fltarr(nnodes)
  ynode = xnode
  FOR j=0L,nnodes-1 DO BEGIN
    k = good[j]
    index = r[r[k]:r[k+1]-1] ; Find subscripts of pixels in bin k.
    ;wvt_weighted_centroid, x[index], y[index], p.signal[index], xBar, yBar
    wvt_unweighted_centroid, x[index], y[index], xBar, yBar
    xnode[j] = xBar
    ynode[j] = yBar
  ENDFOR

  ; Reassign pixels to the closest centroid of a good bin
  bad = where(binnumber eq 0, m)
  FOR j=0L,m-1 DO $
    binnumber[bad[j]] = $
      good[wvt_assign_to_bin(x[bad[j]],y[bad[j]],xnode, ynode,1d0)]+1

  ; Recompute all centroids of the reassigned bins.
  ; These will be used as starting point for the WVT.
  area = HISTOGRAM(binnumber, REVERSE_INDICES=r)
  FOR j=0L,nnodes-1 DO BEGIN
    k = good[j] 
    index = r[r[k]:r[k+1]-1] ; Find subscripts of pixels in bin k.
    wvt_unweighted_centroid, x[index], y[index], xBar, yBar
    xnode[j] = xBar
    ynode[j] = yBar
  ENDFOR

END

;----------------------------------------------------------------------------
PRO WVT_EQUAL_MASS, x, y, dens, binnumber, xnode, ynode, iter, QUIET=quiet, $
  plotit=plotit, targetsn=targetsn, neighborlist=neighborlist, weight=weight, $
  max_area=max_area, gersho=gersho, max_iter=max_iter, keepfixed=keepfixed
  ; Iteration with the new modified Lloyd algorithm that takes advantage of the
  ; know average S/N per pixel to generate a WVT with equal S/N per bin. 
  ; Procedure described in Diehl & Statler (2005)
  COMMON DATAVALUES, P

  npixels = n_elements(x)
  IF NOT(keyword_set(max_area)) THEN max_area=npixels
  IF NOT(keyword_set(max_iter)) THEN max_iter=1000
  IF NOT(keyword_set(targetsn)) THEN targetsn=5d0
  seed=-1
  ; In case of negative data values (e.g bg subtraction), the bin properties 
  ; can get negative. In this case it is advisable to restrict the size of the 
  ; bins by max_area
  SNmin_area=targetSN/max_area

  ; For speed
  IF keyword_set(gersho) THEN dens2=dens^2
  ; For speed: only search neighboring nodes next time you construct the WVT
  nclosenodes=12 < n_elements(xnode)
  closenodes=lonarr(npixels,nclosenodes)

  iter = 1
  diff=1d5
  nbins=n_elements(xnode)
  ; No more "bad" bins with binnumber=0, i.e. start counting at 0!
  wvt_renumber_binnumber, binnumber, minbinnumber=0L
  wvt_calc_bin_sn, binnumber, SNbin
  rnd = randomu(seed,max(binnumber)) ; Randomize bin colors

  area=0
  neighborsexist=0
  nfixed=size(keepfixed, /dimensions)
  IF nfixed[0] NE 0 THEN $
    IF n_elements(nfixed) EQ 1 AND nfixed[0] EQ 2 THEN $
      nfixed=1 ELSE nfixed=nfixed[1]

  ; Start the iteration!
  pixelswitch=double(npixels)
  navg=20
  ps_array=dblarr(navg)
  seed=-1
  rnd = randomu(seed,max(binnumber)) ; Randomize bin colors
  REPEAT BEGIN

    ; To improve convergence and remove limit cyles
    IF pixelswitch/double(npixels) LT .01 AND iter GT 20 THEN BEGIN
      temp=randomu(seed, n_elements(xnode))
      wh=where(temp GT .5, nwh)
      IF nwh NE 0 THEN xnode[wh]=(xnode[wh]+xnodeold[wh])/2.
      IF nwh NE 0 THEN ynode[wh]=(ynode[wh]+ynodeold[wh])/2.
    ENDIF 

    xnodeOld = xnode
    ynodeOld = ynode

    neighborbinnumber=wvt_assign_neighbors(neighborlist,binnumber)
    wvt_calc_bin_sn, binnumber, SNbin, binarea=binarea, $
        binvalue=binvalue, area=area, reverse_indices=r
    IF NOT(keyword_set(gersho)) THEN BEGIN
      ; For convergence: try to avoid 1 to 1 pixel fluctuations
      IF pixelswitch/double(npixels) GE .01 OR iter LE 20 THEN $
        SNbinbar=SNbin/double(binarea) $
      ELSE BEGIN
        temp=randomu(seed, n_elements(xnode))
        wh=where(temp GT .5, nwh)
        IF nwh NE 0 THEN $
          SNbinbar[wh]=(SNbinold[wh]+SNbin[wh]/double(binarea[wh]))/2.
      ENDELSE

      SNbinold=SNbinbar
      SNbinbar=SNbinbar>SNmin_area
    ENDIF ELSE BEGIN
      ; If you use Gersho's conjecture, the scale lengths are all equal, 
      ; i.e. the WVT reduces to a VT
      SNbinbar=1.
    ENDELSE

    wvt_find_binneighbors, binneighbors, neighborlist, neighborbinnumber, $
      binnumber, nbins, area=area, reverse_indices=r
    FOR k=0L,nbins-1 DO BEGIN 
      tmp=wvt_recursive_neighbors(binneighbors, k, 2)
      FOR j=r[k],r[k+1]-1L DO BEGIN
        binnumber[r[j]]=tmp[wvt_assign_to_bin(x[r[j]], y[r[j]], xnode[tmp], $
          ynode[tmp], SNbinbar[tmp])]
      ENDFOR
    ENDFOR

    ;Now make sure that still each bin has at least one pixel
    wvt_check_all_bins, binnumber, x, y, xnode, ynode

    ; Graphical output, if desired. If you want to check the current state of
    ; the program is, type "touch plotit" on the unix command line in the 
    ; current directory. Simply remove the file when you have seen enough...
  IF plotit GE 3 OR file_test('plotit') THEN BEGIN
      !p.multi=[0,1,2]
      wvt_display_pixels, x, y, rnd[binnumber], 1.
      oplot, xnode,ynode, psym=1, symsize=0.5, color=255
      w1 = where(area LE 2 OR area GT max_area*.95, COMPLEMENT=w2, m)
      IF n_elements(w2) GT 2 THEN $
        snhist=histogram(snbin[w2], binsize=targetsn/20., min=0., $
          max=targetsn*2., locations=snbinloc) $
      ELSE snhist=[0]      
      snbinval=snbinloc+.5*targetsn/20.
      plot, snbinval, snhist , psym=10, xtitle='S/N', $
        ytitle='Number of bins', $
        xrange=[0,targetsn*2.], yrange=[0,nbins/2.], xstyle=1, ystyle=1
  ENDIF
    area = HISTOGRAM(binnumber, REVERSE_INDICES=r,min=0, max=nbins-1)
    w = where(area GT 0, m) ; Check for zero-size Voronoi bins

    ; Recompute the new node centers
    FOR j=0L,m-1 DO BEGIN
      k = w[j]                 ; Only loop over nonzero bins
      index = r[r[k]:r[k+1]-1] ; Find subscripts of pixels in bin k.
      IF j GE nfixed THEN BEGIN    
        IF keyword_set(gersho) THEN $
           wvt_weighted_centroid, x[index], y[index], dens2[index], xb, yb $
        ELSE $ ; To get final right
          wvt_unweighted_centroid, x[index], y[index], xb, yb  
      ENDIF ELSE BEGIN
        xb=keepfixed[0,j]
        yb=keepfixed[1,j]
      ENDELSE
      xnode[k] = xb 
      ynode[k] = yb
    ENDFOR

    ;Now make sure that still each bin has at least one pixel 
    ;  wvt_check_all_bins, binnumber, x, y, xnode, ynode, area=area, $
    ;    reverse_indices=r

    ; Redistribute enclosed bins ("bin islands")
    wvt_check_binislands,  x, y, binnumber, neighborbinnumber, xnode, ynode, $
      area=area, reverse_indices=r

    ; Calculate the percentage of pixels that switched bins.
    If iter GT 1 THEN pixelswitch=total(abs(oldbinnumber-binnumber)<1) $   
      ELSE pixelswitch=n_elements(x)      ; # pixels that changed binnumber
    oldbinnumber=binnumber
    diff = sqrt(TOTAL((xnode-xnodeOld)^2+(ynode-ynodeOld)^2))
  
    IF not keyword_set(quiet) THEN BEGIN
      IF iter GT 1 THEN BEGIN
        print, 'Iteration ', strtrim(iter,2), ', pixels that switched bins: ',$
          strcompress(string(pixelswitch/npixels*100),/remove_all)+'%'
      ENDIF ELSE BEGIN
        print, 'Initial WVT done.'
      ENDELSE
    ENDIF
    iter = iter + 1

  area = HISTOGRAM(binnumber, REVERSE_INDICES=r) 
  w1 = where(area LE 2 OR area GT max_area*.95, COMPLEMENT=w2, m)
  IF n_elements(w2) GT 3 THEN snwidth=stddev(snbin[w2]-avg(snbin[w2])) $
    ELSE snwidth=-1

  ; Save the pixelswitch parameter in ps_array for the last navg iterations
  ps_array=shift(ps_array,-1)
  ps_array[iter<(navg-1)]=pixelswitch
  IF iter LE 2 THEN psavg=pixelswitch
  oldpsavg=psavg
  psavg=avg(ps_array[0:iter<(navg-1)])  

  ; Stop the iteration if all pixels stop switching bins, 
  ; you hit the maximum number of allowed iterations, if the program is 
  ; terminated externally by the creating the control file "stopmenow", or
  ; if the convergence has flattened out
  ENDREP UNTIL ( file_test('stopmenow') OR $
                 iter GE max_iter OR $
                 pixelswitch EQ 0. OR $
                 (psavg GE oldpsavg AND iter GT 10+navg $ 
                    AND oldpsavg lt 1.1*pixelswitch) ) 

  IF file_test('stopmenow') THEN $
	  print, "Control file stopmenow found, iteration stopped manually"

  IF iter GT max_iter THEN $
    print, "Iteration reached maximum number of iterations. This should "+$
           "not happen in general except for data with an extremely large "+$
           "dynamical range. Check your input files and/or restart the "+$
           "iteration from this point with the save_all and resume keywords."

  IF pixelswitch EQ 0. THEN $
    print, "Iteration converged to a stable WVT solution"

  IF (psavg GE oldpsavg AND iter GT 10+navg AND oldpsavg lt pixelswitch) THEN $
    print, "Changes per iteration have flattened out. The results are very "+$
           "unlikely to change from this point on. If you are not satisfied "+$
           "with the solution, restart from this point with the save_all "+$
           "and resume keywords." 

  weight=sqrt(snbinbar)

END

;-----------------------------------------------------------------------
PRO WVT_DISPLAY_PIXELS, x, y, counts, pixelSize, countmax=countmax, $
    countmin=countmin
  ; Plots colored pixels with the same pixels size
  
  PLOT, [MIN(x)-pixelSize,MAX(x)+pixelSize], $
    [MIN(y)-pixelSize,MAX(y)+pixelSize], $
    /NODATA, /XSTYLE, /YSTYLE, XTITLE='pixel', YTITLE='pixel', /iso

  IF NOT(keyword_set(countmin)) THEN countMax = MAX(counts,MIN=countMin) 

  countRange = countMax - countMin
  FOR j=0L, N_ELEMENTS(counts)-1 DO BEGIN
    xprime = x[j] + [-0.5, -0.5, +0.5, +0.5, -0.5] * pixelSize
    yprime = y[j] + [+0.5, -0.5, -0.5, +0.5, +0.5] * pixelSize
    POLYFILL, xprime, yprime, COLOR=FLOAT(counts[j]-countMin)/countRange*255
  ENDFOR
END

;----------------------------------------------------------------------
PRO WVT_MAKENEIGHBORLIST,x,y,pixelsize,neighborlist
  ; Create a list of neighbors for each pixel

  nx=n_elements(x)
  ny=n_elements(y)
  IF (nx ne ny) THEN message, 'X and Y coordinate lists incompatible.'
  npixels=nx
  neighborlist=lonarr(npixels,4)

  ; Sort pixels in order of x and y
  sox=sort(x) ; sorted pixel indices
  soy=sort(y)
  xsox=x[sox] ; sorted x and y values 
  ysoy=y[soy]

  ; Find unique values of x and y
  uniquex=xsox[uniq(xsox)]
  uniquey=ysoy[uniq(ysoy)]
  nuniquex=n_elements(uniquex)
  nuniquey=n_elements(uniquey)

  ; Make a square mask showing the pixel numbers
  mask=lonarr(nuniquex+2,nuniquey+2)-1

  FOR i=0L,npixels-1 DO BEGIN
    IF (float(i)/10000. eq float(i/10000)) THEN print,i," pixels done"
    ix=where(x[i] eq uniquex)
    iy=where(y[i] eq uniquey)
    ix=ix[0]+1
    iy=iy[0]+1
    mask[ix,iy]=i
  ENDFOR

  FOR i=1L,nuniquex DO BEGIN
    FOR j=1L,nuniquey DO BEGIN
      pix=mask[i,j]
      IF (pix ge 0) THEN BEGIN
        temp=[mask[i-1,j],mask[i,j+1],mask[i+1,j],mask[i,j-1]]
        temp=temp[reverse(sort(temp))]
        neighborlist[pix,*]=[temp]
      ENDIF
    ENDFOR
  ENDFOR
END

;----------------------------------------------------------------------
PRO WVT_FINDNEIGHBORS, group, neighborlist, neighbors, exneighbors, newmember
  ; Given the neighborlist for each pixel in the image, and a group of test
  ; pixels, return a list of neighbors of the group's members that aren't
  ; already group members themselves.

  ngroup=n_elements(group)
  IF ngroup LE 2 OR n_elements(exneighbors) LT 1 THEN BEGIN
    neighbors=neighborlist[group,*] 
    neighbors=neighbors(uniq(neighbors, sort(neighbors)))
    match, neighbors[*], group, subneighbor, subgroup;,/sort
    IF subneighbor[0] ne -1 THEN neighbors(subneighbor)=-1
    wh=where(neighbors ne -1)
    IF wh[0] EQ -1 THEN neighbors=-1 ELSE neighbors=neighbors(wh)
    exneighbors=intarr(n_elements(neighborlist)/4)
    IF neighbors[0] NE -1 THEN exneighbors[neighbors]=1
    exneighbors[group]=-1
  ENDIF ELSE BEGIN
    tmp2=neighborlist(newmember,*)
    wh=where(tmp2 NE -1)
    IF wh[0] NE -1 THEN BEGIN
      tmp=tmp2[wh]
      subtmp=where(exneighbors[tmp] EQ 0)
      IF subtmp[0] ne -1 THEN BEGIN
        tmp=tmp[subtmp]
        exneighbors[tmp]=1 
        neighbors=[neighbors,tmp]
      ENDIF
    ENDIF

    ; We already know that the given list of neighbors is unique, since
    ; we use the output from the last run. So there is no need to match them
    ; again with everything.
    ; First check which neighbors of the new members are already part of 
    ; the group. All group members have an index of -1.
    exneighbors[newmember]=-1
    subneighbors=where(neighbors NE newmember)
    IF subneighbors[0] NE -1 THEN neighbors=neighbors[subneighbors] $
      ELSE neighbors=-1
  ENDELSE

END

;----------------------------------------------------------------------

PRO WVT_BINNING, x, y, pixelSize, targetSN, $
    binnumber, xNode, yNode, weight, area=area, QUIET=quiet, $
    dens=dens, binvalue=binvalue, snbin=snbin, plotit=plotit, $
    resume=resume, neighborlist=neighborlist, max_area=max_area, $
    gersho=gersho, keepfixed=keepfixed, center=center
  ; This is the main program that has to be called from external programs. 
  ; It simply calls in sequence the different steps of the algorithms
  ; and optionally plots the results at the end of the calculation.
  COMMON DATAVALUES, P

  IF NOT(keyword_set(plotit)) THEN plotit=0 ELSE BEGIN
    loadct, 5
  ENDELSE

  npix = n_elements(x)
  IF NOT(keyword_set(resume)) THEN BEGIN

    IF n_elements(targetSN) ne 1 THEN message, 'targetSN must be a scalar'
    IF n_elements(dens) NE n_elements(x) THEN BEGIN
      dens=dblarr(n_elements(x))
      FOR i=0L,npix-1L DO dens[i]=add_signal([i])/add_noise([i])
    ENDIF
    print, 'Bin-accretion...'
    wvt_bin_accretion, x, y, dens, targetSN, binnumber, pixelSize, $
      QUIET=quiet, SNBin=SNBin, neighborlist=neighborlist, $
      max_area=max_area, keepfixed=keepfixed, center=center
    print, strtrim(max(binnumber),2), ' initial bins.'
    print, 'Reassign bad bins...'
    wvt_reassign_bad_bins, x, y, dens, targetSN, binnumber, xnode, $
      ynode, SNBin=SNBin
  ENDIF

  seed=-1
  rnd = randomu(seed,max(binnumber)) ; Randomize bin colors
  IF plotit GE 2 OR file_test('plotit') THEN BEGIN
      area2 = HISTOGRAM(binnumber, REVERSE_INDICES=r) 
      !p.multi=[0,1,2]
      wvt_display_pixels, x, y, rnd[binnumber], 1.
      oplot, xnode,ynode, psym=1, symsize=0.5, color=255
      w1 = where(area2 LE 2 OR area2 GT max_area*.95, COMPLEMENT=w2, m)
      IF n_elements(w2) GT 2 THEN $
        snhist=histogram(snbin[w2], binsize=targetsn/20., min=0., $
          max=targetsn*2., locations=snbinloc) $
      ELSE snhist=[0]      
      snbinval=snbinloc+.5*targetsn/20.
      plot, snbinval, snhist , psym=10, xtitle='S/N', $
        ytitle='Number of bins', xrange=[0,targetsn*2.], $
        yrange=[0,max(binnumber)/2.], xstyle=1, ystyle=1
  ENDIF

  print, strtrim(n_elements(xnode),2), ' good bins.'

  print, '(Extremely) modified Lloyd algorithm...'
  IF file_test('stopmenow') THEN BEGIN
    print, 'File "stopmenow" exists, exiting after bin accretion...'
    iter=1
  ENDIF ELSE BEGIN
    wvt_equal_mass, x, y, dens, binnumber, xnode, ynode, iter, QUIET=quiet, $
    plotit=plotit, targetsn=targetsn, neighborlist=neighborlist, $
    max_area=max_area, gersho=gersho, keepfixed=keepfixed, weight=weight
  ENDELSE

  wvt_calc_bin_sn, binnumber, SNbin, binvalue=binvalue, area=area
  print, STRTRIM(iter-1,2), ' iterations.'

  w1 = where(area LE 2 OR area GT max_area*.95, COMPLEMENT=w2, m)
  print, 'Fractional S/N scatter (%) around target S/N:', $
    stddev(snbin[w2]-targetSN)/targetSN*100
  averagesn=total(snbin[w2])/double(n_elements(w2))
  print, 'Average S/N:', averagesn
  print, 'Fractional S/N scatter (%) around average S/N:', $
    stddev(snbin[w2]-averagesn)/averagesn*100

  IF plotit GE 1 OR file_test('plotit') THEN BEGIN
      !p.multi=[0,1,2]
      wvt_display_pixels, x, y, rnd[binnumber], 1.
      oplot, xnode,ynode, psym=1, symsize=0.5, color=255
      IF n_elements(w2) GT 2 THEN $
        snhist=histogram(snbin[w2], binsize=targetsn/20., min=0., $
          max=targetsn*2., locations=snbinloc) $
      ELSE snhist=[0]      
      snbinval=snbinloc+.5*targetsn/20.
      plot, snbinval, snhist , psym=10, xrange=[0,targetsn*2.], $
        xstyle=1, ystyle=0, xtitle='S/N', ytitle='Number of bins'
  ENDIF

END
;----------------------------------------------------------------------------

