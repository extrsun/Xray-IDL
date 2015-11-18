; Reconstruct WVT binning structure from nodes positions and weights
; Version 2.0, updated 12/05/2005
;
;######################################################################
;+
; NAME:
;     WVT_GENERATEBINNING
;
; AUTHORS:
;       Steven Diehl & Thomas S. Statler, Ohio University, US
;       diehl@phy.ohiou.edu, statler@ohio.edu
;
; PURPOSE:
;       Reconstruct a weighted Voronoi tesselation from the positions of 
;       the generators and the weight values.
;
; EXPLANATION:
;       For additional information about WVT binning,
;       please refer to Diehl, S. & Statler, T.S. (2006).
;       Further information on the original VORONOI_2D_BINNING algorithm can 
;       be found in Cappellari M., Copin Y., 2003, MNRAS, 342, 345.
;
; CALLING SEQUENCE:
;     WVT_GENERATEBINNING, xnode, ynode, weight, binnumber, outmask $
;       [, do_pixellist=do_pixellist, x=x, y=y]
;
; EXAMPLE
;     ; Adaptively bin an image, INMASK has point sources masked out    
;     inmask=outmask-psrc_mask
;     WVT_IMAGE, signal, noise, targetSN, binnedimage, xnode, ynode, weight, $
;       mask=inmask, binvalue=binvalue, binnumber
;     ; Regenerate the binning for OUTMASK, which has no point sources 
;     ; excluded. (remember: binnumber starts at 1, not 0)
;     WVT_GENERATEBINNING, xnode, ynode, weight, binnumber_filled, outmask
;     binneddimage_filled=binvalue[binnumber_filled-1]
;
; INPUTS (REQUIRED):
;        XNODE: Vector (size Nbins) of the X coordinates of the bin 
;               generators, i.e the geometric centers of the bins.
;        YNODE: Vector (size Nbins) of Y coordinates of the bin generators.
;       WEIGHT: Weight of each bin in the WVT
;      OUTMASK: Two-dimensional image (n_x,n_y) specifying which 
;               pixels should be included in the WVT binning algorithm. 
;               Valid pixels are designated as "1", excluded pixels as "0" 
;               (integer or byte). The size of BINNUMBER will be equal to 
;               the dimensions of OUTMASK. 
;               
; OUTPUT:
;    BINNUMBER: Two-dimensional image (n_x,n_y) containing the bin number 
;               assigned to each input pixel. The index goes from 1 to 
;               Nbins. Pixels that were excluded during the binning process 
;               are marked "0" (see also the keyword OUTMASK).
;               This vector alone is enough for making *any* subsequent
;               computation on the binned data. Everything else is optional.
;         AREA: Vector (size Nbins) with the number of pixels for each bin.
;     BINVALUE: Vector (size Nbins) with the final signal of each bin
;
; KEYWORDS (OPTIONAL):
; DO_PIXELLIST: If this keyword is set, the input is taken as a list of 
;               pixels, whose xy coordinates should be specified by X and Y
;               keywords
;            X: x-coordinates of the pixel list
;            Y: y-coordinates of the pixel list
;
; PROCEDURES PROVIDED:
;       The following procedures are contained in the main WVT_BINNING program.
;       Refer to the main code for further explanations.
;            WVT_GENERATEBINNING           -- main program 
;
; FUNCTIONS PROVIDED:
;       The following functions are contained in the main WVT_BINNING program.
;       Refer to the main code for further explanations.
;            WVT_ASSIGN_TO_BIN            -- assigns a pixel to its WVT bin
;
; MODIFICATION HISTORY (ORIGINAL VORONOI_2D_BINNING):
;       V2.0: First published version (12/05/2005)
;
;
;----------------------------------------------------------------------------


FUNCTION WVT_ASSIGN_TO_BIN, x, y, xnode, ynode, SNnode
  ; Assigns each pixel to the S/N weighted closest pixel 
  ; i.e. this constructs the weighted voronoi tesselation
  ; Note: SNnode is the square of the weight!
  tmp = min(((x-xnode)^2+(y-ynode)^2)*SNnode, index)
  RETURN, index[0]
END


;---------------------------------------------------------------------------


PRO WVT_GENERATEBINNING, xnode, ynode, weight, binnumber, outmask, do_pixellist=do_pixellist, x=x, y=y

  IF keyword_set(do_pixellist) THEN BEGIN  
    binnumber=lonarr(n_elements(x))
    FOR i=0L, n_elements(x)-1L DO BEGIN 
      binnumber[i]=wvt_assign_to_bin(x[i], y[i], xnode, ynode, weight^2)+1L  
    ENDFOR
  ENDIF ELSE BEGIN
    dim=size(outmask,/dimensions)
    binnumber=dblarr(dim[0],dim[1])
    FOR i=0L, dim[0]-1L DO BEGIN
    FOR j=0L, dim[1]-1L DO BEGIN
      IF outmask[i,j] NE 0 THEN $
        binnumber[i,j]=wvt_assign_to_bin(i, j, xnode, ynode, weight^2)+1L  
    ENDFOR
    ENDFOR
  ENDELSE

  

END

