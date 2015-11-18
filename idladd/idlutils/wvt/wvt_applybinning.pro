; Apply existing binning to a different image
; Version 2.0, updated 12/05/2005
;
;######################################################################
;+
; NAME:
;     WVT_APPLYBINNING
;
; AUTHORS:
;       Steven Diehl & Thomas S. Statler, Ohio University, US
;       diehl@phy.ohiou.edu, statler@ohio.edu
;
; PURPOSE:
;       Apply an existing binning structure to a given data set.
;
; CALLING SEQUENCE:
;     binnedimage=WVT_APPLYBINNING(binnumber, inputsignal [, area=area, $
;       xraycolor=xraycolor, input2=input2, binvalue=binvalue])
;
; INPUT:   
;    BINNUMBER: Either 2d image or 1d pixel list, containing the bin
;               number of each pixel
;  INPUTSIGNAL: Has to be same size as binnumber and contain the signal per 
;               pixel
;
; OUTPUT:
;     BINVALUE: Binned signal value for each bin, divided by the bin AREA  
;         AREA: Area per bin, can be used to undo the normalization of BINVALUE
;    XRAYCOLOR: IF this keyword is set, the algorithm uses INPUTSIGNAL as 
;               band A and INPUT2 as band B to compute the hardness ratio 
;               A/B as the signal in the bin  
;       INPUT2: Band B for hardness ratios
;
; RETURNS: array of the same size as BINNUMBER and INPUTIMG, containing the 
;          binned signal, averaged over the bin area
;
; FUNCTIONS PROVIDED:
;             WVT_APPLYBINNING             -- main program
;
; EXAMPLE:
;    ; Adaptively bin an image, consisting of soft and hard band
;    signal=softimage+hardimage
;    noise=sqrt(softnoise^2+hardnoise^2)
;    WVT_IMAGE, signal, noise, targetSN, binnedimage, binnumber=binnumber 
;    ; Apply the binning from the full band to the soft band only
;    binnedmodelimage=wvt_applybinning(binnumber, softimage)
;
; MODIFICATION HISTORY (ORIGINAL VORONOI_2D_BINNING):
;       V2.0: First published version (12/05/2005)
;
;
;---------------------------------------------------------------------------
;

FUNCTION WVT_APPLYBINNING, binnumber, inputsignal, area=area, xraycolor=xraycolor, input2=input2, binvalue=binvalue

  in=inputsignal[*]
  IF keyword_set(xraycolor) THEN in2=input2[*]
  out=in
  class=binnumber[*]

  nbins=max(binnumber)
  binvalue=dblarr(nbins)
  npix=n_elements(in)
  IF NOT(keyword_set(area)) THEN $
    area = HISTOGRAM(class, REVERSE_INDICES=r ,min=1, max=nbins)

  FOR i=0L,nbins-1L DO BEGIN
     IF area[i] NE 0 THEN BEGIN
       w = r[r[i]:r[i+1]-1]
       IF keyword_set(xraycolor) THEN BEGIN        
         binvalue[i]=total(in[w])/total(in2[w])
         out[w]=binvalue[i]
       ENDIF ELSE BEGIN
         binvalue[i]=total(in[w])/area[i]   
         out[w]=binvalue[i]
       ENDELSE
     ENDIF
  ENDFOR

  binnedsignal=inputsignal
  binnedsignal[*]=out
  return, binnedsignal
END

