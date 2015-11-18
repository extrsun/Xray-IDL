;;; $Id: estimate_poisson_background.pro 3134 2008-07-10 18:37:44Z patb $
;;; Estimates a Poisson background (mean) value for an INTEGER data set 
;;; contaminated by positive outliers (e.g. stars).

;;; Test with calls like:
;;;   estimate_poisson_background, random(100000, POISSON=0.1), /PLOT, /VERBOSE
;;; or run the test program at the end of this file.

;;; The algorithm is to find the smallest 1-sided [0..N] confidence interval 
;;; that encompasses the specified probability for a Poisson random variable 
;;; whose mean is the observed mean of the data in that confidence interval.

PRO estimate_poisson_background, data, EMAP=emap_param, background_estimate, SIGNIFICANCE=significance, PLOT=do_plot, VERBOSE=verbose

if ~keyword_set(significance) then significance = 0.99
if ~keyword_set(verbose)      then verbose      = 0

num_data = n_elements(data)

xdim = (size( data, /DIMEN ))[0]
if (size(data, /N_DIM) EQ 2) then begin
    ydim = (size( data, /DIMEN ))[1]
endif else begin
    ydim = 1
endelse

if keyword_set(emap_param) then begin
  ;; Verify that the data image contains no counts where the exposure is <= zero.
  ind = where(emap_param LE 0, count)
  if (count GT 0) then begin
    dum = where(data[ind] NE 0, count)
    if (count GT 0) then begin
      print, 'estimate_poisson_background: WARNING, ignoring data where exposure is <= zero!'
      data[ind] = 0
    endif
  endif

  emap = emap_param 

  if (xdim NE (size( emap, /DIMEN ))[0]) OR (xdim*ydim NE n_elements(emap)) then $
    message, 'estimate_poisson_background ERROR: data and emap must have the same dimensions'
  
  
  flux = data / emap
  
  use_pixel = bytarr(xdim,ydim)
  
  ; Initial bkg estimate is made using pixels with below median flux.
  ; The function median() ignored NaN values. 
  use_pixel[where(flux LE median(flux))] = 1
  bkg_flux = -1
  
  for dum = 1, 1000 do begin
    previous_bkg_flux = bkg_flux
    
    ; Estimate a mean flux over the pixels we've chosen to consider.
    ind = where(use_pixel, num_good)
    bkg_flux = total(data[ind], /DOUBLE) / total(emap[ind], /DOUBLE)
    
    ; Compute an expected background signal in each pixel.
    mean_bkg_counts = bkg_flux * emap
    
    if (verbose GE 2) then begin
       print, bkg_flux, 100*(1 - num_good/float(num_data)), $
           F='(%"background flux = %0.2g; %5.1f%% of data discarded ")'
    endif
    
    if (dum GE 10) then break
    if (bkg_flux EQ previous_bkg_flux) then break
    
    ; Examine each pixel to decide whether it is contaminated by source light.
    for ii=0L, n_elements(data)-1 do begin
      ; Compute the 1-sided confidence interval using the estimate of the mean background signal in this pixel.
      confidence_limit =  1 > poisson_interval(mean_bkg_counts[ii], significance)
;help, confidence_limit  
      use_pixel[ii] = (data[ii] LE confidence_limit)
    endfor ;ii
    
    use_pixel = temporary(use_pixel) AND (emap GT 0)
  endfor ;dum
  
  if (verbose GE 1) then begin
     print, bkg_flux, 100*(1 - num_good/float(num_data)), $
         F='(%"\nbackground flux = %0.2g; %5.1f%% of data discarded ")'
  endif
    
  background_estimate = bkg_flux

endif else begin

  ;; Accept all the data below the median.
  max_val  = median(data, /EVEN)
  
  repeat begin
    max_val = max_val + 1
  
    mean_bkg_counts = mean(/DOUBLE, data[where(data LE max_val, num_good)])
    
    if (num_good EQ num_data) then break
    
    ; Compute the 1-sided confidence interval using this estimate of the mean.
    confidence_limit =  2 > poisson_interval(mean_bkg_counts, significance)
  
  endrep until (max_val GE confidence_limit)
  
  if keyword_set(do_plot) then begin
    hist = histogram( data, MIN=0, MAX=ceil(max_val) )
    plot, hist, XSTYLE=3, PSYM=10
    oplot, [mean_bkg_counts,mean_bkg_counts], [0, max(hist)], LINE=1
  endif
  
    if (verbose GE 1) then begin
    print, mean_bkg_counts, 100*(1 - num_good/float(num_data)), max_val, $
           F='(%"\nbackground = %0.2g; %5.1f%% of data discarded above %f")'
  endif
  
  background_estimate = mean_bkg_counts
endelse

return
end



PRO test
files = findfile("*.img")

for ii=0,n_elements(files)-1 do begin
  print
  print, files[ii]
  im=readfits(files[ii])
  estimate_poisson_background, im
  wait,3
endfor
return
end
