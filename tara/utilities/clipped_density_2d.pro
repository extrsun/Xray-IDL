;;; $Id: clipped_density_2d.pro 2644 2006-12-14 15:01:29Z patb $
;;; Patrick Broos, Penn State University, 2002

;;; This is a tool that performs a non-standard and unproved method of 
;;; adaptive kernel image smoothing based on the notion that the flux in
;;; all the members of a smoothing kernel should be statistically
;;; consistent with the central pixel in that kernel.

;;; If KERNEL_FILE is supplied a block describing each kernel is 
;;; written to an unformatted file.  Each block has this format:
;;;
;;; longx1: total number of pixels under the kernel >=1
;;;
;;; longx1: number (N) of member pixel 1-D indexes following
;;;
;;; longxN: 1-D indexes of kernel members;  the first member is the central
;;;         pixel of the kernel -- others included only if they contain data
;;;
;;; These blocks are written in the 1-D indexing order of the image.

;;;================================================================== 
PRO clipped_density_2d, image, min_significance, $
			EMAP=emap_param, MAX_RADIUS=max_radius, $
			GAUSSIAN=gaussian, $
			flux_map, error_map, radius_map, exclude_map, $
			KERNEL_FILE=kernel_file


;; Type check inputs.
if (size(image,/TYPE) GT 3) then $
  message,'Parameter "image" must be integral type.'

if NOT keyword_set(max_radius) then max_radius=40
max_radius = floor(max_radius)

save_kernels = keyword_set(kernel_file) 
if save_kernels then begin
  openw, kernel_unit, kernel_file, /GET_LUN
endif

;; It is vital to the operation of the search below that the radii
;; list starts with r=0 (a kernel consisting of only the central pixel).
radii = indgen(1+max_radius)
max_radius_index = n_elements(radii)-1
num_kernels      = n_elements(radii)

if keyword_set(gaussian) then begin
  ;; Precompute Gaussian kernels with various radii == sigma out to 3 sigma.  
  ;; The kernel dimension must be odd.
  half_kernel_dim = 3*max_radius
  kernel_dim      = 1 + 2*half_kernel_dim
  print, kernel_dim, kernel_dim, F='(%"Building Gaussian kernels up to %dX%d pixels...")'
  
  kernels   = fltarr(kernel_dim,kernel_dim,num_kernels)
  distances = shift( dist(kernel_dim), half_kernel_dim, half_kernel_dim)
  
  kernels[half_kernel_dim,half_kernel_dim,0] = 1
  for ii = 1,num_kernels-1 do begin
    kernel = distances / radii[ii]
    mask   = (kernel LE 3)
  
    kernels[*,*,ii] = mask * exp( (kernel^2) / (-2.0) )
  endfor

endif else begin
  ;; Precompute Epanechnikov kernels with various radii.  
  ;; The kernel dimension must be odd.
  half_kernel_dim = max_radius
  kernel_dim      = 1 + 2*half_kernel_dim
  print, kernel_dim, kernel_dim, F='(%"Building Epanechnikov kernels up to %dX%d pixels...")'
  
  kernels   = fltarr(kernel_dim,kernel_dim,num_kernels)
  distances = shift( dist(kernel_dim), half_kernel_dim, half_kernel_dim)
  
  for ii = 0,num_kernels-1 do begin
    kernel = distances / (radii[ii] + 1)
    mask   = (kernel LT 1.0)
  
    kernels[*,*,ii] = mask * (1.0 - kernel^2)
  endfor
endelse

kernel_footprint, kf_id, /CREATE, IMAGE=image, KERNELS=kernels

;; Define the criteria used to classify an observed pixel value as 
;; "statistically inconsistent" with the Poisson distribution assumed to 
;; describe that pixel.
max_counts = max(image)
poisson_consistent, central_pixel_standard, /CREATE, PROB_FALSE_NEGATIVE=2E-4, $
		    MAX_COUNTS=max_counts
poisson_consistent, neighbor_pixel_standard, /CREAT, PROB_FALSE_NEGATIVE=0.05, $
		    MAX_COUNTS=max_counts


xdim = (size( image, /DIMEN ))[0]
ydim = (size( image, /DIMEN ))[1]

if keyword_set(emap_param) then begin
  max_emap = float(max(emap_param))
  emap = emap_param / max_emap
  print, max_emap, F='(%"Exposure map normalized by %g")'

  s=where(emap LT 0.1, count)
  if (count GT 0) then begin
    emap[s] = 0
    print, count, F='(%"WARNING, exposure set to zero for %d pixels that have")'
    print, "less than 1/10th nominal exposure."
  endif
endif else begin
  emap = replicate(1,xdim,ydim)
endelse

flux_map   = replicate(-1e-10,xdim,ydim)
error_map  = replicate(-1e-10,xdim,ydim)
radius_map = replicate(-1,    xdim,ydim)
exclude_map= replicate(-1,    xdim,ydim)

checkpoint_rows = round([.1,.2,.3,.4,.5,.6,.7,.8,.9]*ydim) 
start_time = systime(1)
print, 'starting ...'

initial_radius_index = 0 

for y = 0,ydim-1 do begin
  if (y EQ checkpoint_rows[0]) then begin
    elapsed_time = (systime(1)-start_time)
    estimated_total_time = ydim * (elapsed_time/checkpoint_rows[0])
    print, checkpoint_rows[0], (estimated_total_time - elapsed_time)/60, $
	   F='(%"%d rows processed; estimate %d more minutes to finish")'
    checkpoint_rows = shift(checkpoint_rows,-1)
  endif

  for x = 0,xdim-1 do begin
  
    ;; Outside the field of view all outputs are undefined.
    if (emap[x,y] EQ 0) then begin
      if save_kernels then writeu, kernel_unit, 0L, 0L
      continue
    endif
    
    
    ;; Search for an unpruned kernel whose flux estimate has at least the 
    ;; desired significance.    
    done = 0
    search_direction = 0
    radius_index     = initial_radius_index
    repeat begin
      ;; Retrieve the 1-D indexes of pixels that fall under the kernel.
      kernel_footprint, kf_id, x, y, radius_index, pixel_list, weight
      
      ;; Estimate a flux by simply computing a weighted sum of the
      ;; observed counts and dividing by a weighted sum of the exposures.
      exposure      = total(weight *  emap[pixel_list])
      flux_estimate = total(weight * image[pixel_list]) / exposure
      		      
      
      ;; Now, estimate the error on this flux which we consider to be a 
      ;; random variable (not Poisson) that is a simple linear combination
      ;; of the random variables representing the pixels in this kernel.  
      ;; Propogating errors through the linear equation above is easy -- the
      ;; slippery part is assigning errors to the individual pixel random
      ;; variables.  There would seem to be two ways(at least) we could do that:
      ;;
      ;; (1) If we assume that the actual mean flux in each pixel is the SAME
      ;; then the observed counts in each pixel is a Poisson random variable
      ;; characterized by the mean count rate in that pixel, which is simply
      ;; our flux estimate times the exposure for that pixel.  Since these
      ;; pixel random variables are Poisson their variances are equal to
      ;; their means.
      ;;
      ;; (2) If we assume that the actual mean flux in each pixel is
      ;; DIFFERENT, then the pixel random variables have different Poisson
      ;; distributions not only because of exposure variations but because of
      ;; intrinsic flux variations between pixels.  Thus, we are forced to 
      ;; estimate the Poission distribution describing each and every pixel
      ;; independently, using only that pixel's observed counts.
      ;; But, we often have pixel values of 0 and 1 -- what Poisson distribution
      ;; (and thus variance) could one reasonably assign to those??
      ;;
      ;; We choose the first approach.  First compute the expected number of
      ;; observed counts in each pixel, i.e. the mean of the Poisson 
      ;; distribution underlying each observed pixel value.  
      ;; The weights are not relevant here.
      poisson_means = flux_estimate * emap[pixel_list]
      
      ;; The variance of *each* pixel RV is simply this mean value.
      ;; We simply propogate these variances through the linear equation
      ;; used above to compute a flux estimate, then take the sqrt() of the 
      ;; new variance to get a standard deviation, aka "flux_error". 
      flux_error = sqrt(total(weight^2 * poisson_means)) / exposure
            
      significance_is_good = ((flux_estimate/flux_error) GE min_significance)
      
      if significance_is_good then begin
        if (search_direction EQ 1) then begin
          ; We were searching up from bad kernels and found a good one, so
          ; stop and keep this kernel.
          done = 1 
        endif else begin
          ; We just started, or were searching down and found a good one, so
          ; we need to keep going down, if possible.
          if (radius_index LE 0) then begin
            done = 1 
          endif else begin
            search_direction = -1
            radius_index     = radius_index - 1
          endelse
        endelse
        
      endif else begin
        if (search_direction EQ -1) then begin
          ; We were searching down from good kernels and found a bad one, so
          ; stop and keep the next larger kernel.
          radius_index = radius_index + 1
	  kernel_footprint, kf_id, x, y, radius_index, pixel_list, weight 
      
	  exposure      = total(weight *  emap[pixel_list])
	  flux_estimate = total(weight * image[pixel_list])  /exposure
	  poisson_means = flux_estimate * emap[pixel_list]
	  flux_error = sqrt(total(weight^2 * poisson_means)) / exposure

          done = 1
        endif else begin
          ; We just started, or were searching up and found a bad one, so
          ; we need to keep going up, if possible.
          if (radius_index GE max_radius_index) then begin
            done = 1 
          endif else begin
            search_direction = 1
            radius_index     = radius_index + 1
          endelse
        endelse
      endelse

    endrep until done
    
    ;; Save the next smaller kernel as the starting point for the next pixel.
    ;; The way the search above works, if the starting kernel turns out to
    ;; be the one we're looking for, then we must step down one kernel and 
    ;; then back up, wasting time.  If we start just below the goal, then
    ;; we make one step and we're done.
    initial_radius_index = (radius_index - 1) > 0
    
          
    ;; Search for an pruned kernel whose flux estimate has at least 
    ;; the desired significance and whose central pixel is consistent 
    ;; with the flux estimate. 
    done = 0
    searching_down = 0
    repeat begin
      kernel_size = n_elements(pixel_list)

      ;; Prune neighbor pixels (not the central pixel) in the kernel that are
      ;; not statistically consistent with the flux estimate.

      ;; For each pixel under kernel, compute the probability of observing
      ;; the number of counts in the image from a Poisson distribution with
      ;; the specified mean.
      ;; If the observed number of counts is statistically inconsistent with
      ;; the specified Poisson distribution, then mark the pixel as excluded
      ;; from the new group.

        ;; Using current flux estimate for the group, compute mean number of 
        ;; counts one should observe at each pixel under kernel.
        poisson_means     = flux_estimate * emap[ pixel_list ]
        observed_pix_vals = image[ pixel_list ]
        
	poisson_consistent, neighbor_pixel_standard, $
			    poisson_means, observed_pix_vals, is_consistent

      ;; We don't want to test the  
      ;; central pixel (guaranteed by kernel_footprint.pro to be pixel_list[0]) 
      ;; yet.
      is_consistent[0] = 1

      ;; Find the pixels in new group that are consistent with the current
      ;; flux estimate (from a prior iteration).
      s = where(is_consistent)
      pixel_list = pixel_list[s]
      weight     = weight[s]

      ;; compute a new flux estimate.
      exposure      = total(weight *  emap[pixel_list])
      flux_estimate = total(weight * image[pixel_list])  /exposure
      poisson_means = flux_estimate * emap[pixel_list]
      flux_error = sqrt(total(weight^2 * poisson_means)) / exposure
      
      ;; Determine if the central pixel is statistically consistent with 
      ;; the flux estimate.
      ;; Note that we're not retesting ALL the group members to see that
      ;; they are consistent with flux_estimate.  One could use an 
      ;; algorithm that would iteratively sigma-clip each kernel's members
      ;; until a self-consistent group & flux estimate were defined, but 
      ;; that would take a lot of time.  
      observed_pix_val = image[ pixel_list[0] ] 
      
      poisson_consistent, central_pixel_standard, $
			  poisson_means[0], observed_pix_val, root_is_consistent


      ;; Decide how to proceed with the search, based on whether the root is 
      ;; excluded, whether we have met our S/N goal, and how we were already
      ;; searching.
      if (NOT root_is_consistent[0]) then begin
	;; When the root is excluded the current kernel is unacceptable; 
	;; we must try smaller kernels.
        searching_down = 1
	if (radius_index LE 0) then begin
	  message, 'BUG: root excluded for smallest kernel!'
        endif else radius_index = radius_index - 1
	
      endif else begin
        if ((flux_estimate/flux_error) GE min_significance) then begin
	  ;; When the root is included and the S/N meets the goal, 
	  ;; we have the kernel we wanted.
	  done = 1
	endif else begin
	  ;; When the root is included and the S/N is still too low, 
	  ;; our actions depend on how we were already searching.
	  if (searching_down) then begin
	    ; Already searching down; accept this kernel as the best we can do.
	    done = 1
	    print, x,y,observed_pix_val, poisson_means[0], F='(%"Kernel size limited so central pixel (%4d,%4d)=%d is consistent with expected counts = %f")'

	  endif else begin
	    ; No prior search, or already searching up; search up.
	    if (radius_index GE max_radius_index) then begin
	      done = 1 
	      print, x,y, F='(%"Kernel at (%4d,%4d) truncated at max size.")'
            endif else radius_index = radius_index + 1
	  endelse
	endelse ;S/N too low
      endelse ;root included


      if (NOT done) then begin
        ;; If not done then prepare for the next iteration by retrieving
        ;; the 1-D indexes of pixels that fall under the next kernel.
        kernel_footprint, kf_id, x, y, radius_index, pixel_list, weight 
      
        exposure      = total(weight *  emap[pixel_list])
        flux_estimate = total(weight * image[pixel_list])  /exposure
        poisson_means = flux_estimate * emap[pixel_list]
        flux_error = sqrt(total(weight^2 * poisson_means)) / exposure
      endif
    endrep until done

    flux_map  [x,y] = flux_estimate
    error_map [x,y] = flux_error
    radius_map[x,y] = radii[radius_index]
    exclude_map[x,y]= kernel_size - n_elements(pixel_list)
    
    if save_kernels then begin
      has_data    = image[pixel_list] GT 0
      has_data[0] = 1B
      ind = where(has_data, count)
      
      writeu, kernel_unit, n_elements(pixel_list), count, pixel_list[ind]
    endif
      
;if exclude_map[x,y] GT 0 then begin
;  s = where(is_consistent EQ 0,count)
;  print, exclude_map[x,y],  ' pixels excluded'
;  for ii=0,count-1 do $
;   if (poisson_means[s[ii]] GE 9) then print, $
;     (observed_pix_vals[s[ii]]-poisson_means[s[ii]])/sqrt(poisson_means[s[ii]]), ' sigma'
;   print, observed_pix_vals[s[ii]], poisson_means[s[ii]]
;endif

  endfor ;; y loop
endfor ;; x loop

print, xdim*ydim, (systime(1)-start_time)/60, $
	   F='(%"%d pixels processed in %d minutes")'

if save_kernels then free_lun, kernel_unit

kernel_footprint, kf_id, /DESTROY
poisson_consistent, central_pixel_standard, /DESTROY
poisson_consistent, neighbor_pixel_standard, /DESTROY

return
end

