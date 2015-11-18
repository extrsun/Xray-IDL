;;; $Id: adaptive_density_2d.pro 4371 2012-10-30 20:14:49Z psb6 $
;;; Patrick Broos, Penn State University, 2002

;;; This is a tool that performs an adaptive kernel image smoothing.
;;; Despite the name, it works on both 2d data (e.g. an image) and 1d data (e.g. a light curve).

;;; The input data "image" must be integer counts (so that Poisson statistics can be used) if adaptive smoothing is desired (FIXED_RADIUS_MAP is not supplied).

;;; An optional exposure map "EMAP" can be supplied.

;;; An optional real-valued background map "BACKGROUND_MAP" can be supplied.
;;; This should be the _observed_ background, i.e. after passing through the exposure map.
;;; The significance computations assume there is no error on this background map!!

;;; You can specify pixels for which you do NOT want the output maps computed
;;; in either of two ways:
;;;
;;; 1. Supply a boolean array via keyword FIELD_MASK that is 1 "on field" 
;;;    and zero "off field".  Off field pixels can contain data and
;;;    contribute to the smoothing computations at other pixels, but no 
;;;    smoothed estimate is computed at those pixel locations. 
;;;
;;; For example this mechanism lets you save computing time if you have data
;;; in many pixels but you require the smoothed result in only a few places.
;;;
;;; 2. Set the exposure map (EMAP) array to a negative (flag) value for 
;;;    off field pixels.  The data image MUST be set to zero where ever
;;;    the exposure map is non-positive.
;;;
;;; For example this mechanism lets you represent edges of the field of view
;;; so that the output images don't extend outside that field of view.
;;;
;;; Off-field pixels have the value NaN in the output arrays 
;;; flux_map and error_map, and the value -1 in the output array radius_map.
;;;
;;; In scenes where the exposure tapers off to zero at the field edges
;;; pixels with very small relative exposure can cause artifacts in the
;;; flux image because even one count in the data image can correspond to
;;; a very large flux if the exposure is very small.
;;; Before applying adaptive_density_2d to such scenes I recommend that 
;;; you identify the very low exposure pixels, 
;;; e.g. ind = where(emap LT (max(emap)/10.))
;;; and then:
;;;  * set those pixels to zero in the data image (discarding the noisy data)
;;;  * mark those pixels as "off field" by setting their emap to -1

;;; Pixels with EMAP=0 and FIELD_MASK=1 are considered to be in the field
;;; but unobserved for whatever reason (e.g. masked out); a flux
;;; computation is performed there.  
;;; For example this mechanism can be used to "fill in" the "holes" in an 
;;; image where point sources have been masked out.  If you do not want such
;;; "holes" filled in then mark then as "off field" as discussed above.

;;; If the "radius_map" output from a previous adaptive_density_2d run is
;;; supplied in FIXED_RADIUS_MAP then the smoothing is NOT adaptive -- it uses
;;; the specified kernels.

;;; If KERNEL_FILE is supplied a block describing each kernel is 
;;; written to an unformatted file.  This feature is used by dataset_3d.pro.
;;; Each block has this format:
;;;
;;; longx1: total number (>=0) of pixels under the kernel
;;;
;;; longx1: number (>=0) of member pixel 1-D indexes following
;;;
;;; longxN: 1-D indexes of kernel members;  the first member is the central
;;;         pixel of the kernel -- others included only if they contain data
;;;
;;; These blocks are written in the 1-D indexing order of the image.
;;; When FIELD_MASK[ii] is zero the kernel block written is {0, 0}.


;;;================================================================== 
PRO adaptive_density_2d, image, min_significance_param, $
			EMAP=emap_param, BACKGROUND_MAP=background_map, FIELD_MASK=field_mask_param, $
			MAX_RADIUS=max_radius, MAX_NUM_KERNELS=max_num_kernels, RADII=radii, FIXED_RADIUS_MAP=fixed_radius_map_param,$
			GAUSSIAN=gaussian, EPANECHNIKOV=epan, $
			flux_map, error_map, radius_map, $
			KERNEL_FILE=kernel_file, SILENT=silent

; Protect caller's actual parameter variable because we can change min_significance here.
min_significance = min_significance_param

;; Type check inputs.
image_type = size(image,/TNAME)
if ~keyword_set(fixed_radius_map_param) && (image_type EQ 'FLOAT' || image_type EQ 'DOUBLE') then $
  message,'Parameter "image" cannot be FLOAT or DOUBLE.'

xdim = (size( image, /DIMEN ))[0]
if (size(image, /N_DIM) EQ 2) then begin
    ydim = (size( image, /DIMEN ))[1]
endif else begin
    ydim = 1
endelse

if ~keyword_set(background_map)  then background_map = replicate(0, xdim, ydim)

if keyword_set(field_mask_param) then field_mask     = field_mask_param $
                                 else field_mask     = replicate(1B, xdim, ydim)

if keyword_set(emap_param) then begin
  ;; Verify that the data image contains no counts where the exposure is <= zero.
  ind = where(emap_param LE 0, count)
  if (count GT 0) then begin
    dum = where(image[ind] NE 0, count)
    if (count GT 0) then message, 'ERROR: image must be zero where exposure is <= zero!'
    
    dum = where(background_map[ind] NE 0, count)
    if (count GT 0) then message, 'ERROR: background must be zero where exposure is <= zero!'
  endif

  emap = emap_param 

  ;; Consider negative emap values to be off-field flags.
  ;; Set negative emap values to zero.
  ind = where(emap LT 0, count)
  if (count GT 0) then begin
    field_mask[ind] = 0
    emap      [ind] = 0
    print, count, ' pixels flagged as off-field in emap'
  endif

endif else begin
  emap       = replicate(1, xdim,ydim)
endelse


;; Verify dimensions of inputs match.
if (xdim NE (size( background_map, /DIMEN ))[0]) OR (xdim*ydim NE n_elements(background_map)) then $
  message, 'ERROR: image and background_map must have the same dimensions'

if (xdim NE (size( field_mask, /DIMEN ))[0]) OR (xdim*ydim NE n_elements(field_mask)) then $
  message, 'ERROR: image and field_mask must have the same dimensions'

if (xdim NE (size( emap,       /DIMEN ))[0]) OR (xdim*ydim NE n_elements(emap)) then $
  message, 'ERROR: image and emap must have the same dimensions'



if keyword_set(fixed_radius_map_param) then begin
  fixed_radius_map = fixed_radius_map_param > 0
  ; Find the set of radii used in the provided map.
  radii = fixed_radius_map[ uniq(fixed_radius_map,sort(fixed_radius_map)) ]
  if NOT keyword_set(silent) then print, 'Using the fixed kernels supplied by caller.'
endif else if (n_elements(radii) GT 0) then begin
  ; The desired radii list is supplied.
endif else begin
  ; Create an incrementing radius list.
  if ~keyword_set(max_radius)      then max_radius      =ceil( (xdim>ydim)/2.0 )
  if ~keyword_set(max_num_kernels) then max_num_kernels = 50 
  
  if (max_radius LE max_num_kernels) then begin
    radii = indgen(1+max_radius)
  endif else begin
    radii = floor((float(max_radius)/max_num_kernels) * (indgen(1+max_num_kernels)))
  endelse
endelse

if (min(radii) LT 0) then message, 'ERROR: Negative radius value found!'

;; It is vital to the operation of the search below that the radii
;; list starts with r=0 (a kernel consisting of only the central pixel).
if (radii[0] NE 0) then radii = [0,radii]

max_radius_index = n_elements(radii)-1
num_kernels      = n_elements(radii)
max_radius       = max(radii)
if NOT keyword_set(silent) then print, 'Using kernel radii: ', radii


save_kernels = keyword_set(kernel_file) 
if save_kernels then begin
  openw, kernel_unit, kernel_file, /GET_LUN
endif


if keyword_set(gaussian) then begin
  ;; Precompute Gaussian kernels with various radii == sigma out to 3 sigma.  
  ;; The kernel dimension must be odd.
  half_kernel_dim = 3*max_radius
  kernel_dim      = 1 + 2*half_kernel_dim
  if NOT keyword_set(silent) then $
    print, kernel_dim, kernel_dim, F='(%"Building Gaussian kernels up to %d X %d pixels...")'
  
  kernels   = fltarr(kernel_dim,kernel_dim,num_kernels)
  distances = shift( dist(kernel_dim), half_kernel_dim, half_kernel_dim)
  
  kernels[half_kernel_dim,half_kernel_dim,0] = 1
  for ii = 1,num_kernels-1 do begin
    kernel = distances / radii[ii]
    mask   = (kernel LE 3)
  
    kernels[*,*,ii] = mask * exp( (kernel^2) / (-2.0) )
  endfor

  kernel_footprint, kf_id, /CREATE, IMAGE=image, KERNELS=kernels

endif else if keyword_set(epan) then begin
  ;; Precompute Epanechnikov kernels with various radii.  
  ;; The kernel dimension must be odd.
  half_kernel_dim = max_radius
  kernel_dim      = 1 + 2*half_kernel_dim
  if NOT keyword_set(silent) then $
    print, kernel_dim, kernel_dim, F='(%"Building Epanechnikov kernels up to %dX%d pixels...")'
  
  kernels   = fltarr(kernel_dim,kernel_dim,num_kernels)
  distances = shift( dist(kernel_dim), half_kernel_dim, half_kernel_dim)
  
  for ii = 0,num_kernels-1 do begin
    kernel = distances / (radii[ii] + 1)
    mask   = (kernel LT 1.0)
  
    kernels[*,*,ii] = mask * (1.0 - kernel^2)
  endfor
  
  kernel_footprint, kf_id, /CREATE, IMAGE=image, KERNELS=kernels

endif else begin
  if NOT keyword_set(silent) then $
    print, max_radius, F='(%"Building Tophat kernels up to radius = %d ...")'
  kernel_footprint, kf_id, /CREATE, IMAGE=image, RADII=radii
endelse 



flux_map   = replicate(!VALUES.F_NAN,xdim,ydim)
error_map  = replicate(!VALUES.F_NAN,xdim,ydim)
radius_map = replicate(-1,    xdim,ydim)

reporting_milestones = [.1,.2,.3,.4,.5,.6,.7,.8,.9, 1.1] 
numpix_in_field      = total(/INT, field_mask)

start_time = systime(1)
if NOT keyword_set(silent) then $
  print, 'starting ...'

initial_radius_index = 0 

for y = 0,ydim-1 do begin
  if ~keyword_set(silent) then begin
    ; Measure our progress.
    numpix_processed = total(/INT, finite(flux_map))
    
    ; Report progress if we've reached a milestone.
    if (float(numpix_processed)/numpix_in_field GT reporting_milestones[0]) then begin
      elapsed_time         = (systime(1)-start_time)
      estimated_total_time = (elapsed_time/reporting_milestones[0])

      print, 100*reporting_milestones[0], ceil((estimated_total_time - elapsed_time)/60.), $
	     F='(%"%d%% of pixels processed; estimate %d more minutes to finish")'
   
	    reporting_milestones = shift(reporting_milestones,-1)
	    
	    ; Report how many pixels have used the maximum kernel size.
	    count = total(/INT, radius_map EQ max_radius)
	    if (count GT 0) then print, (100.*count)/numpix_in_field, max_radius, F='(%"    %0.1f%% of kernels have been the maximum allowed size (%d pixels)")'
    endif ; report milestone
  endif ;~keyword_set(silent)


  for x = 0,xdim-1 do begin
      
    ;; Outside the field of view all outputs are undefined.
    if (field_mask[x,y] EQ 0) then begin
      if save_kernels then writeu, kernel_unit, 0L, 0L
      continue
    endif
    
    
    ;; Search for an unpruned kernel whose flux estimate has at least the 
    ;; desired significance.    
    done = 0

    if keyword_set(fixed_radius_map) then begin
      ; Look up the radius_index corresponding to the specified radius map value.
      radius_index     = (where(radii EQ fixed_radius_map[x,y]))[0]
      
      ; If this pixel skipped before then skip it this time too.
      if (radius_index EQ -1) then continue
      
      ; Arrange for the search loop below to exit after one pass.
      search_direction = 1
      min_significance = 0
    endif else begin
      ; Arrange for the search to have an efficient starting point and direction.
      radius_index     = initial_radius_index
      search_direction = 0
    endelse
    
    repeat begin
      ;; Retrieve the 1-D indexes of pixels that fall under the kernel.
      kernel_footprint, kf_id, x, y, radius_index, pixel_list, weight
 
 
      ;; Estimate a flux by simply computing a weighted sum of the
      ;; observed counts and dividing by a weighted sum of the exposures.
      exposure   = total(weight *           emap[pixel_list])
      counts     = total(weight *          image[pixel_list])
      background = total(weight * background_map[pixel_list])
      
      if ((counts LE 0) OR (exposure LE 0)) then begin
        observed_flux_estimate = 0
        net_flux_estimate      = 0
        flux_error    = 0
        significance_is_good = (min_significance EQ 0)
      endif else begin
        ;; In this algorithm "observed_flux_estimate" is a non-negative observed
        ;; flux estimate (counts under the kernel normalized by exposure under the kernel).
        ;; This non-negative flux is used below as a poisson mean in the equations that 
        ;; estimate significance.

        ;; The signed quantity net_flux_estimate has the background under the kernel subtracted off.
        ;; In the flux_error calculation below we make the assumption that there is little Poisson uncertainty in the background map.
        net_flux_estimate      = (counts - background) / exposure
        observed_flux_estimate = (counts             ) / exposure
      		      
      
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
        poisson_means = observed_flux_estimate * emap[pixel_list]
        
        ;; The variance of *each* pixel RV is simply this mean value.
        ;; We simply propogate these variances through the linear equation
        ;; used above to compute a flux estimate, then take the sqrt() of the 
        ;; new variance to get a standard deviation, aka "flux_error". 
        flux_error = sqrt(total(weight^2 * poisson_means)) / exposure
        
        ;; Significance is, however, computed as net flux over flux error.
        significance_is_good = ((net_flux_estimate/flux_error) GE min_significance)
      endelse
      
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
            
          exposure   = total(weight *           emap[pixel_list])
          counts     = total(weight *          image[pixel_list])
          background = total(weight * background_map[pixel_list])
          
          net_flux_estimate      = (counts - background) / exposure
          observed_flux_estimate = (counts             ) / exposure
          
          poisson_means = observed_flux_estimate * emap[pixel_list]
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
    ;; we make one step up and we're done.
    initial_radius_index = (radius_index - 1) > 0

    flux_map  [x,y] = net_flux_estimate
    error_map [x,y] = flux_error
    radius_map[x,y] = radii[radius_index]
    
    if save_kernels then begin
      has_data    = image[pixel_list] GT 0
      has_data[0] = 1B
      ind = where(has_data, count)
      
      writeu, kernel_unit, n_elements(pixel_list), count, pixel_list[ind]
    endif
      

  endfor ;; y loop
endfor ;; x loop

if NOT keyword_set(silent) then $
  print, xdim*ydim, (systime(1)-start_time)/60, $
	   F='(%"%d pixels processed in %d minutes")'

if save_kernels then free_lun, kernel_unit

kernel_footprint, kf_id, /DESTROY

return
end


PRO test_adaptive_density_2d

dim = 250

image = round(random(dim,dim, POI=0.5))

function_2d, id0, image, DATASET='data'

max_area   = n_elements(image) * 0.20
max_radius = ceil(sqrt(max_area/!PI))

significance = (1 / [0.2, 0.1, 0.05, 0.033333])
foreach min_significance_param, significance do begin

  adaptive_density_2d, image, min_significance_param, flux_map, error_map, radius_map, MAX_RADIUS=max_radius, GAUSSIAN=0

  name = string(min_significance_param, F='(%"SNR=%0.1f, tophat")')
  dataset_1d, id1, flux_map  , DATASET=name
  dataset_1d, id2, radius_map, DATASET=name

  
  adaptive_density_2d, image, min_significance_param, flux_map, error_map, radius_map, MAX_RADIUS=max_radius, GAUSSIAN=1

  name = string(min_significance_param, F='(%"SNR=%0.1f, Gaussian")')
  dataset_1d, id3, flux_map  , DATASET=name
  dataset_1d, id4, radius_map, DATASET=name
  function_2d, id0,flux_map  , DATASET=name
  function_2d, id0,radius_map, DATASET=name+',radius'

endforeach


return
end


