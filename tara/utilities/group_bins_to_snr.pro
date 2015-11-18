;;; $Id: group_bins_to_snr.pro 4290 2012-06-27 17:07:05Z psb6 $
;;; Group the bins of a histogram to achieve a specified signal-to-noise ratio.
;;;
;;; See calls in acis_extract.pro to understand usage.




FUNCTION snr_for_this_group, group_start, group_stop, src_observed_counts, bkg_observed_counts, bkg_counts_in_src_region, BACKSCAL, group_without_background

; Calculate the S/N of the current group using Gehrels upper errors.
SRC_CNTS   = total(/INT, src_observed_counts     [group_start:group_stop])
BKG_CNTS   = total(/INT, bkg_observed_counts     [group_start:group_stop])
scaled_bkg = total(      bkg_counts_in_src_region[group_start:group_stop])

net_counts = (SRC_CNTS - scaled_bkg)>0

src_cnts_sigma_up = 1 + sqrt(SRC_CNTS + 0.75)
bkg_cnts_sigma_up = 1 + sqrt(BKG_CNTS + 0.75)
NET_CNTS_SIGMA_UP = sqrt(src_cnts_sigma_up^2 + (bkg_cnts_sigma_up/BACKSCAL)^2)

snr = net_counts / NET_CNTS_SIGMA_UP

; This option allows the observer to obtain a specific number of SRC_CNTS per group (the old style of grouping used in AE).
if keyword_set(group_without_background) then snr = SRC_CNTS / src_cnts_sigma_up 
    
return, snr
end



PRO group_bins_to_snr, src_observed_counts, bkg_observed_counts, bkg_counts_in_src_region,$
                       GROUP_WITHOUT_BACKGROUND=group_without_background,$
                       START_INDEX=start_index, STOP_INDEX=stop_index, $
                       SNR_RANGE=snr_range, NUM_GROUPS_RANGE=num_groups_range,  $
                       VERBOSE=verbose, $
                       current_snr_threshold, group_codes

num_channels = n_elements(src_observed_counts)

if keyword_set(group_without_background) then begin
  bkg_observed_counts      = fltarr(num_channels)
  bkg_counts_in_src_region = fltarr(num_channels)
endif

; The algorithm used for defining groups requires that non-trivial first and last groups are explicitly defined 
; by the caller via parameters START_INDEX>0 and STOP_INDEX<(num_channels-1).
; The last bin in the first group is at START_INDEX-1; the first bin in the second group is at START_INDEX.
; The last bin in the next-to-last group is at STOP_INDEX; the first bin in the last group is at STOP_INDEX+1.
; This is all very convenient when grouping spectra since the energy range over which a fit is performed in XSPEC
;  can be conveniently specified by "ignoring" the first and last groups.
;
; When grouping other data (e.g. light curves) however, we may not wish to explicitly define the first and last
; groups, but rather wish to let the algorithm determine them like all the others.
; In this case, we have to do some trickery---pad the data and recursively call group_bins_to_snr.
pad_on_left  = (n_elements(start_index) NE 1)
pad_on_right = (n_elements(stop_index)  NE 1)

if pad_on_left || pad_on_right then begin
  padded_src_observed_counts      = src_observed_counts
  padded_bkg_observed_counts      = bkg_observed_counts
  padded_bkg_counts_in_src_region = bkg_counts_in_src_region

  if pad_on_left then begin
    padded_src_observed_counts      = [0,padded_src_observed_counts]
    padded_bkg_observed_counts      = [0,padded_bkg_observed_counts]
    padded_bkg_counts_in_src_region = [0,padded_bkg_counts_in_src_region]
    start_index = 1
  endif
  
  if pad_on_right then begin
    stop_index = n_elements(padded_src_observed_counts)-1
    padded_src_observed_counts      = [padded_src_observed_counts     ,0]
    padded_bkg_observed_counts      = [padded_bkg_observed_counts     ,0]
    padded_bkg_counts_in_src_region = [padded_bkg_counts_in_src_region,0]
  endif 
  
  ; Make a recursive call using these padded vectors.
  ; Since the padded vectors will produce two null groups, we need to increase the NUM_GROUPS_RANGE values passed by two!
  
;help, padded_src_observed_counts, padded_bkg_observed_counts, padded_bkg_counts_in_src_region, start_index, stop_index

  group_bins_to_snr, padded_src_observed_counts, padded_bkg_observed_counts, padded_bkg_counts_in_src_region,$
                       GROUP_WITHOUT_BACKGROUND=group_without_background,$
                       START_INDEX=start_index, STOP_INDEX=stop_index, $
                       SNR_RANGE=snr_range, NUM_GROUPS_RANGE=2+num_groups_range,  $
                       VERBOSE=verbose, $
                       current_snr_threshold, padded_group_codes
                     
  group_codes = padded_group_codes
  if pad_on_left  then group_codes = group_codes[1:*]
  if pad_on_right then group_codes = group_codes[0:n_elements(group_codes)-2]
  return
endif


;; Define a typical BACKSCAL value for use in error propagation later.
BACKSCAL = median(bkg_observed_counts / bkg_counts_in_src_region) 
if (NOT finite(BACKSCAL)) then BACKSCAL = 1E20
                                      
; Start SNR search at the upper value supplied by caller.
preferred_snr_threshold = snr_range[1]
current_snr_threshold   = preferred_snr_threshold

; Initialize an allowed range of SNR values; the algorithm will shrink this range as it progresses
; We keep track of whether each endpoint of the range is an acceptable result.
snr_lower_bound       = snr_range[0]
snr_upper_bound       = !VALUES.F_INFINITY
snr_lower_bound_is_acceptable = 0
snr_upper_bound_is_acceptable = 0

; Error check the NUM_GROUPS_RANGE values supplied by caller.
; A lower bound less than 1 has no meaning.
; To form an interval, the upper bound cannot be smaller than the lower bound.
num_groups_lower_bound = num_groups_range[0] > 1
num_groups_upper_bound = num_groups_range[1] > num_groups_lower_bound 


; Compute all the channel positions where a group beginning or group end is allowed.
; If a group boundary needs to fall in a run of zeros in src_observed_counts, we wish to divide that run of zeros evenly between the two groups, so as not to impart an upward bias on the flux of the group we're ending or impart a downward bias on the flux of the group we're beginning.
start_is_allowed = bytarr(num_channels)
stop_is_allowed  = bytarr(num_channels)

start_is_allowed[0]              = 1
stop_is_allowed [num_channels-1] = 1

nonzero_indexes = where(src_observed_counts GT 0, count)
if (count GE 2) then begin
  ind = (nonzero_indexes + nonzero_indexes[1:*]) / 2
  stop_is_allowed [ind    ] = 1
  start_is_allowed[ind + 1] = 1
endif


;; ------------------------------------------------------------------------
;; Search for a SNR theshold value that meets all our objectives.
is_last_pass = 0
fat_middle_group_exists = 1
while (1) do begin
  ; Initial groups:        first        last
  gstart_list    = [             0, stop_index+1  ]
  gstop_list     = [ start_index-1, num_channels-1]
  
  ungrouped_start   = start_index
  ungrouped_stop    = stop_index
  

  while (1) do begin
    ; Search the current "ungrouped interval" for TWO valid groups, working from both ends.
    
    ; Begin with empty groups, and expand until SNR threshold is met or proto-groups collide.
    left_start = ungrouped_start
    right_stop = ungrouped_stop
  
    left_stop   = left_start-1
    right_start = right_stop+1
    
    
    ; Expand left group until reaching a valid group boundary where SNR criteria is met.
    repeat begin
      while ( ~stop_is_allowed[++left_stop]) do dum=0
      
      ; Stop if group expanded beyond the "ungrouped interval".
      abort = (left_stop GT ungrouped_stop)
  
      if abort then break
       
      this_snr = snr_for_this_group(left_start,left_stop, $
                                    src_observed_counts, bkg_observed_counts, bkg_counts_in_src_region, BACKSCAL, keyword_set(group_without_background))
                                  
    endrep until (this_snr GT current_snr_threshold)
    
    if abort then begin
      ; The "ungrouped interval" does not contain even one valid group.
      if (n_elements(gstart_list) EQ 2) then begin
        ; No in-band groups have yet been defined, so our "ungrouped interval" becomes a third group.
        gstart_list = [ungrouped_start, gstart_list]
         gstop_list = [ungrouped_stop,  gstop_list]
      endif else begin
        ; Combine the "ungrouped interval" with the group at the head of the list.
        if            (ungrouped_start EQ gstop_list[0]+1) then begin
          ; Append to group on the left.
           gstop_list[0] = ungrouped_stop
        endif else if (ungrouped_stop EQ gstart_list[0]-1) then begin
          ; Append to group on the right
           gstart_list[0] = ungrouped_start
        endif else message, 'BUG!'
      endelse
      
      break    
    endif
    

    ; Expand right group until reaching a valid group boundary where SNR criteria is met.
    repeat begin
      while (~start_is_allowed[--right_start]) do dum=0

      ; Stop if the "right group" has encroached on the "left group" just constructed above.
      abort = (left_stop GE right_start)
  
      if abort then break
      
      this_snr = snr_for_this_group(right_start,right_stop, $
                                    src_observed_counts, bkg_observed_counts, bkg_counts_in_src_region, BACKSCAL, keyword_set(group_without_background))
                                  
    endrep until (this_snr GT current_snr_threshold)
    
    if abort then begin
      ; If we make it here, then the left group search was successful, but the right group search failed.
      ; Expand the left group to cover the ungrouped interval, save it, and then abort the search.
      gstart_list = [left_start    ,  gstart_list]
       gstop_list = [ungrouped_stop,   gstop_list]
      
      fat_middle_group_exists = (ungrouped_stop GT left_stop)
      break
    endif
   
    
    ; We have successfully found two valid groups lying in the current "ungrouped interval".
    ; Push these new groups onto our list of groups, and update the "ungrouped interval".
    gstart_list = [left_start, right_start, gstart_list]
     gstop_list = [left_stop , right_stop ,  gstop_list]

    ungrouped_start   = left_stop  +1
    ungrouped_stop    = right_start-1

    if (ungrouped_stop LT ungrouped_start) then begin
      ; There is no remaining "ungrouped interval", so abort the search.
      fat_middle_group_exists = 0
      break
    endif
  endwhile  ; Processing the "ungrouped interval".
  
  
  if keyword_set(verbose) && fat_middle_group_exists then begin
    this_snr = snr_for_this_group(gstart_list[0], gstop_list[0], $
                                  src_observed_counts, bkg_observed_counts, bkg_counts_in_src_region, BACKSCAL, keyword_set(group_without_background))
    print, left_start, ungrouped_stop, this_snr, F='(%"''Fat'' group in the middle, [%d:%d], has SNR of %0.2f.")'       
  endif
    
  
  num_groups = n_elements(gstart_list)
  if is_last_pass then begin
    if keyword_set(verbose) then print, current_snr_threshold, num_groups, F='(%"SNR threshold %0.2f produced %d groups; aborting the search")' 

    break
  endif


  
  ;; ------------------------------------------------------------------------
  ;; Now, examine the number of groups produced by our current SNR threshold (current_snr_threshold) and deduce what adjustment
  ;; to the SNR range [snr_lower_bound, snr_upper_bound] is appropriate.
  if            (num_groups LT num_groups_lower_bound) then begin
    ; Since this trial SNR value produced too few groups, it should serve as the snr_upper_bound.
    snr_upper_bound               = current_snr_threshold
    snr_upper_bound_is_acceptable = 0
    
  endif else if (num_groups GT num_groups_upper_bound) then begin
    ; Since this trial SNR value produced too many groups, it should serve as the snr_lower_bound.
    snr_lower_bound               = current_snr_threshold
    snr_lower_bound_is_acceptable = 0
  
  endif else begin
    ; The trial SNR value produced an acceptable number of groups.
    if            (current_snr_threshold EQ preferred_snr_threshold) then begin
      ; The caller's preferred value for SNR is acceptable, so stop the search.
      break
    endif else if (current_snr_threshold GT preferred_snr_threshold) then begin
      ; This trial SNR value forms an acceptable upper bound.
      snr_upper_bound               = current_snr_threshold
      snr_upper_bound_is_acceptable = 1
      
    endif else if (current_snr_threshold LT preferred_snr_threshold) then begin
      ; This trial SNR value forms an acceptable lower bound.
      snr_lower_bound               = current_snr_threshold
      snr_lower_bound_is_acceptable = 1
    
    endif else message, 'BUG!'
  endelse ; trial SNR value is acceptable.
  
  if keyword_set(verbose) then print, current_snr_threshold, num_groups, snr_lower_bound, snr_upper_bound, F='(%"SNR threshold %0.2f produced %d groups; SNR search range is now [%0.2f, %0.2f]")' 
  
  ; Our SNR search range has been updated, so choose a new trial value that falls in the current search range.
  if ~finite(snr_upper_bound) then begin
    ; We have not yet established an upper bound on SNR, so just double the current threshold.
    current_snr_threshold *= 2
  endif else begin
    if ((snr_upper_bound - snr_lower_bound) LT 0.1) then begin
      ; The search range is very small, so choose one of the boundaries and re-group one last time.
      current_snr_threshold = (snr_upper_bound_is_acceptable) ? snr_upper_bound : snr_lower_bound
      is_last_pass = 1
    
    endif else begin
      ; Try an SNR target at the middle of the search range, which has just been revised.
      current_snr_threshold = 0.5 * (snr_lower_bound + snr_upper_bound)
      endelse
  endelse ; finite(snr_upper_bound) 
  
endwhile ; Search for a SNR theshold value.


; Build a "group codes" vector following the HEASARC convention.

; The HEASARC grouping codes are defined from a left-to-right perspective.  A value of "1" is the left boundary (inclusive) of a group.
group_codes = replicate(-1,num_channels)
group_codes[gstart_list] = 1


;forprint, indgen(num_channels), src_observed_counts, start_is_allowed, stop_is_allowed, group_codes

return
end ; group_bins_to_snr

