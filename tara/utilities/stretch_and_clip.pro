;; %W%
;; Patrick Broos, 2004

;; "data" is an array of "intensity" data with arbitrary scaling.

;; The "data" is scaled 
;; such that LOW_DATA->LOW_NORM and HIGH_DATA->HIGH_NORM.

;; LOW_DATA, HIGH_DATA (optional) are "signal levels", while 
;; LOW_NORM, HIGH_NORM (optional) are in [0,1].
;; For example, if you wish for pixels with a data of 1000 to be displayed with 1/2 of 
;; full brightness, then you would use LOW_DATA=1000, LOW_NORM=0.5 
;; (or HIGH_DATA=1000, HIGH_NORM=0.5).
;; Defaults are scaling over full range of data, i.e.
;; HIGH_DATA=max(data), LOW_DATA=min(data), HIGH_NORM=1, LOW_NORM=0.

;; The "scaled_data" output is normalized [0,1].

PRO stretch_and_clip, data, $
		LOW_DATA =low_data,  LOW_NORM =low_norm, $
		HIGH_DATA=high_data, HIGH_NORM=high_norm, $
		LOG=log_scaling, $
	       	scaled_data,  $
	       	GET_ZERO_DATA=get_zero_data, GET_FULL_DATA=get_full_data, $
	       	USE_CACHED_DATA=use_cached_data

;; A COMMON block is used to cache data-related vars to speed up interactive
;; brightness scaling in client.
COMMON stretch_and_clip, log_data


;; Use defaults or error check LOW_NORM & HIGH_NORM.
if (n_elements(low_norm)   EQ 0) then low_norm   = 0.0
if (n_elements(high_norm)  EQ 0) then high_norm  = 1.0

if (low_norm  GE high_norm)  then high_norm  = low_norm  + 1


;; Make sure desired data scaling range is well defined.
;  Discard non-positive LOW_DATA OR HIGH_DATA keywords when LOG scaling.
if (n_elements(low_data) EQ 1) then begin
  default_ld = 0
  if (NOT finite(low_data))                        then default_ld = 1
  if keyword_set(log_scaling) AND (low_data  LE 0) then default_ld = 1
endif else default_ld = 1

if (n_elements(high_data) EQ 1) then begin
  default_hd = 0
  if (NOT finite(high_data))                        then default_hd = 1
  if keyword_set(log_scaling) AND (high_data  LE 0) then default_hd = 1
endif else default_hd = 1


;  Use defaults when needed.
if default_ld then begin
  low_data  = min(data, /NAN)
  if (NOT finite(low_data)) then low_data=0
endif

if default_hd then begin
  high_data = max(data, /NAN)
  if (NOT finite(high_data)) then high_data=0
endif


;  Make sure low_data < high_data.
if (low_data GE high_data) then high_data = low_data + 1


if keyword_set(log_scaling) then begin
  log_low_data  = alog10(low_data)
  log_high_data = alog10(high_data)

  if NOT keyword_set(use_cached_data) then log_data = alog10(data)

  ;; Stretch "data" such that low_data->low_norm and high_data->high_norm.
  slope        = (high_norm - low_norm) / (log_high_data - log_low_data)
  scaled_data = 0 > (low_norm +  slope * (log_data - log_low_data) ) < 1
  
  ;; For client, compute datas corresponding to zero and full scaling.
  get_zero_data = log_low_data - (low_norm/slope)
  get_full_data = get_zero_data + (1/slope)
  
  get_zero_data = 10.^get_zero_data
  get_full_data = 10.^get_full_data

endif else begin
  ;; Stretch "data" such that low_data->low_norm and high_data->high_norm.
  slope        = (high_norm - low_norm) / (high_data - low_data)
  scaled_data = 0 > (low_norm +  slope * (data - low_data) ) < 1
  
  ;; For client, compute datas corresponding to zero and full scaling.
  get_zero_data = low_data - (low_norm/slope)
  get_full_data = get_zero_data + (1/slope)
endelse

return
end



