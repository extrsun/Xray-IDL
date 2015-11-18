;;; $Id: ml_powerlaw.pro 3972 2011-08-08 18:56:25Z psb6 $

;;; Tool to fit a powerlaw probability distribution to univaritate data (e.g. mass function, luminosity function).
;;;
;;; The assumed powerlaw probability distribution is Eq. 1 in Maschberger & Kroupa 2009:

;;;   p(x;alpha,x_min,x_max) = x^(-alpha) * (1-alpha)/(x_max^(1-alpha) - x_min^(1-alpha))

;;; This distribution must have a lower cutoff, LOW_CUTOFF, and can optionally have an upper cutoff, HIGH_CUTOFF.

;;; Supply a univariate data vector as "x_data".
;;; The unbiased maximum likelihood estimate for alpha (>0) is returned via the output parameter alpha_unbiased. 
   
;;; See Maschberger & Kroupa 2009 for derivation.
;;; \bibitem[Maschberger \& Kroupa(2009)]{2009MNRAS.395..931M} Maschberger, T., \& Kroupa, P.\ 2009, \mnras, 395, 931 


;;; Alpha values near 1.0 cause the likelihood to approach zero, and this code will fail.


FUNCTION ml_powerlaw_likelihood_derivative, alpha

COMMON ml_powerlaw, id1, id2, x_min, x_max, num_data, T

beta = 1D - alpha

; Eq. 9 in M&K.
return, num_data * (x_max^beta * alog(x_max) - x_min^beta * alog(x_min)) / (x_max^beta - x_min^beta)  $
        -  num_data / beta $
        - T 
end



PRO ml_powerlaw, x_data_p, LOW_CUTOFF=low_cutoff, HIGH_CUTOFF=high_cutoff, $
                 ALPHA=alpha_unbiased, NORMALIZATION=normalization, $
                 PLOT=plot, DEX_PER_BIN=dex_per_bin, NAME=name_p, $
                 LOG_BIN_CENTER=log_bin_center, OBSERVED_PER_BIN=observed_per_bin, PREDICTED_PER_BIN=predicted_per_bin

COMMON ml_powerlaw, id1, id2, x_min, x_max, num_data, T

if ~keyword_set(low_cutoff) || (low_cutoff LT 0) then message, 'ERROR: Lower-cutoff parameter must be > 0!'
if ~keyword_set(high_cutoff)                     then high_cutoff = !VALUES.F_INFINITY

if (total(~finite(x_data_p)) GT 0) then begin
  print, 'ERROR: some elements of the data vector are not finite!'
  retall
endif

if (min(x_data_p) LE 0) then begin
  print, 'ERROR: some elements of the data vector are not positive!'
  retall
endif

name = keyword_set(name_p) ? name_p : ''


;; --------------------------------------------------------------------------------------------------------
;; Define a set of bins of constant width in log base 10 space that span the model and the data.

; Choose the location of the first bin (x_min and log_x_min) such that the specified LOW_CUTOFF values lies on a bin boundary.
x_min = min(x_data_p) < low_cutoff 
x_max = max(x_data_p)
if finite(high_cutoff) then x_max >=  high_cutoff

log_x_min = floor( (alog10(x_min) - alog10(low_cutoff)) / dex_per_bin ) * dex_per_bin + alog10(low_cutoff)

; Form a histogram of the data across those bins.
observed_per_bin = histogram( alog10(x_data_p), MIN=log_x_min, BINSIZE=dex_per_bin )
;forprint, alog10(bin_edge), observed_per_bin

num_bins = n_elements(observed_per_bin)

log_bin_edge      = log_x_min + dex_per_bin * findgen(1+num_bins)
log_bin_center    = 0.5 * (log_bin_edge[1:*] + log_bin_edge)
bin_edge          = 10.^log_bin_edge


;; --------------------------------------------------------------------------------------------------------
;; Crop the data at the low and high cutoffs.
ind = where((low_cutoff LE x_data_p) AND (x_data_p LE high_cutoff), num_data)

print, num_data, alog10(low_cutoff), alog10(high_cutoff), F='(%"\n  %d data points analyzed: %0.2f <= alog10(x) <= %0.2f \n")'

if (num_data LE 1) then message, 'No data left after cropping.'

x_data = x_data_p[ind]
x_min  = low_cutoff
x_max  = high_cutoff
if (x_min GE x_max) then message, 'ERROR: Lower-cutoff parameter must be > upper-cutoff.!'


T = total(/DOUBLE, alog(x_data))

x_sorted = x_data[sort(x_data)]


if finite(x_max) then begin
  ; Truncated powerlaw model.
  
  alpha_ml = fx_root([2.0, 2.35, 2.5], 'ml_powerlaw_likelihood_derivative', /DOUBLE)

  ; Equation 12 in Maschberger & Kroupa 2009.
  alpha_unbiased = 1 + (alpha_ml - 1) *  num_data    / float(num_data - 2)
  
  ; Equation 1 in M&K
  beta = 1 - alpha_unbiased
  normalization = num_data * (beta / (x_max^beta - x_min^beta))
   
  ; Cumulative distribution of model, evaluated at each data poin via Eq. 2 in M&K.
  model_cdf = (x_sorted^beta - x_min^beta) / (x_max^beta - x_min^beta) 
  

  ; Integrate the model across those bins by subtracting the cumulative DF at the bin edges,  P(Xj) - P(Xi) using Equation 2 in M&K.
  predicted_per_bin = num_data * (bin_edge[1:*]^beta - bin_edge^beta) / (x_max^beta - x_min^beta)

endif else begin
  ; Infinite powerlaw model.
  
  ; Equation 10 in Maschberger & Kroupa 2009.
  alpha_ml = 1 + ( float(num_data) / (T - num_data*alog(x_min)) )
  ; Equation 11 in Maschberger & Kroupa 2009.
  alpha_unbiased = 1 + (alpha_ml - 1) * (num_data-1) / float(num_data)  

  ; Equation 3 in M&K
  beta = 1 - alpha_unbiased
  normalization = num_data * (-beta / x_min^beta)

  ; Cumulative distribution of model, evaluated at each data point via Eq. 4 in M&K.
  model_cdf = 1 - (x_sorted/x_min)^beta
  

  ; Integrate the model across those bins by subtracting the cumulative DF at the bin edges,  P(Xj) - P(Xi) using Equation 4 in M&K.
  predicted_per_bin = num_data * (-(bin_edge[1:*]/x_min)^beta + (bin_edge/x_min)^beta)  
endelse

help, normalization, alpha_ml, alpha_unbiased




if keyword_set(plot) then begin
  ; Show a "stabilized" P-P plot---cumulative distribution of model vs cumulative distribution of data.
  ; See S4.2 in Maschberger & Kroupa 2009.
  cum_distn_before_step = (  findgen(num_data)) / num_data
  cum_distn_after_step  = (1+findgen(num_data)) / num_data
  data_cdf              = 0.5*(cum_distn_before_step+cum_distn_after_step)
  
  function_1d, id1, (2/!PI)*asin(sqrt(model_cdf)), (2/!PI)*asin(sqrt(data_cdf)), PSYM=3, LINE=6, XTIT='S(P(Xi))', YTIT='S((i-0.5)/N)', DATASET=name+' Stabilized P-P plot'
  function_1d, id1, model_cdf, model_cdf, COLOR='red', DATASET=' goal line'

  
  ; Pad the histogram plot with zeros to make the first and last points look right.
  abscissa = [min(log_bin_center)-dex_per_bin, log_bin_center  , max(log_bin_center)+dex_per_bin]
  ordinate =                               [0, observed_per_bin, 0]
; marker_ordinate = 10.^(alog10(max(observed_per_bin))/2)
  marker_ordinate = 1.2*max(observed_per_bin)
  
  function_1d, id2, abscissa, ordinate, PSYM=10, DATASET=name+' observed' , Y_ERROR=[0, sqrt(observed_per_bin), 0], $
               PLOT_WINDOW_OPTIONS=string(alog10(low_cutoff), marker_ordinate, $
                                          finite(high_cutoff) ? alog10(high_cutoff) : 0, $
                                          finite(high_cutoff) ? marker_ordinate        : 0, $
                                          F='(%"SET_BIG_MARKER=[%0.2f,%0.2f], SET_SMALL_MARKER=[%0.2f,%0.2f]")')
  ; Plot model.
  function_1d, id2, log_bin_center, predicted_per_bin, PSYM= 4, DATASET=name+' model', LINE=6, XTIT='log X', YTIT='#/bin'
endif

return
end


;;; Generate powerlaw-distributed data and estimate alpha many times; plot the alpha estimates. 
; ml_powerlaw_simulate, 2.3, 300, 100, LOW_CUTOFF=10.^30.1, HIGH_CUTOFF=10.^31.5

PRO ml_powerlaw_simulate, alpha, num_data, num_simulations, LOW_CUTOFF=low_cutoff, HIGH_CUTOFF=high_cutoff

if ~keyword_set(high_cutoff) then message, 'You must supply a HIGH_CUTOFF in order to simulate data sets.'

if (alpha LT 1.01) then message, 'alpha must be > 1.01'


alpha_unbiased = fltarr(num_simulations)

for ii=0,n_elements(alpha_unbiased)-1 do begin
  RANDOMP, x_data, -alpha, num_data, RANGE_X = [low_cutoff,high_cutoff]
  
  ml_powerlaw, x_data, LOW_CUTOFF=low_cutoff, HIGH_CUTOFF=high_cutoff, ALPHA=this_alpha, NORM=norm, /PLOT, DEX_PER_BIN=0.1
  alpha_unbiased[ii] = this_alpha
endfor ; ii

dataset_1d, id2, alpha_unbiased, XTIT='estimated alpha'

print, alpha, median(alpha_unbiased), stddev(alpha_unbiased), F='(%"\nmodel alpha = %0.3f \nestimated alpha: median = %0.3f, stdev = %0.3f")'

return
end

