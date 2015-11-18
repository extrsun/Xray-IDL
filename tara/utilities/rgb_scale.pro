;; $Id: rgb_scale.pro 2434 2006-04-25 15:18:26Z patb $
;; Patrick Broos, 2003

;; *_data are images containing red, green, & blue data with arbitrary scaling.

;; BACKGROUNDS (optional) is 3-vector containing background level in each data image;
;; default is 0.

;; GAINS (optional) is 3-vector containing relative strength each data image should have.
;; Provides adjustment of "color" in RGB image produced.  Default is 1.0.

;; The "signal" in each band is computed as gains[?] * (?_data - backgrounds[?]).
;; The "value" (from HSV color model) = max(red_signal,grn_signal,blu_signal) is scaled 
;; such that LOW_VALUE->LOW_NORM and HIGH_VALUE->HIGH_NORM.

;; LOW_VALUE, HIGH_VALUE (optional) are "signal levels", while 
;; LOW_NORM, HIGH_NORM (optional) are in [0,1].
;; For example, if you wish for pixels with a value of 1000 to be displayed with 1/2 of 
;; full brightness, then you would use LOW_VALUE=1000, LOW_NORM=0.5 
;; (or HIGH_VALUE=1000, HIGH_NORM=0.5).
;; Defaults are scaling over full range of data, i.e.
;; HIGH_VALUE=max(value), LOW_VALUE=min(value), HIGH_NORM=1, LOW_NORM=0.

;; The red_channel, grn_channel, blu_channel outputs are normalized [0,1].

;; If /DO_PLOT then image will be displayed in current window.  

;; To test rgb_scale in X window:
;; color_manager & window
;; dim = 16L
;; dim = 256L
;; red=lindgen(dim,dim) & grn=transpose(red)
;; x=lindgen(dim) & y=x & make_2d, x, y
;; blu=sqrt(x^2 + y^2)  & blu=dim^2 * blu/max(blu)
;; rgb_scale, red, grn, blu, /DO_PLOT
;; rgb_scale, red, red-red, red-red, /DO_PLOT

;; Typical usage for printing:
;;  color_manager, /PS_TRUE, XSIZE=7, YSIZE=7, YOFFSET=2, /INCHES, BITS=8, FILE='foo.ps'
;;  rgb_scale, ..., /DO_PLOT
;;  device, /CLOSE

PRO rgb_scale, 	red_data, grn_data, blu_data, BACKGROUNDS=bkg_kywd, GAINS=gain_kywd, $
		LOW_VALUE =low_value,  LOW_NORM =low_norm, $
		HIGH_VALUE=high_value, HIGH_NORM=high_norm, $
		LOG=log_scaling, INVERT=invert, $
	       	red_channel, grn_channel, blu_channel, DO_PLOT=do_plot, $
	       	GET_ZERO_VALUE=get_zero_value, GET_FULL_VALUE=get_full_value, $
	       	GET_LOW_VALUE=get_low_value,   GET_HIGH_VALUE=get_high_value, $
	       	USE_CACHED_VALUES=use_cached_values

;; A COMMON block is used to cache value-related vars to speed up brightness scaling in client.
COMMON rgb_scale, value, low_value_p, high_value_p, red_norm, grn_norm, blu_norm


if NOT keyword_set(invert) then invert=0

;; Use defaults or error check LOW_NORM & HIGH_NORM.
if (n_elements(low_norm)   EQ 0) then low_norm   = 0.0
if (n_elements(high_norm)  EQ 0) then high_norm  = 1.0

if (low_norm  GE high_norm)  then high_norm  = low_norm  + 1


;; Used cached computations of value array, low_value, & high_value limits if requested.
if keyword_set(use_cached_values) then GOTO, VALUE_VARS_DEFINED


;; Handle BACKGROUNDS & GAINS keywords.
if keyword_set(bkg_kywd) then begin
  backgrounds = 1.0 * bkg_kywd
endif else begin
  backgrounds = [0.,0.,0.]
endelse

if keyword_set(gain_kywd) then begin
  gains = 1.0 * gain_kywd
endif else begin
  gains = [1.,1.,1.]
endelse

;; Subtract backgrounds and apply gains to get a non-negative signal in each band.
red_signal = 0 > (gains[0] * (red_data - backgrounds[0]))
grn_signal = 0 > (gains[1] * (grn_data - backgrounds[1]))
blu_signal = 0 > (gains[2] * (blu_data - backgrounds[2]))


;; Compute the "value" (brightness) component of the HSV color model.
;; Then scale RGB signals by "value" to get vectors touching boundary of unit cube 
;; with unchanged hue, saturation.

value = red_signal > grn_signal > blu_signal

red_norm = red_signal/value
grn_norm = grn_signal/value
blu_norm = blu_signal/value


;; Fix pixels with zero (signal/value) = NaN.
zero = where( value LE 0, num_zero, COMPLEMENT=pos, NCOMP=num_pos )
if (num_zero GT 0) then begin
  red_norm[zero] = 0
  grn_norm[zero] = 0
  blu_norm[zero] = 0

  if keyword_set(log_scaling) then begin
    if (num_pos GT 0) then $
      value[zero] = min(value[pos]) / 10. $
    else $
      value[zero] = 0.0
  endif
endif


;; Make sure desired value scaling range is well defined.
;  Discard non-positive LOW_VALUE OR HIGH_VALUE keywords when LOG scaling.
if (n_elements(low_value) EQ 1) then begin
  default_lv = 0
  if (NOT finite(low_value))                        then default_lv = 1
  if keyword_set(log_scaling) AND (low_value  LE 0) then default_lv = 1
endif else default_lv = 1

if (n_elements(high_value) EQ 1) then begin
  default_hv = 0
  if (NOT finite(high_value))                        then default_hv = 1
  if keyword_set(log_scaling) AND (high_value  LE 0) then default_hv = 1
endif else default_hv = 1


;  Use defaults when needed.
if default_lv then low_value  = min(value, /NAN)
if default_hv then high_value = max(value, /NAN)

;  Make sure low_value < high_value.
if (low_value GE high_value) then high_value = low_value + 1

get_low_value  = low_value
get_high_value = high_value

;; Handle LOG option.
if keyword_set(log_scaling) then begin
  low_value_p  = alog10(low_value)
  high_value_p = alog10(high_value)
  value      = alog10(value)
endif else begin
  low_value_p  = low_value
  high_value_p = high_value
endelse


VALUE_VARS_DEFINED:

;; Scale the "value" such that low_value_p->low_norm and high_value_p->high_norm.
slope = (high_norm - low_norm) / (high_value_p - low_value_p)
scaled_value = 0 > (low_norm +  slope * (value - low_value_p) ) < 1

;; For client, compute values corresponding to zero and full scaling.
get_zero_value = low_value_p - (low_norm/slope)
get_full_value = get_zero_value + (1/slope)
if keyword_set(log_scaling) then begin
  get_zero_value = 10.^get_zero_value
  get_full_value = 10.^get_full_value
endif

;; Multiply our RGB_norm vectors (which have value=1.0) by scaled_value to get 
;; RGB channels ([0,1]) with desired brightness and original hue, saturation.
red_channel = red_norm * scaled_value
grn_channel = grn_norm * scaled_value
blu_channel = blu_norm * scaled_value


;; If requested "invert" to get white where no signal.
;; We convert to Hue-Lightness-Saturation model and invert Lightness.
;; Equations derived from http://astronomy.swin.edu.au/~pbourke/colour/hsl/index.html
;; by Paul Bourke, June 2000.
;; The clear way to code this would be:
;;  compute lightness
;;  invert: lightness = 1-lightness
;;  red_channel = (1-L)*(2*red_channel) + 2*L - 1
;;
;; But, we'll code for efficiency instead.

if (invert EQ 1) then begin
  ; Compute inverted lightness
  one_minus_lightness = ((red_channel > grn_channel > blu_channel) + $
  			 (red_channel < grn_channel < blu_channel)) / 2.0
  lightness           = 1.0 - one_minus_lightness
  
  ind_less = where(lightness LT 0.5, num_less, COMPLEMENT=ind_more, NCOMP=num_more)
  if (num_less GT 0) then begin
    scale = 2 * lightness[ind_less] 
    red_channel[ind_less] = scale * red_channel[ind_less]
    grn_channel[ind_less] = scale * grn_channel[ind_less]
    blu_channel[ind_less] = scale * blu_channel[ind_less]
  endif
   			 
  if (num_more GT 0) then begin
    scale  =  2 * one_minus_lightness[ind_more] 
    offset = (2 *           lightness[ind_more]) - 1
    red_channel[ind_more] = scale * red_channel[ind_more] + offset
    grn_channel[ind_more] = scale * grn_channel[ind_more] + offset
    blu_channel[ind_more] = scale * blu_channel[ind_more] + offset
  endif  

  ; We could have color_convert to HLS, then invert L, then color_convert to RGB
  ; but since color_convert requires integer RGB the quantization effects would be bad.
endif


if NOT keyword_set(do_plot) then return

;; Finally we scale up the channel values to the range of the device.
color_manager, NCOLORS=ncolors, DECOMPOSED=decomposed

; Usually we quantize to NCOLORS (from color_manager) levels, but if color_quan() 
; is going to be called below, need to quantize to 256 levels
num_levels = ((!D.NAME EQ 'PS') OR decomposed) ? ncolors : 256
;help, num_levels

red_img = floor( num_levels * red_channel ) < (num_levels-1)
grn_img = floor( num_levels * grn_channel ) < (num_levels-1)
blu_img = floor( num_levels * blu_channel ) < (num_levels-1)

if (!D.NAME EQ 'PS') then begin
  ;; Note that the PostScript device does not have "channels" -- you must
  ;; use the TRUE keyword instead.
  tv, [[[red_img]], [[grn_img]], [[blu_img]]], TRUE=3
  
endif else begin
  
  if (decomposed) then begin
    ; We expect the color tables to be simple ramps.
    tv, red_img, CHAN=1
    tv, grn_img, CHAN=2
    tv, blu_img, CHAN=3
  endif else begin
    ;; Quantize the color and display.
    color_image = color_quan( red_img, grn_img, blu_img, GET_TRANS=trans, $
  			      rtable, gtable, btable, COLORS=ncolors)
    tvlct, rtable, gtable, btable
    tv, color_image
    print, 'WARNING, color is quantized due to 8 bit graphics system.'
  endelse
endelse

return
end





