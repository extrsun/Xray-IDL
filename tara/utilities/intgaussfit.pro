;+
;========================================================================
;;;
;;; FILE NAME:    $Id: intgaussfit.pro 782 1999-04-30 10:40:16Z patb $
;;;
;;; DESCRIPTION:  Fitting Routine for an Integrated Gaussian Plus Quadratic
;;;               Model
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;;; This routine fits a histogram dataset to a model that consists of an
;;; integrated probability distribution that has the form of a gaussian 
;;; plus an optional polynomial of order <= 2.
;;; The integrations are performed across bins of size BINSIZE whose centers
;;; are located at the positions CENTERS.
;;;
;;; The algorithm used to guess initial values of the fit parameters 
;;; assumes your data consists of a POSITIVE gaussian with a very small
;;; polynomial background.
;;;
;;; The input parameters consist of a vector of histogram bin CENTERS, a 
;;; vector of bin COUNTS, and a bin size parameter, BINSIZE, which defaults 
;;; to 1.0 if not supplied.
;;; A vector of weights can be supplied via the WEIGHTS keyword -- the 
;;; default is equal weighting.
;;;
;;; The gaussian part of the model is described by the return keywords 
;;; GAIN, MEAN, and SIGMA.
;;; The polynomial part of the model is described by the return keywords
;;; CONSTANT, LINEAR, and QUADRATIC.  The order of the polynomial used in 
;;; the model is determined by the integer parameter ORDER, which can have
;;; the following values:
;;;	-1: no polynomial used
;;;	 0: constant term only (the DEFAULT)
;;;	 1: constant plus linear terms
;;;	 2: constant plus linear plus quadratic terms
;;;
;;; Thus, the full model has the form shown below, where "cen" is a bin center
;;; value and "B" is the binsize.  The linear and quadratic terms are optional.
;;;
;;;                                  (cen + B/2)
;;;                                       --
;;;                           1           |      -(t-mean)^2 
;;; f(cen) = gain * { ------------------  | exp[ ----------- ] dt } +
;;;                   sigma * sqrt(2*pi)  |      2 * sigma^2
;;;                                      --
;;;                                  (cen - B/2)
;;;
;;;          (cen + B/2)
;;;               --
;;;               | [constant  +  linear * t  +  quadratic * t^2] dt
;;;              --
;;;          (cen - B/2)
;;;
;;;
;;; Using the properties of the gaussian function, we can evaluate this
;;; integral using the IDL function GAUSSINT.  The model reduces to:
;;;
;;;                            cen + B/2 - mean     
;;; f(cen) = gain * {GAUSSINT[ ---------------- ] - 
;;;                                  sigma
;;;                                              cen - B/2 - mean     
;;;                                    GAUSSINT[ ---------------- ]} +
;;;                                                    sigma
;;;
;;;          (cen + B/2)
;;;               --
;;;               | [constant  +  linear * t  +  quadratic * t^2] dt
;;;              --
;;;          (cen - B/2)
;;;
;;;
;;; If the keyword ERRORS is set, then the output parameters GAIN,
;;; MEAN, SIGMA, CONSTANT, LINEAR, & QUADRATIC are returned as two-elements 
;;; vectors instead of scalars -- the first element is the parameter value
;;; and the second element is the estimated parameter standard deviation.
;;;
;;; The output parameter CHI2 returns the chi-squared value of the fit.
;;; If WEIGHTS are supplied, then the output parameter REDUCED_CHI2 returns 
;;; the reduced chi-squared value of the fit.
;;;
;;; An example usage with ORDER = 2 is shown below.
;;;
;;; min     = -3.0  &   binsize =  0.4 
;;;
;;; counts  = histogram( randomn(seed,1000000), MIN=min, BINSIZE=binsize )
;;; centers = binsize * lindgen( n_elements(counts) ) + min + binsize/2.0
;;; counts  = counts + 100000 + 5000*centers + 2000*centers*centers
;;;
;;; fit = intgaussfit( centers, counts, BINSIZE=binsize, ORDER=2, $
;;;		       GAIN=gain, MEAN=mean, SIGMA=sigma, $
;;;		       CONSTANT=constant, LINEAR=linear, QUADRATIC=quadratic)
;;;
;;; help, gain, mean, sigma, constant, linear, quadratic
;;;
;;; Result is : 
;;; GAIN            FLOAT     =       997669.
;;; MEAN            FLOAT     =  -0.000109744
;;; SIGMA           FLOAT     =      0.999091
;;; CONSTANT        FLOAT     =       250431.
;;; LINEAR          FLOAT     =       12545.3
;;; QUADRATIC       FLOAT     =       4961.82
;;;
;-
;==========================================================================
;;; The model function.
;;; The vector params is [gain, mean, sigma, constant, linear, quadratic]
;;; The constant, linear, and quadratic terms may be omitted.
;==========================================================================
PRO intgaussfit_model, centers, params, F

COMMON INTGAUSSFIT, binsize

gain  = params(0)
mean  = params(1)
sigma = params(2)

f = gain * ( gaussint( (centers + binsize/2.0 - mean) / sigma ) - $
	     gaussint( (centers - binsize/2.0 - mean) / sigma ) )


if (n_elements(params) GE 4) then begin
  constant  = params(3)
          F = F + (constant * binsize)

  ;This is really:                |(centers + binsize/2)
  ;                 [constant * t]|
  ;                               |(centers - binsize/2)
endif

if (n_elements(params) GE 5) then begin
  linear    = params(4)
          F = F + (linear * binsize * centers)

  ;This is really:                      |(centers + binsize/2)
  ;                 [(1/2) * linear * t^2]|
  ;                                     |(centers - binsize/2)
endif

if (n_elements(params) GE 6) then begin
  quadratic = params(5)
          F = F + (quadratic * binsize * (centers^2 + binsize^2/12.))

  ;This is really:                      |(centers + binsize/2)
  ;                 [(1/3) * quadratic * t^3]|
  ;                                     |(centers - binsize/2)
endif

end


;==========================================================================
FUNCTION intgaussfit, centers, counts, BINSIZE=binsize_kwrd, $
			WEIGHTS=weights, $
			ORDER=order, ERRORS=errors, $
			CHI2=chi2, REDUCED_CHI2=reduced_chi2, $
			GAIN=gain, MEAN=mean, SIGMA=sigma, $
			CONSTANT=constant, LINEAR=linear, QUADRATIC=quadratic

;; Since we cannot pass fixed parameters of the model into the function
;; intgaussfit_model, we'll have to resort to common blocks.  Yuk!
COMMON INTGAUSSFIT, binsize

;; Handle default parameters.
if (not keyword_set(order))        then order   = 0
if (not keyword_set(binsize_kwrd)) then binsize = 1.0 $
				   else binsize = float( binsize_kwrd )

if (order LT -1 OR order GT 2) then message, 'parameter ORDER not valid'

;; Find some initial values for all the fit parameters.
max_counts = max( counts, i_max, MIN=min_counts )
mean       = centers( i_max )

; Search for the "half max" point so we can estimate sigma.
half_max       = (max_counts+min_counts)/2.0
half_max_found = 0

; First, look leftward for the most distant data point that is >= half_max.
index = where( (centers LE mean) AND (counts GE half_max), hits )
i_half_max = index(0) - 1 

; If we found a data point, and it is NOT at the left edge of the dataset,
; then it's left-hand neighbor will be below the half_max level.
if ((hits GT 0) AND (i_half_max GE 0)) then begin

  half_max_found = 1

endif else begin
  ; If the leftware search for the half_max point failed, then let's try
  ; looking rightward.
  index = where( (centers GE mean) AND (counts GE half_max), hits )
  i_half_max = 1 + index( n_elements(index) - 1 )

  ; If we found a data point, and it is NOT at the right edge of the dataset,
  ; then it's right-hand neighbor will be below the half_max level.
  if ((hits GT 0) AND (i_half_max LT n_elements(centers))) then begin
    half_max_found = 1
  endif
endelse
help, i_max, i_half_max, half_max

if (half_max_found EQ 0) then begin
  message, 'ERROR: cannot find half_max point!'
  return, 0
endif

sigma = abs( centers( i_max ) - centers( i_half_max ) ) * 2 / 2.354

gain = max_counts * sigma * sqrt(2*!PI) / binsize

case order of
 -1: params = [gain, mean, sigma]
  0: params = [gain, mean, sigma, 0.0]
  1: params = [gain, mean, sigma, 0.0, 0.0]
  2: params = [gain, mean, sigma, 0.0, 0.0, 0.0]
endcase

print
print, 'INTEGRATED GAUSSIAN FIT'
print, 'Binsize = ', binsize
;print, 'Estimated initial values for parameters:'
;print, params, f='(6F15.5,/)'


;; Perform the final fit.

if keyword_set( weights ) then begin
  wght = weights
  print, 'performing weighted fit ...'
endif else begin
  wght = replicate( 1.0, n_elements( centers ) )
  print, 'performing non-weighted fit ...'
endelse

;;; Run a double-precision fit because curvefit has to estimate partial
;;; derivatives.
params = double(params)
;;; !!! As of IDL 5.2, the CURVEFIT keyword CHISQ returnes _reduced_ chi^2,
;;; !!! which is very misleading!
fit = curvefit( centers, counts, wght, params, param_errors, $
		CHISQ=reduced_chi2, $
		FUNCTION_NAME='intgaussfit_model', /NODERIVATIVE, $
		TOL=1E-10, ITER=iter )

chi2 = reduced_chi2 * (n_elements(counts) - n_elements(params))

;  print, iter, reduced_chi2, $
;	  f='(I0, " iterations performed; reduced chi-squared = ", G10.4)'
;  print, chi2, f='("chi-squared = ", G10.4)'


;; Extract parameter return values.
params       = [params,       [0,0,0]]
param_errors = [param_errors, [0,0,0]]

if keyword_set( errors ) then begin
  gain      = [params(0), param_errors(0)]
  mean      = [params(1), param_errors(1)]
  sigma     = [params(2), param_errors(2)]
  constant  = [params(3), param_errors(3)]
  linear    = [params(4), param_errors(4)]
  quadratic = [params(5), param_errors(5)]
endif else begin
  gain      = params(0)
  mean      = params(1)
  sigma     = params(2)
  constant  = params(3)
  linear    = params(4)
  quadratic = params(5)
endelse

;;; Return single, not double result.
return, float(fit)
end

