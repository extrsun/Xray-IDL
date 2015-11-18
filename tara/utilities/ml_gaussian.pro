;+
;========================================================================
;;;
;;; Maximum Likelihood Fitting to Gaussian Model: $Id: ml_gaussian.pro 1864 2003-10-02 11:15:12Z patb $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool may be used to directly apply the Maximum Likelihood
;;; Method to model the probability distribution of a univariate dataset
;;; as a sum of gaussians plus a polynomial.   
;;; See Chapter 10 of Bevington.
;;; 
;;; If an empirical distribution function for the dataset is supplied, 
;;; the Kolmogorov-Smirnov statistic is computed to characterize the
;;; goodness-of-fit.  See Section 14.3 of Numerical Recipes and 
;;; Section 4.5.2 of Astrostatistics (Babu & Feigelson).
;========================================================================
;;; CALLING CONVENTIONS
;;;
;;; The normal parameter "powell_params" is not used by the client -- it exists
;;; because ml_gaussian is written recursively to satisfy the requirements
;;; of IDL's optimization routine POWELL.
;;;
;;; The client should supply a pointer to the univariate data vector in the 
;;; keyword DATA_PTR.
;;;
;;; The client should supply estimates of the model parameters in the
;;; keyword PARAMETERS, which should be a structure of the form:
;;; 
;;; {gauss: replicate({amplitude:0., mean:0., sigma:0., $
;;;		       freeze_amp:0, freeze_mean:0, freeze_sigma:0}, N), $
;;;  poly:  replicate({coeff:0., freeze_coeff:0, exponent:0., $
;;;		       low:0.0, high:1.0}, M)}
;;;
;;; Either of the tags "gauss" or "poly" can be omitted if only polynomial 
;;; or only gaussian components are desired.
;;; The tags "freeze_*"  specify which parameters should be frozen at their
;;; initial value. 
;;;
;;; The model fit to the data vector is of the form:
;;;
;;;  f(x; a1 m1 s1 a2 m2 s2 ... p0 p1 ...) =
;;;     a1*g(u1,s1) + a2*g(u2,s2) + ... + c0*x^p0 + c1*x^p1 + ...
;;;
;;; Polynomial terms are zero outside the specified range [low,high].
;;; 
;;; The parameters are constrained such that the integral of f(x), over the
;;; range of the data vector, equals N, the number of data points.  If we
;;; did not enforce this constraint, then the likelihood would be unbounded.
;;;
;;; The client should supply keywords X0, DELTA_X, & DISTRIBUTION specifying
;;; an empirical distribution function for the dataset.  The model 
;;; distribution function is compared to this using the Kolmogorov-Smirnov 
;;; statistic, yielding a goodness-of-fit metic.
;;;
;;; The updated model parameters are returned in the PARAMETERS structure.
;;; A string array describing the fit results and goodness-of-fit metic
;;; is returned in REPORT.  
;;;
;;; Evaluations of the model at the X-values where the distribution functions
;;; are defined are returned in F_OF_X (suitable for plotting).
;;;
;;; The function value returned is the final likelihood, or ZERO if there
;;; was an error.
;;;
;;; The number of actual free parameters, usually one less than the number
;;; of parameters the user specified as "free" since one amplitude parameter
;;; must be designated as "derived", is returned in NUMFREE. 
;;; 
;-

FUNCTION ml_gaussian, powell_params, $
	 PARAMETERS=pars_kywd, $
	 DATA_PTR=data_ptr_kywd, PROMPT=prompt, $
	 X0=x0, DELTA_X=delta_x, DISTRIBUTION=distribution, $
	 REPORT=report, F_OF_X=f_of_x, NUMFREE=numfree_kywd

COMMON ml_gaussian, pars, data_ptr, num_data, $
		    data_min, data_max, num_gauss, num_poly, numfree, integral

if keyword_set(pars_kywd) then begin
  ;; Is this the call from the client ...
  
  pars       = pars_kywd
  data_ptr   = data_ptr_kywd
  num_data   = n_elements( *data_ptr )  
  data_min   = float(min( *data_ptr, MAX=data_max ))
  data_max   = float(data_max)
  
  dum = where( tag_names(pars) EQ 'GAUSS', count )
  if (count EQ 1 ) then begin
    num_gauss = n_elements( pars.gauss )
  endif else begin
    num_gauss = 0
  endelse 

  dum = where( tag_names(pars) EQ 'POLY', count )
  if (count EQ 1 ) then begin
    num_poly = n_elements( pars.poly )
  endif else begin
    num_poly = 0
  endelse 
  
  
  ;; We are constraining our model so that its integral equals the number
  ;; of datapoints in order to keep the likelihood bounded.
  ;; Thus, the integrals of each of the model terms cannot all be allowed
  ;; to vary independently.  
  ;; We are going to choose one of the model terms whose integral has been
  ;; left free by the caller and make its integral derived, i.e. calculated
  ;; at each fit iteration, but NOT visible to the fit engine.
  ;; We'll mark this derived parameter by setting the corresponding
  ;; freeze_* structure element to the value -1.
  ;; If the user has tried to freeze all the integrals then we'll complain.
  derived_term_found = 0
  for ii = 0, num_gauss-1 do begin
    if (derived_term_found EQ 0 AND pars.gauss[ii].freeze_amp EQ 0) then begin
      derived_term_found        =  1
      pars.gauss[ii].freeze_amp = -1
    endif
  endfor
  
  for ii = 0, num_poly-1 do begin
    if (derived_term_found EQ 0 AND pars.poly[ii].freeze_coeff EQ 0) then begin
      derived_term_found         =  1
      pars.poly[ii].freeze_coeff = -1
    endif
  endfor
  
  if (derived_term_found EQ 0) then begin
    report = ['','---- YOU MUST HAVE AT LEAST ONE COMPONENT WHOSE',$
    		 'AMPLITUDE IS LEFT AS A FREE PARAMETER! ----']
    return, 0
  endif
  
  
  ;; Extract all the free parameters into a vector that can be passed to
  ;; the fitting engine.  For each free parameter, give the powell routine
  ;; some idea of how much to try adjusting the parameter initially.
  powell_params = [0.0]
  powell_gains  = [0.0]
  for ii = 0, num_gauss-1 do begin
    if (pars.gauss[ii].freeze_amp EQ 0) then begin
      powell_params = [powell_params, pars.gauss[ii].amplitude]
      powell_gains  = [powell_gains, (pars.gauss[ii].amplitude * 0.05) > 1E-5]
    endif
     
    if (pars.gauss[ii].freeze_mean EQ 0) then begin
      powell_params = [powell_params, pars.gauss[ii].mean]
      powell_gains  = [powell_gains, (pars.gauss[ii].mean * 0.05) > 1E-5]
    endif
     
    if (pars.gauss[ii].freeze_sigma EQ 0) then begin
      powell_params = [powell_params, pars.gauss[ii].sigma]
      powell_gains  = [powell_gains, (pars.gauss[ii].sigma * 0.05) > 1E-5]
    endif
  endfor
  
  for ii = 0, num_poly-1 do begin
    if (pars.poly[ii].freeze_coeff EQ 0) then begin
      powell_params = [powell_params, pars.poly[ii].coeff]
      powell_gains  = [powell_gains, (pars.poly[ii].coeff * 0.05) > 1E-5]
    endif
  endfor
  
  numfree      = n_elements(powell_params) - 1
  numfree_kywd = numfree
  if (numfree GT 0) then begin  
    powell_params = powell_params[1:*]  
    powell_gains  = powell_gains[1:*]
    num_params    = n_elements(powell_params)
    directions    = reform(identity(num_params), num_params, num_params)
    for ii=0, num_params-1 do directions[ii,ii] = powell_gains[ii]
   ;print, powell_params
   ;print, directions  
  endif

  
  ;; Create the stop button.
  if keyword_set(prompt) then begin
    location = get_screen_size()/2 - [150,0]
    stop_base = widget_base( TITLE='Fitting ...', /COLUMN, $
	                     XOFF=location(0), YOFF=location(1) )
    dum=widget_text(stop_base, VALUE=prompt, $
    		    XSIZE=max(strlen(prompt)), YSIZE=n_elements(prompt))
    stop_button = widget_button( stop_base, VALUE='STOP', XSIZ=300 )
    widget_control, stop_base, /REALIZE, /HOURGLASS
  endif
  

  ;; Perform the fit.
  ;; We should NOT simply let Powell run 1 or 2 iterations each time through our while loop
  ;; because, contrary to my expectations, powell does NOT update "directions" when it
  ;; stops due to ITMAX limit.  Thus, each time powell is called it uses the poor initial
  ;; guess at "directions" rather than retaining what it's learned so far.
  ;; "directions" is updated only when powell converges.
  ;; Thus, the right way to give the user a STOP button is to let powell run to convergence
  ;; in each call, but use an "frac_tolerance" parameter that starts large and decreases.
  done = 0
  frac_tolerance = 1.0
  while (NOT done) do begin
    if (numfree EQ 0) then begin
      ;; We simply want to evaluate the model once, allowing the derived 
      ;; amplitude term to be computed.  Then we'll quit this fitting loop
      ;; and let the K-S be computed and return.
      fmin = ml_gaussian()
      done = 1
    endif else begin
      ;; Establish an error handler for exceptions thrown in fitting routine.
      catch, error_code
      if (error_code EQ 0) then begin
        frac_tolerance = frac_tolerance/100
        powell, powell_params, directions, frac_tolerance, fmin, 'ml_gaussian', /DOUBLE, ITER=iter
;        help, iter

        done = frac_tolerance LE 1.0e-8
      endif else begin
        if keyword_set(prompt) then $
          widget_control, stop_base, /DESTROY, BAD_ID=bad
        report = ['','---- ERROR PERFORMING FIT! ----', !ERROR_STATE.MSG]
        return, 0
      endelse
      catch, /CANCEL
    endelse
  
    report = ''
      
    ;; Evaluate the model function at the X-values where we have a distribution
    ;; function supplied, and format a report for the user.
    xval = x0 + delta_x * findgen( n_elements(distribution) )
    f_of_x = 0.0
    
    for ii = 0, num_gauss-1 do begin
      k1 = pars.gauss[ii].amplitude / (pars.gauss[ii].sigma * sqrt(2*!PI))
      k2 = (xval - pars.gauss[ii].mean)^2.0 / (-2.0 * pars.gauss[ii].sigma^2.0)
      f_of_x = f_of_x + k1 * exp( k2 > (-50.0) ) 
      
      txt=string(pars.gauss[ii].amplitude, $
      		 pars.gauss[ii].mean, pars.gauss[ii].sigma,$
  	f='("Gaussian: area =",G10.4,"; mean =",G10.4,"; sigma =",G10.4)') 
      report = [report, strcompress(txt), '']
    endfor
    
    if (num_poly GT 0) then begin
      txt="Background: "
      for ii = 0, num_poly-1 do begin
        mask = ((pars.poly[ii].low LE xval) AND (xval LE pars.poly[ii].high))
        	
        f_of_x = f_of_x + $
        	 mask * pars.poly[ii].coeff * xval^(pars.poly[ii].exponent)
        
        if (ii GT 0) then txt=txt + ' + '
        txt=txt+string( pars.poly[ii].coeff, pars.poly[ii].exponent, $
        	        f='(G10.4," * X^",F5.2)' )
      endfor
      report = [report, strcompress(txt),'']
    endif

    print & print, report
    print, -fmin, frac_tolerance, f='(%"Log Likelihood = %10.7G (tolerance=%6.0E)")'
    
    if keyword_set(prompt) then begin
      event = widget_event( stop_button, /NOWAIT, BAD_ID=bad )
      widget_control, /HOURGLASS

      if (event.id EQ stop_button) then done=1
    endif

  endwhile
  
  if keyword_set(prompt) then $
    widget_control, stop_base, /DESTROY, BAD_ID=bad
  
  
  ;; Compute the KS statistic = max( |empirical_dist(x) - model_dist(x)| )
  model_dist = total( f_of_x / total( f_of_x, /DOUBLE ), /CUMULATIVE )

  ks = max( abs( model_dist - distribution ) )
  prob_ks, ks, num_data, probks
  
  txt0 = 'From Section 14.3 of Numerical Recipes,'
  txt1 = string(ks, probks, $
  		f='("K-S statistic: ",G10.4,"; significance: ",G10.4)')
  txt2 = string(-fmin,f='("Log Likelihood = ",G10.7)')
  report=[report,txt0,txt1,'',txt2]
  
  ;; Restore the freeze_* structure element corresponding to the term whose
  ;; integral was derived (not free), and return the final parameters.
  for ii = 0, num_poly-1 do begin
    if (pars.poly[ii].freeze_coeff EQ -1) then pars.poly[ii].freeze_coeff = 0
  endfor
  
  for ii = 0, num_gauss-1 do begin
    if (pars.gauss[ii].freeze_amp EQ -1)  then pars.gauss[ii].freeze_amp = 0
  endfor
  pars_kywd = pars
  
  return, fmin

;-------------------------------------------------------------------------
endif else begin
  ;; Or is this the recursive call?

  if (numfree GT 0) then begin  
   ;print, 'powell_params', powell_params
  
    ;; Update the pars structure based on the adjusted powell_params vector.
    index = 0
    for ii = 0, num_gauss-1 do begin
      if (pars.gauss[ii].freeze_amp EQ 0) then begin
        pars.gauss[ii].amplitude = powell_params[index]
        index = index + 1
      endif
  
      if (pars.gauss[ii].freeze_mean EQ 0) then begin
        pars.gauss[ii].mean = powell_params[index]
        index = index + 1
      endif
  
      if (pars.gauss[ii].freeze_sigma EQ 0) then begin
        pars.gauss[ii].sigma = powell_params[index]
        index = index + 1
      endif
    endfor
    
    for ii = 0, num_poly-1 do begin
      if (pars.poly[ii].freeze_coeff EQ 0) then begin
        pars.poly[ii].coeff = powell_params[index]
        index = index + 1
      endif
    endfor
  endif

  
  ;; If we have wandered into a region of parameter space with non-positive
  ;; sigma parameters, then return a very small likelihood.
  if (num_gauss GT 0) then begin
    if (min(pars.gauss.sigma) LE 0) then begin
      print, 'Negative sigma value attempted!'
      ;; The POWELL routine MINIMIZES the metric, so we return a large number
      ;; to discourage it. 
      return,  -alog(1E-30) * num_data
    endif
  endif
  
  
  ;; We are constraining our model so that its integral is equal to the
  ;; number of datapoints, num_data.  One of the model terms has a derived
  ;; parameter that allows us to adjust the integral of that term so that
  ;; the integral of the whole model comes out right.
  ;; Here we compute the integral of the model over the data range,
  ;; separating out the contribution of the derived term.  
  ;; Note that we may have to make the derived term have a negative integral.
  int_derived_term = 0.0
  area_needed      = float(num_data)
  
  for ii = 0, num_poly-1 do begin
    exponent = pars.poly[ii].exponent
    low      = pars.poly[ii].low  > data_min
    high     = pars.poly[ii].high < data_max
    
    int_this_term =  (high^(exponent+1) - low^(exponent+1)) / (exponent+1)

    if (pars.poly[ii].freeze_coeff EQ -1) then begin
      int_derived_term = int_this_term
    endif else begin
      area_needed = area_needed - pars.poly[ii].coeff * int_this_term
    endelse
  endfor
  
  for ii = 0, num_gauss-1 do begin
    k1 = gaussint( (data_max - pars.gauss[ii].mean) / pars.gauss[ii].sigma )
    k2 = gaussint( (data_min - pars.gauss[ii].mean) / pars.gauss[ii].sigma )
    int_this_term = (k1 - k2) 

    if (pars.gauss[ii].freeze_amp EQ -1) then begin
      int_derived_term = int_this_term
    endif else begin
      area_needed = area_needed - pars.gauss[ii].amplitude * int_this_term
    endelse
  endfor

  if (int_derived_term EQ 0) then begin
    print, 'Cannot adjust area of model!'
    ;; The POWELL routine MINIMIZES the metric, so we return a large number
    ;; to discourage it. 
    return,  -alog(1E-30) * num_data
  endif

  ;; Now, we have to adjust the coefficient on the derived term to get the
  ;; right integral for the entire model. 
  for ii = 0, num_poly-1 do begin
    if (pars.poly[ii].freeze_coeff EQ -1) then $
      pars.poly[ii].coeff = area_needed / int_derived_term 
  endfor
  
  for ii = 0, num_gauss-1 do begin
    if (pars.gauss[ii].freeze_amp EQ -1) then $
      pars.gauss[ii].amplitude = area_needed / int_derived_term
  endfor
    
  
  ;; Finally, evaluate the model at each of the datapoints.
  f_of_x = 0.0
  for ii = 0, num_poly-1 do begin
    mask=((pars.poly[ii].low LE *data_ptr) AND (*data_ptr LE pars.poly[ii].high))

    f_of_x = f_of_x + $
    	     mask * pars.poly[ii].coeff * (*data_ptr)^(pars.poly[ii].exponent)
  endfor
  
  for ii = 0, num_gauss-1 do begin
    k1 = pars.gauss[ii].amplitude / (pars.gauss[ii].sigma * sqrt(2*!PI))
    k2 = (*data_ptr - pars.gauss[ii].mean)^2.0 / (-2.0*pars.gauss[ii].sigma^2.0)
    f_of_x = f_of_x + k1 * exp( k2 > (-50.0) ) 
  endfor
  

  ;; Compute the likelihood.  The model is a probability density function, and
  ;; should thus be positive at all datapoints.  If we get a non-positive value
  ;; we are in a forbidden area of parameter space, and we want to set 
  ;; the likelihood to a very small value.
  if (min( f_of_x ) LE 1E-30) then begin
    print, 'Negative model value encountered!'
    likelihood = num_data * alog(1E-30)
  endif else begin
    likelihood = total( alog( f_of_x ), /DOUBLE ) 
  endelse
;print
;print, 'likelihood', likelihood
  
  ;; The POWELL routine MINIMIZES the metric, so we return the negative 
  ;; likelihood.
  return, - likelihood 
endelse

END
