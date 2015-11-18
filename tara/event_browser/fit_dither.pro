;;; $Id: fit_dither.pro 889 1999-09-27 16:20:06Z patb $
;;; A = [amplitude, period, null, offset]

PRO fit_dither, X, A, F, XPOSITION=xposition, YPOSITION=yposition

if (keyword_set(xposition) or keyword_set(yposition)) then begin

  if (0 EQ GetProperty( 'exposure', WDS_DATA=exposure )) then $
      message, 'exposure not defined'
      
  if (keyword_set(xposition)) then begin
    if (0 EQ GetProperty( 'TDETX', WDS_DATA=data )) then $
      message, 'TDETX not defined'
      
    param_names = ['x_dither_amplitude', 'x_dither_period', 'x_dither_null']
  endif else begin
    if (0 EQ GetProperty( 'TDETY', WDS_DATA=data )) then $
      message, 'TDETY not defined'
      
    param_names = ['y_dither_amplitude', 'y_dither_period', 'y_dither_null']
  endelse
  
  ;; Estimate the parameters.
  offset    = median(data)
  amplitude = stddev(data) / 0.7
  read, 'Estimate the dither period (exposures):', period
  read, 'Estimate any zero-crossing (exposure number):', null
  
  ;; Do the fit.
  params  = double([amplitude,period,null,offset])
  data    = double(data)
  exposure=double(exposure)
  n_data  = n_elements(data)
  print, 'Fitting with ', n_data, ' data points.'
  residuals=data - curvefit( exposure, data, replicate(1D, n_data), $
  			     params, FUNCTION_NAME='fit_dither', /NODERIVATIVE )
  
  print, 'First fit results: ', params
  
  ;; Sigma clip the data and fit again.  
  index = where( abs(residuals-mean( residuals,/DOUBLE )) LT $
  		 2*stddev(residuals,/DOUBLE) )
  
  data    = data[index]
  exposure=exposure[index]
  n_data  = n_elements(data)
  print, 'Sigma clipping and then fitting with ', n_data, ' data points.'
  residuals=data - curvefit( exposure, data, replicate(1D, n_data), $
  			     params, FUNCTION_NAME='fit_dither', /NODERIVATIVE )
  print, 'Final fit results: ', params
   
  ;; Save the parameters.
  for ii = 0, 2 do begin
    if (0 EQ GetParameter( param_names[ii], param_ptr )) then $
      message, param_names[ii] + ' not defined'
      
    *(*param_ptr).data      = params[ii]
    (*param_ptr).data_epoch = GetEpoch( /INCREMENT )
  endfor
endif else begin
  F = A[3] + (A[0] * sin( 2 * !PI * (X - A[2]) / A[1] ))
endelse

return
end

