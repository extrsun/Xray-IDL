;========================================================================
;;;
;;; Integration Routine: $Id: int_tabulated_psu.pro 455 1998-02-16 16:28:40Z patb $
;;;
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool extends the capabilities of the IDL intrinsic INT_TABULATED,
;;; allowing the specification of the limits of integration.
;;;
;;; The definite integral 	 - high_limit
;;;				|
;;;				| f(x) dx
;;;				|
;;;		     low_limit -
;;;
;;; is estimated.  If the integration limits extend beyond the tabulated
;;; range of the function, the function is linearly extrapolated.

;;; The absicissae values (x) for the function MUST be monotonically 
;;; increasing.

FUNCTION int_tabulated_psu, x, f, low_limit, high_limit

;; Evaluate function at limits of integration.
f_limits = interpol( f, x, [low_limit, high_limit] )


;; Find the subset of the tablulated values that lies within the limits.
index = where( x GT low_limit AND x LT high_limit, count )

if (count GT 0) then begin
  x_vals = [low_limit,   x[index], high_limit]
  f_vals = [f_limits[0], f[index], f_limits[1]]
endif else begin
  x_vals = [low_limit, high_limit]
  f_vals = f_limits
endelse

return, int_tabulated( x_vals, f_vals )
end
