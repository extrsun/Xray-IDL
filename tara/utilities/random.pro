;+
;============================================================================
;;;
;;; FILE NAME:    @(#)random.pro	1.3
;;;
;;; DESCRIPTION:  This routine hides the caller from the inelegant and 
;;;		  ever-changing interface of RSI's RANDOMU & RANDOMN routines.
;;;		  Keywords are passed on to RANDOMU via the _extra mechanism.
;;;		  For example, to get normal deviates use random(/NORMAL).
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Copyright (C) 1999, Pennsylvania State University
;;;
;============================================================================
;-

FUNCTION random, n1, n2, n3, NEW_SEED=new_seed, _EXTRA=extra

COMMON random_seed, seed

if (n_elements(seed) EQ 0) then begin
  ; Generate a seed using the 5 digits to the right of the decimal in the current time.
  seed = floor((systime(1) mod 1)*1E5)
  ;print, seed, F='("Random seed chosen from system time: ",I0)'
endif

if keyword_set(new_seed) then begin
  seed = long(new_seed)
  ;print, seed, F='("Random seed set to: ",I0)'
endif

case n_params() of
     0: return, randomu(seed,          _EXTRA=extra)
     1: return, randomu(seed,n1,       _EXTRA=extra)
     2: return, randomu(seed,n1,n2,    _EXTRA=extra)
     3: return, randomu(seed,n1,n2,n3, _EXTRA=extra)
endcase

end



