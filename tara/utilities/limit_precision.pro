;+
;========================================================================
;;;
;;; FILE NAME:    $Id: limit_precision.pro 3591 2010-01-05 15:34:18Z psb6 $
;;;
;;; DESCRIPTION:  This routine converts a real number to a string with the
;;;               specified number of significant digits.  
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;-
;========================================================================
FUNCTION limit_precision, value, sig_digits

if (value EQ 0) then return, '0.0'

if finite(value, /NAN) then return, 'NaN'

if finite(value, /INFINITY, SIGN=-1) then return, '-Inf'
if finite(value, /INFINITY, SIGN= 1) then return, '+Inf'

;; Calculate an increment to add or subtract to value.
log_value = alog10( abs(value) )

increment = 10.0D^(ceil( log_value ) - sig_digits)


;; Determine whether we want to add to value (round UP) or
;; subtract from value (round DOWN) in order to achieve the
;; result that ROUND() would do if the significant digits
;; were shifted to the integer part of the number.
shifted_value = value/increment
round_up = (shifted_value LT round(shifted_value))

;; Adjust the value by increment until the desired condition is met.
fmt = string( sig_digits, f='("(G20.",I0,")")' )

done  = 0
steps = 0
while (NOT done) do begin
  if (round_up) then begin
    rounded_string = string( value + steps*increment, f=fmt )
    done = (value LE float( rounded_string ))
  endif else begin
    rounded_string = string( value - steps*increment, f=fmt )
    done = (value GE float( rounded_string ))
  endelse

  steps = steps + 1
  if (steps GE 100) then message, 'Loop failed to converge.'
endwhile

return, strcompress( rounded_string, /REMOVE_ALL )
end


PRO test_limit_precision

base = random()
for i=-10,10 do begin
  val = base * 10.^i
  round = limit_precision(val,5,/R)
  round = limit_precision(val,5)
  print, round, val - float(round)
endfor
return
end
