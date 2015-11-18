;;; $Id: poisson_consistent.pro 1468 2001-11-25 10:16:20Z patb $
;;; Test whether an integer data value is consistent with a specified
;;; Poisson random variable.
;;; We compute the cumulative probability that a Poisson random variable of the  
;;; specified mean will take on a value <= data_value (an integer) and 
;;; then see if that probability lies in the "main part" of the
;;; distribution or out in the upper or lower tail.
;;; The parameter prob_false_negative is the area one wants to consider to
;;; be in the tails, i.e. the probability that a true sample from the
;;; specified distribution would (falsely) be labeled as "not consistent".

PRO poisson_consistent, this, $
		CREATE=create, PROB_FALSE_NEGATIVE=prob_false_negative, $
		MAX_COUNTS=max_counts, $

		poisson_means, data_values, is_consistent, $

		DESTROY=destroy

if keyword_set(create) then begin
  ;; For each data_value from 0 to max_counts we're going to keep track of the
  ;; range of Poisson distributions (means) that are consistent with that
  ;; data value.

  ;; For each data_value we maintain lower and upper limits on the unknown
  ;; lowest Poisson mean value that is consistent with that data_value, and
  ;; lower and upper limits on the unknown highest Poisson mean value that is
  ;; consistent with that data_value.
  ;; Thus the Poisson mean axis is divided into 5 intervals:
  ;;                         mean <  min_mean_lowlimit  : NOT consistent
  ;;   min_mean_lowlimit  <  mean <  min_mean_highlimit : unknown
  ;;   min_mean_highlimit <= mean <= max_mean_lowlimit  : consistent
  ;;   max_mean_lowlimit  <  mean <  max_mean_highlimit : unknown 
  ;;   max_mean_highlimit <  mean                       : NOT consistent

  ;; For small Poisson means one must take special care to avoid defining
  ;; degenerate sets of consistent data values.  For example, for a mean
  ;; of 0.01 the data_value 1 has a cumulative probablility very close
  ;; to 1.0, and indeed the data_value 0 also has a high cumulative
  ;; probability which might itself already exceed the upper_tail
  ;; threshold.  
  ;; It would be irrational to conclude that the two data values just
  ;; below and just above the mean are not consistent with the distribution.
  ;; Thus, below we predefine the "consistent" range for each mean to
  ;; include the integer data values just below and above.

  min_mean_lowlimit  = ptr_new(replicate(  0.0,max_counts+1))
  min_mean_highlimit = ptr_new( (findgen(      max_counts+1)-1) > 0)

  max_mean_lowlimit  = ptr_new(  findgen(      max_counts+1)+1)
  max_mean_highlimit = ptr_new(replicate(+1E10,max_counts+1))

  ;; We choose to define the data_value "1" as consistent with all means
  ;; in [0,1].
  (*min_mean_highlimit)[1] = 0

  this = {max_counts:max_counts, $
	  lower_tail:      prob_false_negative/2.0, $
	  upper_tail: 1 - (prob_false_negative/2.0), $
	  updated   : ptr_new(bytarr(max_counts+1)), $

	  min_mean_lowlimit :min_mean_lowlimit, $
	  min_mean_highlimit:min_mean_highlimit, $

	  max_mean_lowlimit :max_mean_lowlimit, $
	  max_mean_highlimit: max_mean_highlimit}
  return
endif

if keyword_set(destroy) then begin
  print, F='(%"\nRanges of Poisson mean where consistency unknown:")'
  for ii=0,this.max_counts do $
    if ((*this.updated)[ii]) then $ 
      print, ii, (*this.min_mean_lowlimit)[ii], (*this.min_mean_highlimit)[ii], $
		 (*this.max_mean_lowlimit)[ii], (*this.max_mean_highlimit)[ii], $
      		 F='(%"data_value=%3d: %10.3g %10.3g;  %10.3g %10.3g")'
  ptr_free, this.min_mean_lowlimit, this.min_mean_highlimit, $
	    this.max_mean_lowlimit, this.max_mean_highlimit
  return
endif

;; ---------------------------------------------------------------------------
;; Find the data values whose consistency is not known, perform the
;; calculation, and adjust the tables.
s = where(((*this.min_mean_lowlimit) [data_values] LT poisson_means AND $
	   (*this.min_mean_highlimit)[data_values] GT poisson_means) OR $

	  ((*this.max_mean_lowlimit) [data_values] LT poisson_means AND $
	   (*this.max_mean_highlimit)[data_values] GT poisson_means), count)

for ii =0,count-1 do begin
  poisson_mean = poisson_means[ s[ii] ]
  data_value   = data_values  [ s[ii] ]
  cumulative_prob = poisson_distribution( poisson_mean, $
        				  data_value, $
        				  this.upper_tail )

  if ((this.lower_tail LE cumulative_prob ) AND $
      (this.upper_tail GE cumulative_prob )) then begin
    ; The data_value is consistent with the distribution, so we must
    ; adjust either min_mean_highlimit or max_mean_lowlimit.
    if ((*this.min_mean_highlimit)[data_value] GT poisson_mean) then begin
      (*this.min_mean_highlimit)[data_value] = poisson_mean
    endif else begin
      (*this.max_mean_lowlimit) [data_value] = poisson_mean
    endelse

  endif else begin
    ; The data_value is NOT consistent with the distribution, so we must
    ; adjust either min_mean_lowlimit or max_mean_highlimit.
    if ((*this.min_mean_highlimit)[data_value] GT poisson_mean) then begin
      (*this.min_mean_lowlimit) [data_value] = poisson_mean
    endif else begin
      (*this.max_mean_highlimit)[data_value] = poisson_mean
    endelse

  endelse

  (*this.updated)[data_value] = 1B
endfor


;; Finally, now that the tables are updated, for each data_value  we can 
;; just check the poisson_mean against the range of means
;; known to be consistent.
;;   min_mean_highlimit <= mean <= max_mean_lowlimit  : consistent

is_consistent = (((*this.min_mean_highlimit)[data_values] LE poisson_means) AND $
		 ((*this.max_mean_lowlimit) [data_values] GE poisson_means)) 
return
end
