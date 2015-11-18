;; $Id: poisson_distribution.pro 2700 2007-03-22 20:12:21Z patb $
;; Patrick Broos

;; Return the cumulative probability that a Poisson random variable of the  
;; specified mean will take on a value <= N (an integer).

FUNCTION poisson_distribution, poisson_mean, N, abort_limit

if (N LT 0) then return, 0.

;; For means bigger than 100, use Gaussian approximation since the exp()
;; in the direct Poisson calculation underflows.
if (poisson_mean GE 100) then begin
  z = (N - poisson_mean) / sqrt(poisson_mean)
  return, 0. > gauss_pdf(z) < 1.

endif else begin
  if NOT keyword_set(abort_limit) then abort_limit=1.0

  prob            = exp(-double(poisson_mean))
  cumulative_prob = prob
  for pix_val = 1.0,N do begin
    prob            = (poisson_mean/pix_val) * prob
    cumulative_prob = cumulative_prob + prob
    
    ;; If the cumulative probability gets high enough, don't bother to continue.
    if (cumulative_prob GT abort_limit) then break
  endfor  

  return, 0. > cumulative_prob < 1.
endelse
end
