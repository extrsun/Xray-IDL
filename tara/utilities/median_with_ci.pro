;;; $Id: median_with_ci.pro 2656 2007-01-05 18:38:31Z patb $ 
;;; Patrick Broos, 2007
;;; The routine computes the sample median, equivalent to median(data, /EVEN), 
;;; and estimates a confidence interval on that median.

;;; The data are assumed to represent the parent population, as in all bootstrap resampling approaches.
;;; In the case of the median however, random resampling is not needed because the 
;;; distribution of the sample median can be derived in closed form

; These references discuss the binomial formulation of the confidence interval for the sample median.
; Section 1.1.2 of Introduction to Modern Nonparametric Statistics, James J. Higgins, 2004
; Lehmann (Nonparametrics: Statistical Methods Based on Ranks, Holden-Day, 1975, p. 182-183)
; Conover, W.J. (1980) Practical Nonparametric Statistics John Wiley and Sons, New York. 
; http://www.stat.berkeley.edu/~stark/SticiGui/Text/ch18.htm#solution_7

PRO median_with_ci, data, CONFIDENCE_LEVEL=confidence_level, median_value, limit_lower, limit_upper, actual_confidence_level

; The default confidence interval has a probabililty equal to a 1-sigma interval of a Gaussian.
if NOT keyword_set(confidence_level) then confidence_level = 0.6827

; Sort the data.
data = data[sort(data)]
N    = n_elements(data)

N_is_odd = (N mod 2 EQ 1)

p_to_the_N = (0.5D)^N

; Order statistics range 1..N.
if N_is_odd then begin
  median_value = data[N/2]
endif else begin
  median_value = mean(data[N/2 - 1 : N/2])
endelse


; Remember that order stats are 1-based, array indexing is 0-based!

if (N LT 3) then begin
  limit_lower             = !VALUES.f_nan
  limit_upper             = !VALUES.f_nan
  actual_confidence_level = !VALUES.f_nan
  return
endif   

if (N GT 1000) then begin   
  ; For large N the binomial coefficient will overflow so we integrate the Gaussian approximation to the binomial distribution.
  alpha = 1-confidence_level
  z_val = gauss_cvf(alpha/2.)
; help, z_val
  
  integral_lower_limit = round( (N - z_val*sqrt(N))/2.0 )
  integral_upper_limit = round( (N + z_val*sqrt(N))/2.0 )     
  actual_confidence_level = confidence_level  
  GOTO, FINISH
endif

; Our goal is to find the shortest confidence interval (definite integral of our binomial distribution).  
; The length of our CI is taken as the difference in event energies, NOT the difference in order statistics.

; The integration idea below comes from Kraft, Burrows, & Nousek (1991).

; Our convention is that integral_lower_limit, integral_upper_limit are the 
; current limits of our integration of the binomial distribution. 
; Remember that order stats are 1-based, 1...N; array indexing is 0-based!

; Precompute the binomial coefficients for k=0...N using a convenient recurrance relation.
bc = dblarr(N+1)
bc[0] = 1
for kk=1,N do bc[kk] = bc[kk-1] * (double(N-kk+1)/kk)
;print, bc


; Initialize the loop.  The initial integral is a single panel at the peak of the binomial.
score_left  = 0
score_right = 0

integral_lower_limit = long(N/2)
integral_upper_limit = integral_lower_limit
area = bc[integral_lower_limit] * p_to_the_N
;help, bc[integral_lower_limit]

repeat begin
  if (score_left EQ 0) AND (integral_lower_limit GT 1) then begin
    ; Evaluate the value of the binomial distribution at the next step to the left.
    step_area_left = bc[integral_lower_limit - 1] * p_to_the_N
    
    ; Remember that order stats are 1-based, array indexing is 0-based!
    index_lower      = integral_lower_limit-1
    step_length_left = data[index_lower] - data[index_lower-1]
    score_left       = step_area_left/step_length_left
  endif

  if (score_right EQ 0) AND (integral_upper_limit LT N) then begin
    ; Evaluate the value of the binomial distribution at the next step to the right.
    step_area_right = bc[integral_upper_limit + 1] * p_to_the_N
    
    ; Remember that order stats are 1-based, array indexing is 0-based!
    index_upper       = integral_upper_limit-1
    step_length_right = data[index_upper+1] - data[index_upper]
    score_right       = step_area_right/step_length_right
  endif                  
  
  ; Choose which step will keep the confidence interval most compact.
  if (score_left GT score_right) then begin
    ; We accept the left step.
    score_left       = 0
    area             = area + step_area_left
    integral_lower_limit = integral_lower_limit - 1
  endif else begin
    ; We accept the right step.
    score_right      = 0
    area             = area + step_area_right
    integral_upper_limit = integral_upper_limit + 1
  endelse
endrep until (area GE confidence_level) OR ((score_left EQ 0) AND (score_right EQ 0))
actual_confidence_level = area

FINISH:
;help, integral_lower_limit, integral_upper_limit+1

; The method shown in Higgins sets the lower confidence limit at the data value whose 
; order statistic is equal to the lower integration limit of the binomial distribution (integral_lower_limit).
; Strangely, the method sets the upper confidence limit at the data value whose  
; order statistic is one higher than the upper integration limit 
; (accounting for the "integral_upper_limit+1" below).

; The "-1" in the indexing below is because order statistics are 1-based while IDL array indexing is 0-based!
limit_lower = data[ integral_lower_limit  -1]
limit_upper = data[(integral_upper_limit+1-1) < (N-1)]
return
end


; Test driver to measure coverage of our confidence intervals.
; test,  10,10000,ll,ul  &   print, total((ll LE 0) AND (ul GE 0),/DOUBLE)/n_elements(ll)
; test, 100,10000,ll,ul  &   print, total((ll LE 0) AND (ul GE 0),/DOUBLE)/n_elements(ll)
; test,1000,10000,ll,ul  &   print, total((ll LE 0) AND (ul GE 0),/DOUBLE)/n_elements(ll)
; test,1001,10000,ll,ul  &   print, total((ll LE 0) AND (ul GE 0),/DOUBLE)/n_elements(ll)
PRO test, N, M, ll, ul, NORMAL=normal

ll = fltarr(M)
ul = fltarr(M)

for ii=0L,M-1 do begin
  if keyword_set(normal) then data = random(N, /NORM) $
                         else data = random(N) - 0.5
  median_with_ci, data, mv, llimit, ulimit, actual_confidence_level, CONF=0.9
  ll[ii] = llimit
  ul[ii] = ulimit
endfor
help, actual_confidence_level
return
end



