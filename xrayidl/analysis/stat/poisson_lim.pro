pro poisson_lim,n,sigma,lo_lim,up_lim

;---------------------------------------------------------------------
; approximates true lower and upper limit (based on Poisson statistics)
; to an observed integer number of events, "n", at a given confidence level
; corresponding to "sigma" Gaussian standard deviations.
;
; tested to be accurate to better than 1% for n<100, sigma<5 (lower limit)
;                   and to better than 0.2% for n<100, sigma<5 (upper limit)
;                   and to better than 0.5% for n<100, sigma<7 (upper limit)
;
; author: H. Ebeling, Institute for Astronomy, University of Hawaii
;
; based on and inspired by Gehrels, 1986, ApJ, 303, 336
; 
;*************************************************************************
;
; If you found this code useful in work leading to scientific publications
; please consider including a reference to "Ebeling H. 2003, MNRAS, 340, 1269"
; 
;*************************************************************************
;---------------------------------------------------------------------

if n_params() lt 2 then begin
   print,' syntax: poisson_lim,n,S,lo_lim,up_lim'
   print,'         where'
   print,'         o "n" is the observed (integer) number of counts and "S" is'
   print,'           the number of equivalent Gaussian standard deviations'
   print,'         o "lo_lim" is the (approximate) Poissonian lower limit'
   print,'         o "up_lim" is the (approximate) Poissonian upper limit'
   print,''
   print,' (reference: Ebeling H. 2003, MNRAS, 340, 1269)'
   print,''
   return
endif

lo_lim = poisson_lolim(n,sigma)
up_lim = poisson_uplim(n,sigma)

return

end

