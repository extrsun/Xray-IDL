function poisson_lolim,n_in,sigma_in,g86=g86

;---------------------------------------------------------------------
; approximates true lower limit (based on Poisson statistics) to an
; observed integer number of events, "n_in", at a given confidence level
; corresponding to "sigma_in" Gaussian standard deviations.
;
; tested to be accurate to better than 1% for n<100, sigma<5
;
; author: H. Ebeling, Institute for Astronomy, University of Hawaii
;
; based on and inspired by Gehrels, 1986, ApJ, 303, 336
; 
; Note: the 7 decimal digit accuracy for all coefficients is critical!
;
;*************************************************************************
;
; If you found this code useful in work leading to scientific publications
; please consider including a reference to "Ebeling H. 2003, MNRAS, 340, 1269"
; 
;*************************************************************************
;---------------------------------------------------------------------

if n_params() lt 2 then begin
   print,' syntax: result = poisson_lolim(n,S)'
   print,'         where'
   print,'         o "n" is the observed (integer) number of counts and "S" is'
   print,'           the number of equivalent Gaussian standard deviations'
   print,'         o "result" is the Poissonian lower limit'
   print,''
   print,' (reference: Ebeling H. 2003, MNRAS, 340, 1269)'
   print,''
   return,-1
endif

nn = n_elements(n_in)
ns = n_elements(sigma_in)

if ns gt 1 and nn gt 1 and nn ne ns then begin
   print,' n and sigma have incompatible dimensions'
   return,replicate(-1,ns>nn)
endif

; turn input into double precision fp:

n = 1.d0*n_in
sigma = 1.d0*sigma_in

; ensure both vectors are of equal length:

if ns gt nn then n = replicate(n,ns)
if nn gt ns then sigma = replicate(sigma,nn)

num = n_elements(n)

sigma_b1 = 0.938756d0  ; singularity of gamma
sigma_b2 = 3.0         ; two fits to beta(s) meet
sigma_b3 = 2.7         ; two fits to gamma(s) meet

beta = sigma-sigma
gamma = beta
delta = beta

; coefficients of polynomial fit to beta(sigma):

b_c1 = [-3.8605809d-03,$
	-6.6002964d-03,$
	 6.5798149d-03,$
	 2.8172041d-03,$
	 2.9892915d-03,$
	-5.4387574d-04]

b_c2 = [ 3.4867327e-01,$
        -4.0996949e-01,$
         1.6514495e-01,$
        -1.5783156e-02,$
         5.2768918e-04]

nbc1 = n_elements(b_c1)
nbc2 = n_elements(b_c2)

; compute beta:

for j=0d0,nbc1-1 do beta = beta+b_c1[j]*sigma^j
i = where(sigma ge sigma_b2,ct)
if ct gt 0 then begin
   beta[i] = 0.d0
   for j=0d0,nbc2-1 do beta[i] = beta[i]+b_c2[j]*sigma[i]^j
endif

; coefficients of piecewise polynomial fit to gamma(1/sigma)
; or gamma(log(sigma)):

g_c1 = [ -1.7174713e+00,$
         -1.7015942e+00,$
         -1.9059468e+00,$
       	 -3.1324250e+00,$
       	 -2.0145052e+00,$
       	 -4.2578095e-01]

g_c2 = [-1.0131243d+00,$
        -2.9319339d+00,$
         3.2459998d+00,$
        -2.1348935d+00,$
         6.6769015d-01,$
        -8.3404124d-02]

g_c3 = [-2.8115538d+00,$
         3.5117552d-01,$
        -1.3215426d-02]

; use G86, Eq14 if /g86 is set (for testing and debugging purposes only)

if keyword_set(g86) then begin

   g_c1 = [-1.7480435d+00,$
           -1.8895824d+00,$
       	   -3.0808786d+00,$
       	   -5.5164953d+00,$
       	   -3.9940504d+00,$
       	   -1.0248451d+00]

   g_c2 = [-6.3473512d-01,$
       	   -4.6707845d+00,$
       	    6.1602866d+00,$
       	   -4.3543401d+00,$
       	    1.4470675d+00,$
       	   -1.8708963d-01]

   g_c3 = [-2.7517416d+00,$
            3.1692400d-01,$
           -8.7788310d-03]

endif

ngc1 = n_elements(g_c1)
ngc2 = n_elements(g_c2)
ngc3 = n_elements(g_c3)

; compute gamma:

i = where(sigma lt sigma_b1,ct)
if ct gt 0 then begin
   for j=0d0,ngc1-1 do gamma[i] = gamma[i]+g_c1[j]*alog10(sigma_b1-sigma[i])^j
endif
i = where(sigma gt 1.1 and sigma lt sigma_b3,ct)
if ct gt 0 then begin
   for j=0d0,ngc2-1 do gamma[i] = gamma[i]+g_c2[j]*1d0/(sigma[i]-sigma_b1)^j
endif
i = where(sigma ge sigma_b3,ct)
if ct gt 0 then begin
   for j=0d0,ngc3-1 do gamma[i] = gamma[i]+g_c3[j]*sigma[i]^j
endif

gamma = -5d1>gamma<0

; coefficients of polynomial fit to delta(sigma):

d_c = [-2.2906640d-02,$
       	6.8209168d-02,$
       -9.1678422d-02,$
       	7.1533924d-02,$
       -3.5010270d-02,$
       	1.0928872d-02,$
       -2.1069241d-03,$
       	2.2638722d-04,$
       -1.0302360d-05]

ndc = n_elements(d_c)

; compute delta:

for i=0d0,ndc-1 do delta = delta+d_c[i]*sigma^i

i = where(sigma lt 1.2,ct)
if ct gt 0 then delta[i] = 0.

if keyword_set(g86) then delta = delta-delta

result = (n-n)*sigma
i = where(n gt 0,ct)

if ct gt 0 then begin
   result[i] = n[i]*(1d0-1d0/(9d0*n[i])-sigma[i]/(3d0*sqrt(n[i]))+beta[i]*$
               n[i]^gamma[i]+delta[i]*sin(5d0/(n[i]+0.25d0)*90*!pi/180))^3d0
endif

return,result

end