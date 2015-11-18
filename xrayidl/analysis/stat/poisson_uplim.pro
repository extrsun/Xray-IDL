function poisson_uplim,n_in,sigma_in

;---------------------------------------------------------------------
; approximates true upper limit (based on Poisson statistics) to
; an observed integer number of events, n, at a given confidence level
; corresponding to sigma Gaussian standard deviations.
;
; tested to be accurate to better than 0.2% for n<100, sigma<5
; tested to be accurate to better than 0.5% for n<100, sigma<7
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
   print,' syntax: result = poisson_uplim(n,S)'
   print,'         where'
   print,'         o "n" is the observed (integer) number of counts and "S" is'
   print,'           the number of equivalent Gaussian standard deviations'
   print,'         o "result" is the Poissonian upper limit'
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

sigma_b1 = 0.50688d0  ; first singularity of gamma
sigma_b2 = 1.2d0      ; break between 2nd and 3rd fit to gamma(sigma)
sigma_b3 = 2.27532d0  ; second singularity of gamma

beta = sigma-sigma
gamma = beta

; coefficients of polynomial fit to beta(sigma):

;b_c = [-3.8330d-03,+5.6242d-03,+7.1103d-03,-7.7747d-03,$
;       +2.9158d-03,-7.4817d-04,+1.1179d-04,-7.0317d-06]
b_c = [-3.8954d-03,+6.2328d-03,+5.2345d-03,-5.3096d-03,+1.3093d-03,$
       -2.0344d-04,+2.0393d-05,-1.1974d-06,+3.1161d-08]
nbc = n_elements(b_c)

; compute beta:

for i=0d0,nbc-1 do beta = beta+b_c[i]*sigma^i

; coefficients of piecewise polynomial fit to gamma(sigma) or gamma(1/sigma)
; or gamma(log(sigma)):

g_c1 = [-2.0799d+00,-7.1925d-01,-4.0064d-01,-7.3386d-02,-5.4791d-03]

g_c2 = [-1.4354d+00,-6.3188d-01,-1.6177d-01,-5.6966d-01,-2.2835d-01]

g_c3 = [-8.4098d-01,+6.8766d-01,+2.0358d-01,+3.9965d-02]

g_c4 = [-1.0120d+00,-2.8853d-01,+4.2013d-01,-5.3310d-02,$
        -1.6319d-02,+4.8667d-02,-5.5299d-02,-3.3361d-02]

ngc1 = n_elements(g_c1)
ngc2 = n_elements(g_c2)
ngc3 = n_elements(g_c3)
ngc4 = n_elements(g_c4)

; compute gamma:

i = where(sigma lt sigma_b1,ct)
if ct gt 0 then begin
   for j=0d0,ngc1-1 do gamma[i] = gamma[i]+g_c1[j]*1d0/(sigma[i]-sigma_b1)^j
endif
i = where(sigma ge sigma_b1 and sigma lt sigma_b2,ct)
if ct gt 0 then begin
   for j=0d0,ngc2-1 do gamma[i] = gamma[i]+g_c2[j]*alog10(sigma[i]-sigma_b1)^j
endif
i = where(sigma ge sigma_b2 and sigma lt sigma_b3,ct)
if ct gt 0 then begin
   for j=0d0,ngc3-1 do gamma[i] = gamma[i]+g_c3[j]*1d0/(sigma[i]-sigma_b3)^j
endif
i = where(sigma ge sigma_b3,ct)
if ct gt 0 then begin
   for j=0d0,ngc4-1 do gamma[i] = gamma[i]+g_c4[j]*alog10(sigma[i]-sigma_b3)^j
endif

gamma = (-10)>gamma<0

return,(n+1d0)*(1d0-1d0/(9*(n+1.d0))$
       +sigma/(3*sqrt(n+1d0))+beta*(n+1.d0)^gamma)^3d0

end